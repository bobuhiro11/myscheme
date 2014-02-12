;;; Chap 4.4 Stack Base Scheme Interpreter

;; (rec sum (lambda (x) (if (= x 0) 0 (+ x (sum (- x 1))))))
(define-syntax rec (syntax-rules ()
                                 ((_ a b) (let ((a '()))
                                            (set! a b)))))

;; (recur count ((x '(a b c d e))) (if (null? x) 0 (+ (count (cdr x)) 1)))
(define-syntax recur
  (syntax-rules ()
                ((_ f ((v i) ...) e ...)
                 ((rec f (lambda (v ...) e ...))
                  i ...))))

;; (record (x y z) '(1 2 3) (+ x y z))
(define-syntax record
  (syntax-rules ()
                ((_ (var ...) val exp ...)
                 (apply (lambda (var ...) exp ...) val))))

;; (record-case '(hello 1 2 3)
;;              (hello (x y z)
;;                     (+ x y z))
;;              (bye   (x y z)
;;                     (- x y z)))
(define-syntax record-case
  (syntax-rules (else)
                ((_ exp1 (else exp3 ...))
                 (begin exp3 ...))
                ((_ exp1 (key vars exp2 ...))
                 (if (eq? (car exp1) 'key) (record vars (cdr exp1) exp2 ...)))
                ((_ exp1 (key vars exp2 ...) c ...)
                 (if (eq? (car exp1) 'key)
                   (record vars (cdr exp1) exp2 ...)
                   (record-case exp1 c ...)))))

;; (extend
;;   '((a b c) (u v))
;;   '(x y z))
;;
;; => ((x y z) (a b c) (u v))
(define extend
  (lambda (env rib)
    (cons rib env)))

(define closure
  (lambda (body n s)
    (let ((v (make-vector (+ n 1))))
      (vector-set! v 0 body)
      (recur f ((i 0))
             (unless (= i n)
               (vector-set! v (+ i 1) (index s i))
               (f (+ i 1))))
      v)))

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

(define continuation
  (lambda (s n)
    (closure
      (list 'refer-local
            0
            (list 'nuate  (save-stack s) (list 'return n)))
      0
      '())))

(define set-member?
  (lambda (x s)
    (cond
      ((null? s) #f)
      ((eq? x (car s)) #t)
      (else (set-member? x (cdr s))))))

(define set-cons
  (lambda (x s)
    (if (set-member? x s)
      s
      (cons x s))))

(define set-union
  (lambda (s1 s2)
    (if (null? s1)
      s2
      (set-union (cdr s1)
                 (set-cons (car s1) s2)))))

(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
        (cons (car s1) (set-intersect (cdr s1) s2))
        (set-intersect (cdr s1) s2)))))

(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
        (cons (car s1) (set-intersect (cdr s1) s2))
        (set-intersect (cdr s1) s2)))))

;; (find-free
;;   '((lambda (x y) z) 10 20)
;;   '()
;;   )
(define find-free
  (lambda (x b)
    (cond
      ((symbol? x) (if (set-member? x b)
                     '()
                     (list x)))
      ((pair? x)
       (record-case x
                    (quote (obj)
                           '())
                    (lambda (vars body)
                      (find-free body (set-union vars b)))
                    (if (test then else)
                      (set-union (find-free test b)
                                 (set-union (find-free then b)
                                            (find-free else b))))
                    (call/cc (exp)
                             (find-free exp b))
                    (else
                      (recur next ((x x))
                             (if (null? x)
                               '()
                               (set-union (find-free (car x) b)
                                          (next (cdr x))))))))
      (else
        '()))))

(define collect-free
  (lambda (vars e next)
    (if (null? vars)
      next
      (collect-free (cdr vars) e
                    (compile-refer (car vars) e
                                   (list 'argument next))))))

(define compile-refer
  (lambda (x e next)
    (compile-lookup x e
                    (lambda (n) (list 'refer-local n next))
                    (lambda (n) (list 'refer-free  n next)))))

;; (compile-lookup
;;   'z
;;   '((x y) .  (z w))
;;   (lambda (x) x)
;;   (lambda (x) x)
;;   )
(define compile-lookup
  (lambda (x e return-local return-free)
    (recur nxtlocal ((locals (car e)) (n 0))
           (if (null? locals)
             (recur nxtfree ((free (cdr e)) (n 0))    ; free list
                    (if (eq? (car free) x)
                      (return-free n)
                      (nxtfree (cdr free) (+ n 1))))
             (if (eq? (car locals) x)                 ; local list
               (return-local n)
               (nxtlocal (cdr locals) (+ n 1)))))))


(define compile
  (lambda (x e next)
    (cond
      ((symbol? x)
       (compile-refer x e next))
      ((pair? x)
       (record-case x
                    (quote (obj)
                           (list 'constant obj next))
                    (lambda (vars body)
                      (let ((free (find-free body vars)))
                        (collect-free free e
                                      (list 'close
                                            (length free)
                                            (compile body
                                                     (cons vars free)
                                                     (list 'return
                                                           (length vars)))
                                            next))))
                    (if (test then else)
                      (let ((thenc (compile then e next))
                            (elsec (compile else e next)))
                        (compile test e (list 'test thenc elsec))))
                    (call/cc (x)
                             (list 'frame
                                   next
                                   (list 'conti
                                         0
                                         (list 'argument
                                               (compile x e '(apply))))))
                    (else
                      (recur loop ((args (cdr x))
                                   (c (compile (car x) e '(apply))))
                             (if (null? args)
                               (list 'frame next c)
                               (loop (cdr args)
                                     (compile (car args)
                                              e
                                              (list 'argument c))))))))
      (else
        (list 'constant x next)))))


(define *stack*
  (make-vector 100))

;; (set! s (push 1 s))
(define push
  (lambda (x s)
    (vector-set! *stack* s x)
    (+ s 1)))

;; (index s 0)
(define index
  (lambda (s i)
    (vector-ref *stack* (- (- s i) 1))))

;; (index-set! s 0 99)
(define index-set!
  (lambda (s i v)
    (vector-set! *stack* (- (- s i) 1) v)))

(define find-link
  (lambda (n e)
    (if (= n 0)
      e
      (find-link (- n 1) (index e -1)))))

(define save-stack
  (lambda (s)
    (let ((v (make-vector s)))
      (recur copy ((i 0))
             (unless (= i s)
               (vector-set! v i (vector-ref *stack* i))
               (copy (+ i 1))))
      v)))

(define restore-stack
  (lambda (v)
    (let ((s (vector-length v)))
      (recur copy ((i 0))
             (unless (= i s)
               (vector-set! *stack* i (vector-ref v i))
               (copy (+ i 1))))
      s)))

(define display-stack
  (lambda ()
    (display "---bottom---\n")
    (map (lambda (x) (if (undefined? x)
                       '()
                       (begin
                         (display x)
                         (newline))))
         (vector->list *stack*))
    (display "--- top ----\n")))

(define VM
  (lambda (a x f c s)
    ;(display-stack)
    ;(display x) (newline) (newline)
    (record-case x
                 (halt ()
                       a)
                 (refer-local (n x)
                        (VM (index f n) x f c s))
                 (refer-free (n x)
                        (VM (index-closure c n) x f c s))
                 (constant (obj x)
                           (VM obj x f c s))
                 (close (n body x)
                        (VM (closure body n s) x f c (- s n)))
                 (test (then else)
                       (VM a (if a then else) f c s))
                 (conti (n x)
                        (VM (continuation s n) x f c s))
                 (nuate (stack x)
                        (VM a x f c (restore-stack stack)))
                 (frame (ret x)
                        (VM a x f c (push ret (push f (push c s)))))
                 (argument (x)
                           (VM a x f c (push a s)))
                 (apply ()
                        (VM a (closure-body a) s a s))
                 (return (n)
                         (let ((s (- s n)))
                           (VM a (index s 0) (index s 1) (index s 2) (- s 3)))))))

(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) 0 '() 0)))

(display (compile '((lambda (x y) x) 1 2)
                  '(() . ())
                  '(halt)))
(display (evaluate
           '((lambda (x y) y) 1 2)
           ))

(display (compile
           '(call/cc (lambda (k) (k 10)))
           '(() . ())
           '(halt)
           ))

(display (evaluate '(call/cc (lambda (k)  (if (k #f) 10 20)))))
(display (evaluate '(quote hello)))
(display (evaluate '((lambda (x) x) 3)))
(display (evaluate '(if #t 5 0)))
(display (evaluate '(((call/cc (lambda (c) c)) (lambda (x) x)) 11)))
(display (evaluate '((lambda (f x) (f x)) (lambda (x) x) 13)))
(display (evaluate 17))
