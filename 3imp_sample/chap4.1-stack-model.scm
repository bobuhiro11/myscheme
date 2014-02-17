;;; Chap 4.1 Stack Base Scheme Interpreter

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

;; (compile-lookup
;;   'c
;;   '((x y z) (a b c) (u v))
;;   (lambda (n m) (cons n m))  ; n...rib, m...elt
;;   )
;; => (1. 2)
(define compile-lookup
  (lambda (var e return)
    (recur nxtrib ((e e) (rib 0))
           (recur nxtelt ((vars (car e)) (elt 0))
                  (cond
                    ((null? vars) (nxtrib (cdr e) (+ rib 1)))  ; next rib
                    ((eq? (car vars) var) (return rib elt))      ; discover
                    (else (nxtelt (cdr vars) (+ elt 1))))))))  ; next elt

;; (extend
;;   '((a b c) (u v))
;;   '(x y z))
;;
;; => ((x y z) (a b c) (u v))
(define extend
  (lambda (env rib)
    (cons rib env)))


(define compile
  (lambda (x e next)
    (cond
      ((symbol? x)
       (compile-lookup x e
                       (lambda (n m)
                         (list 'refer n m next))))
      ((pair? x)
       (record-case x
                    (quote (obj)
                           (list 'constant obj next))
                    (lambda (vars body)
                      (list 'close
                            (compile body
                                     (extend e vars)
                                     (list 'return (+ (length vars) 1)))
                            next))
                    (if (test then else)
                      (let ((thenc (compile then e next))
                            (elsec (compile else e next)))
                        (compile test e (list 'test thenc elsec))))
                    (set! (var x)
                      (compile-lookup var e
                                      (lambda (n m)
                                        (compile x e (list 'assign n m next)))))
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

(define functional
  (lambda (body e)
    (list body e)))

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
  (lambda (a x e s)
    ;(display-stack)
    (display x) (newline) (newline)
    (record-case x
                 (halt ()
                       a)
                 (refer (n m x)
                        (VM (index (find-link n e) m) x e s))
                 (constant (obj x)
                           (VM obj x e s))
                 (close (body x)
                        (VM (functional body e) x e s))
                 (test (then else)
                       (VM a (if a then else) e s))
                 (assign (n m x)
                         (index-set! (find-link n e) m a)
                         (VM a x e s))
                 (frame (ret x)
                        (VM a x e (push ret (push e s))))
                 (argument (x)
                           (VM a x e (push a s)))
                 (apply ()
                        (record (body link) a
                                (VM a body s (push link s))))
                 (return (n)
                         (let ((s (- s n)))
                           (VM a (index s 0) (index s 1) (- s 2)))))))

;(display (compile '((lambda (x y) x) 1 2)
;                  '((x y z) (a b c) (u v))
;                  '(halt)))

(define evaluate
  (lambda (x)
    (VM '() (compile x 0 '(halt)) 0 0)))

;(display (evaluate
;           '(if #t 1 2)
;           ))
;(newline)
;
;(display (evaluate
;           '(quote (1 2 3))
;           ))
;(newline)
;
;(display (evaluate
;           '((lambda (x y) y) 19 29)
;           ))
;(newline)

(display (compile
           '((lambda (x y) (x y))
              (lambda (x) (if x 10 20))
              #f)
           '()
           '(halt)
           ))
(display (evaluate
           '((lambda (x y) (x y))
              (lambda (x) (if x 10 20))
              #f)
           ))
(newline)
