;;; Heap Base Scheme Interpreter

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

;; (tail? '(return)) => #t
(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

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
       (list 'refer (compile-lookup x e) next))
      ((pair? x)
       (record-case x
                    (quote   (obj)
                             (list 'constant obj next))
                    (lambda  (vars body)
                      (list 'close
                            (compile body (extend e vars) '(return))
                            next))
                    (if      (test then else)
                      (let ((thenc (compile then next))
                            (elsec (compile else next)))
                        (compile test e (list 'test thenc elsec))))
                    (set!    (var x)
                      (let ((access (compile-lookup var e)))
                        (compile x e (list 'assign access next))))
                    (call/cc (x)
                             (let ((c (list 'conti (list 'argument (compile x e '(apply))))))
                               (if (tail? next)
                                 c
                                 (list 'frame next c))))
                    (else
                      (recur loop ((args (cdr x))
                                   (c (compile (car x) e '(apply))))
                             (if (null? args)
                               (if (tail? next)
                                 c
                                 (list 'frame next c))
                               (loop (cdr args)
                                     (compile (car args)
                                              e
                                              (list 'argument c))))))))
      (else
        (list 'constant x next)))))



;; (lookup
;;    'a
;;    '(((d e f) . (3 4 5))
;;      ((c) . (2))
;;      ((a b) . (0 1)))) => (0 1)
(define lookup
  (lambda (var e)
    (recur nxtrib ((e e))
           (recur nxtelt ((vars (caar e)) (vals (cdar e))) ; vars = (d e f), vals = (3 4 5)
                  (cond
                    ((null? vars) (nxtrib (cdr e)))
                    ((eq? (car vars) var) vals)
                    (else (nxtelt (cdr vars) (cdr vals))))))))

;; (closure '(cons x y) '((x).(1)) '(y))
;;    => ((cons x y) ((x) 1) (y))
(define closure
  (lambda (body e vars)
    (list body e vars)))

;; (continuation '((return) ((a) (1)) () ()))
;;  =>
;; ((nuate ((return) ((a) (1)) () ()) v) () (v))
(define continuation
  (lambda (s)
    (closure (list 'nuate s 'v)
             '()
             '(v))))

;; (call-frame '(return)
;;             '((a). (1))
;;             '()
;;             '())
;; => ((return) ((a). (1)) () ())
(define call-frame
  (lambda (x e r s)
    (list x e r s)))

;;(compile-lookup
;;  'c
;;  '((x y z) (a b c) (u v))
;;  )
;; => (1. 2)
(define compile-lookup
  (lambda (var e)
    (recur nxtrib ((e e) (rib 0))
           (recur nxtelt ((vars (car e)) (elt 0))
                  (cond
                    ((null? vars) (nxtrib (cdr e) (+ rib 1)))  ; next rib
                    ((eq? (car vars) var) (cons rib elt))      ; discover
                    (else (nxtelt (cdr vars) (+ elt 1))))))))  ; next elt


;(define VM
;  (lambda (a x e r s)
;    (record-case x
;                 (halt () a)
;                 (refer (var x)
;                        (VM (car (lookup var e)) x e r s))
;                 (constant (obj x)
;                           (VM obj x e r s))
;                 (close (vars body x)
;                        (VM (closure body e vars) x e r s))
;                 (test (then else)
;                       (VM a (if a then else) e r s))
;                 (assign (var x)
;                         (set-car! (lookup var e) a)
;                         (VM a x e r s))
;                 (conti (x)
;                        (VM (continuation s) x e r s))
;                 (nuate (s var)
;                        (VM (car (lookup var e)) '(return) e r s))
;                 (frame (ret x)
;                        (VM a x e '() (call-frame ret e r s)))
;                 (argument (x)
;                           (VM a x e (cons a r) s))
;                 (apply ()
;                        (record (body e vars) a
;                                (VM a body (extend e vars r) '() s)))
;                 (return ()
;                         (record (x e r s) s
;                                 (VM a x e r s))))))
; (define evaluate
;   (lambda (x)
;     (VM '() (compile x '(halt)) '() '() '())))


;(debug '(if 0 10 20))
;(debug '(lambda (x y z) x))
;(debug '((lambda (x y z) z)
;                     1 2 3))
;(debug '((lambda (x y z)
;                       ((lambda (p) y
;                          ) (set! y 10))
;                      )
;                     1 2 3))
(display (compile
           '((lambda (x y) x) 10 20) '() '(halt)))
(newline)
(display (compile
'(lambda (x y)
   ((lambda (a b c)
      (a (lambda (x b) (y x c b))
         b
         y))
    x
    x
    y))
'()
'(halt)))
