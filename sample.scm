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
;(define-syntax record
;  (syntax-rules ()
;                ((_ (var ...) val exp ...)
;                 (apply (lambda (var ...) exp ...) val))))
(define-syntax record
  (syntax-rules ()
                ((_ args parm exps ...)
                 (apply (lambda args exps ...) parm))))

;; (record-case '(hello 1 2 3)
;;              (hello (x y z)
;;                     (+ x y z))
;;              (bye   (x y z)
;;                     (- x y z)))
(define-syntax record-case
  (syntax-rules (else)
                [(_ exp1 (else exp3 ...))
                 (begin exp3 ...)]
                [(_ exp1 (key vars exp2 ...))
                 (if (eq? (car exp1) 'key) (record vars (cdr exp1) exp2 ...))]
                [(_ exp1 (key vars exp2 ...) c ...)
                 (if (eq? (car exp1) 'key)
                   (record vars (cdr exp1) exp2 ...)
                   (record-case exp1 c ...))]))
(define *macros* '())

;; (get-macro 'double)
;;
;; => (double . (lambda (list '* x x)))
(define get-macro
  (lambda (x)
    (assq x *macros*)))

;; (expand-macro '(double (double 2)))
;;
;; => (* (* 2 2) (* 2 2))
;;
(define expand-macro
  (lambda (x)
    (if (pair? x)
      (record-case x
                   [lambda (vars . bodies)
                     ;(display "vars=")
                     ;(display vars)
                     ;(newline)
                     ;(display "bodies=")
                     ;(display bodies)
                     ;(newline)
                     (append (list 'lambda vars)
                           (map expand-macro bodies))]
                   [else
                     (let ([macro (get-macro (car x))]
                           [args (map expand-macro (cdr x))])
                       (if macro
                         (apply (cdr macro) args)
                         (cons (car x) args)))])
      x)))

;(display (expand-macro '(lambda (x y) (+ x y))))
;(newline)
;(display (expand-macro '(lambda args (cons args y))))
;(newline)
;(display (expand-macro '(define-macro (lambda (x . y) (cons x y)))))
;(newline)


;; (def-macro 'double
;;            (lambda (x)
;;             `(* ,x ,x)))
(define def-macro
  (lambda (name closure)
    (set! *macros*
      (cons (cons name closure)
            *macros*))))

(def-macro 'double
           (lambda (x)
           `(* ,x ,x)))

;(display *macros*)
;(display (expand-macro '(double 10)))


;(newline)
;
;;; gaucheの方に定義してテスト
;(define-macro my-let
;              (lambda (binds . bodies)
;                (cons (append (list 'lambda (map (lambda (x) (car x)) binds))
;                              bodies)
;                      (map (lambda (x) (cadr x)) binds))))
;(display (macroexpand '(my-let ((a 10) (b (+ 20 10))) (+ a b))))
;
;
;;
;;(define x 10)
;;(define y x)
;;(set! x 20)
;;(display x) => 20
;;(display y) => 10
;
;;(define x 10)
;;((lambda ()
;;   (set! x 20)
;;   ))
;;(display x)
;; => 20
;
;;(define x 10)
;;(define func
;;  (lambda () x))
;;(set! x 20)
;;(display (func))
;; => 20
;
;(define x 10)
;(define func
;  (lambda () x))
;(define x 20)
;(display (func))
;; => 20
;
;(define bank-account
;  (let ((amount 1000))
;    (lambda (n)
;      (set! amount (+ amount n))
;      amount)))
;(display (bank-account 2000))
;(display (bank-account -2500))
;
;
;;; (record (x y z) '(1 2 3) (+ x y z))
;(define-syntax record
;  (syntax-rules ()
;                ((_ args parm exps ...)
;                 (apply (lambda args exps ...) parm))))
;(display
;  (record (x . y) '(1 4) y)
;  )
;;; (record-case '(hello 1 2 3)
;;;              (hello (x y z)
;;;                     (+ x y z))
;;;              (bye   (x y z)
;;;                     (- x y z)))
;;;
;(define-syntax record-case
;  (syntax-rules (else)
;                ((_ exp1 (else exp3 ...))
;                 (begin exp3 ...))
;                ((_ exp1 (key vars exp2 ...))
;                 (if (eq? (car exp1) 'key) (record vars (cdr exp1) exp2 ...)))
;                ((_ exp1 (key vars exp2 ...) c ...)
;                 (if (eq? (car exp1) 'key)
;                   (record vars (cdr exp1) exp2 ...)
;                   (record-case exp1 c ...)))))
;
;(display (record-case '(hello 1 2 3)
;              (hello (x y .  z)
;                     (display x)
;                     (display y)
;                     (display z))
;              (bye   (x y z)
;                     (- x y z))))

(define (myabs x)
  (if (> x 0)
    x
    (- 0 x)))

(define (addelement x)
  (append x '(a b c)))

;(display (disasm myabs))
;(display (disasm addelement))

;(frame (halt) (conti (argument (close 0 (frame (test (constant 10 (return 1)) (constant 20 (return 1))) (constant #f (argument (refer-local 0 (apply))))) (apply)))))
(define vmcode->ccode
  (lambda (code adr)
    (record-case code
                 [halt ()
                       (list (list adr 'halt))]
                 [refer-local (n x)
                              (cons (list adr 'refer-local n)
                                    (vmcode->ccode x (+ adr 1)))]
                 [refer-free (n x)
                              (cons (list adr 'refer-free n)
                                    (vmcode->ccode x (+ adr 1)))]
                 [refer-global (n x)
                              (cons (list adr 'refer-global n)
                                    (vmcode->ccode x (+ adr 1)))]
                 [indirect (x)
                              (cons (list adr 'indirect)
                                    (vmcode->ccode x (+ adr 1)))]
                 [constant (obj x)
                              (cons (list adr 'constant obj)
                                    (vmcode->ccode x (+ adr 1)))]
                 [close (n body x)
                        (let* ([nextc (vmcode->ccode x (+ adr 1))]
                               [bodyadr (+ 1 adr (length nextc))]
                               [bodyc (vmcode->ccode body bodyadr)])
                          (cons (list adr 'close n bodyadr)
                                (append nextc bodyc)))]
                 [box (n x)
                              (cons (list adr 'box n)
                                    (vmcode->ccode x (+ adr 1)))]
                 [test (then else)
                       (let* ([thenc (vmcode->ccode then (+ adr 1))]
                              [elsadr (+ 1 adr (length thenc))]
                              [elsec (vmcode->ccode else elsadr)])
                         (cons (list adr 'test elsadr)
                               (append thenc elsec)))]
                 [plus (x)
                       (cons (list adr 'plus)
                             (vmcode->ccode x (+ adr 1)))]
                 [minus (x)
                       (cons (list adr 'minus)
                             (vmcode->ccode x (+ adr 1)))]
                 [equal (x)
                       (cons (list adr 'equal)
                             (vmcode->ccode x (+ adr 1)))]
                 [assign-local (n x)
                       (cons (list adr 'assign-local n)
                             (vmcode->ccode x (+ adr 1)))]
                 [assign-free (n x)
                       (cons (list adr 'assign-free n)
                             (vmcode->ccode x (+ adr 1)))]
                 [assign-global (n x)
                       (cons (list adr 'assign-global n)
                             (vmcode->ccode x (+ adr 1)))]
                 [define (n x)
                       (cons (list adr 'define n)
                             (vmcode->ccode x (+ adr 1)))]
                 [conti (x)
                       (cons (list adr 'conti )
                             (vmcode->ccode x (+ adr 1)))]
                 [nuate (stack x)
                       (cons (list adr 'nuate stack)
                             (vmcode->ccode x (+ adr 1)))]
                 [frame (ret x)
                        (let* ([xcode (vmcode->ccode x (+ adr 1))]
                               [retadr (+ 1 adr (length xcode))]
                               [retcode (vmcode->ccode ret retadr)])
                          (cons (list adr 'frame retadr)
                                (append xcode retcode)))]
                 [argument (x)
                       (cons (list adr 'argument)
                             (vmcode->ccode x (+ adr 1)))]
                 [shift (n m x)
                       (cons (list adr 'shift n m)
                             (vmcode->ccode x (+ adr 1)))]
                 [apply ()
                       (list (list adr 'apply ))]
                 [return (n)
                         (list (list adr 'return n))])))

(define display-ccode
  (lambda (ccode)
    (if (null? ccode)
      '()
      (begin
        (map (lambda (x) (display x) (display " "))(car ccode))
        (newline)
        (display-ccode (cdr ccode))))))

(display-ccode (vmcode->ccode '(halt) 0))
(newline)
(display-ccode (vmcode->ccode '(refer-local 0 (halt)) 0))
(newline)
(display-ccode (vmcode->ccode '(test (halt) (halt)) 0))
(newline)
(display-ccode (vmcode->ccode '(close 0 (test (halt) (halt)) (halt)) 0))
(newline)
(display-ccode (vmcode->ccode '(frame (halt) (plus (halt))) 0))
(newline)
(display-ccode
  (vmcode->ccode
    '(frame (halt)
            (conti
              (argument
                (close 0
                       (frame (test (constant 10 (return 1)) (constant 20 (return 1)))
                              (constant #f
                                        (argument
                                          (refer-local 0
                                                       (apply)))))
                       (apply)))))
    0))
(newline)
