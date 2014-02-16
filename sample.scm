;(define x 10)
;(define y x)
;(set! x 20)
;(display x) => 20
;(display y) => 10

;(define x 10)
;((lambda ()
;   (set! x 20)
;   ))
;(display x)
; => 20

;(define x 10)
;(define func
;  (lambda () x))
;(set! x 20)
;(display (func))
; => 20

(define x 10)
(define func
  (lambda () x))
(define x 20)
(display (func))
; => 20

(define bank-account
  (let ((amount 1000))
    (lambda (n)
      (set! amount (+ amount n))
      amount)))
(display (bank-account 2000))
(display (bank-account -2500))


;; (record (x y z) '(1 2 3) (+ x y z))
(define-syntax record
  (syntax-rules ()
                ((_ args parm exps ...)
                 (apply (lambda args exps ...) parm))))
(display 
  (record (x . y) '(1 4) y)
  )
;; (record-case '(hello 1 2 3)
;;              (hello (x y z)
;;                     (+ x y z))
;;              (bye   (x y z)
;;                     (- x y z)))
;;
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

(display (record-case '(hello 1 2 3)
              (hello (x y .  z)
                     (display x)
                     (display y)
                     (display z))
              (bye   (x y z)
                     (- x y z))))
