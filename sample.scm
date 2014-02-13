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
