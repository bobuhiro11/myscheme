;;; Utilities

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

(define set-member?
  (lambda (x s)
    (cond
      [(null? s) #f]
      [(eq? x (car s)) #t]
      [else (set-member? x (cdr s))])))

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

(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
        (cons (car s1) (set-intersect (cdr s1) s2))
        (set-intersect (cdr s1) s2)))))

(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
	(set-minus (cdr s1) s2)
	(cons (car s1) (set-minus (cdr s1) s2))))))
