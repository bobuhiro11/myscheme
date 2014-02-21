;;; Macro
;;;
;;; 伝統的なマクロにより，compile以前にコードの展開する

(load "./util.scm")

(define *traditional-macros* '())

;; (get-traditional-macro 'double)
;;
;; => (double . (lambda (list '* x x)))
(define get-traditional-macro
  (lambda (x)
    (assq x *traditional-macros*)))

;; (expand-traditional-macro '(double (double 2)))
;;
;; => (* (* 2 2) (* 2 2))
;;
(define expand-traditional-macro
  (lambda (x)
    (if (pair? x)
      (record-case x
                   [lambda (vars . bodies)
                     ; lambda式の場合(define-macro内も含む)は，bodyのみを評価する．
                     (append (list 'lambda vars)
                           (map expand-traditional-macro bodies))]
                   [else
                     (let ([macro (get-traditional-macro (car x))]
                           [args (map expand-traditional-macro (cdr x))])
                       (if macro
                         (apply (cdr macro) args)
                         (cons (car x) args)))])
      x)))