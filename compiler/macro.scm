;;; Macro
;;;
;;; 伝統的なマクロにより，compile以前にコードの展開する

(add-load-path "." :relative)
(load "util.scm")

(define *traditional-macros*
  (list
    (cons 'begin (lambda exps (list (append (list 'lambda '()) exps))))
    (cons 'let   (lambda (binds . bodies)
                   (cons (append (list 'lambda
                                       (map (lambda (x) (car x))
                                            binds))
                                 bodies)
                         (map (lambda (x) (cadr x)) binds))))
    (cons 'letrec (lambda (args . bodies)
                    (let ([vars (map (lambda (x) (car x)) args)]
                          [vals (map (lambda (x) (cadr x)) args)])
                      (append (list (append (list 'lambda vars)
                                            (map (lambda (x) (list 'set! (car x) (cadr x)))
                                                 args)
                                            bodies
                                            ))
                              (map (lambda (x) 0) args)))))
    (cons 'list (lambda args
                  (letrec ([next (lambda (args)
                                  (if (null? args)
                                    '()
                                    (list 'cons (car args) (next (cdr args)))))])
                    (next args))))))


;; (get-traditional-macro 'double)
;;
;; => (double . (lambda (x) (list '* x x)))
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
                     (append (list 'lambda (map expand-traditional-macro vars))
                             (map expand-traditional-macro bodies))]
                   [else
                     (let ([macro (get-traditional-macro (car x))]
                           [args (map expand-traditional-macro (cdr x))])
                       (if macro
                         (apply (cdr macro) args)
                         (cons (expand-traditional-macro (car x)) args)))])
      x)))
