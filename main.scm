(load "./util.scm")
(load "./macro.scm")
(load "./compile.scm")
(load "./vm.scm")

(define evaluate
  (lambda (x)
    (VM '() (compile (expand-traditional-macro x) '(() . ()) '() '(halt)) 0 '() 0)))

(define debug
  (lambda (code)
    (let ((opecode (compile (expand-traditional-macro code) '(() . ()) '() '(halt))))
      (display opecode)
      (newline)
      (display (VM '() opecode 0 '() 0))
      (newline))))

(debug '((lambda (x y) y) 1 2))
(debug '(call/cc (lambda (k)  (if (k #f) 10 20))))
(debug '(quote hello))
(debug '((lambda (x) x) 3))
(debug '(if #t 5 0))
(debug '(((call/cc (lambda (c) c)) (lambda (x) x)) 11))
(debug '((lambda (f x) (f x)) (lambda (x) x) 13))
(debug 17)
(debug '((lambda (x)
           ((lambda (y) x)
            (set! x 19)))
         29))
(debug '((lambda (x) y) 10))
(debug '(set! y 100) )
(debug 'y)
(debug '+)
(debug '(+ 1 2))
(debug '(define func
          (lambda (x y z)
            (+ x (+ y z)))))
(debug '(func 1 2 3))
(debug '(= 1 2))

(debug '(define sum
          (lambda (n s)
            (if (= n 0)
              s
              (sum (- n 1) (+ s n))))))
(debug '(sum 10 0))
(debug '((lambda (x)
           1 2 3
           (+ x x))
         11))

(debug '(define-macro double
                      (lambda (x)
                        (list '+ x x))))
(debug '(define-macro begin
                      (lambda exps
                        (list (append (list 'lambda '())
                                      exps)))))
(debug '(double 10))
(debug '(begin 10 20 30))

(debug '(define-macro let
                      (lambda (binds . bodies)
                        (cons (append (list 'lambda (map (lambda (x) (car x)) binds))
                                      bodies)
                              (map (lambda (x) (cadr x)) binds)))))

(debug '(let ((a 10)) (+ a 1)))
