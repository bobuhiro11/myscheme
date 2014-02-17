(load "./util.scm")
(load "./macro.scm")
(load "./compile.scm")
(load "./linear.scm")
(load "./vm.scm")

(define evaluate
  (lambda (x)
    (VM '()     ; accumlator
        0       ; program counter
        0       ; frame pointer
        '()     ; closure
        0       ; stack pointer
        (3imp->linaer ;code
         (compile (expand-traditional-macro x) '(() . ()) '() '(halt))
         0))))

(define debug
  (lambda (code)
    (let ([opecode (3imp->linear
                    (compile (expand-traditional-macro code) '(() . ()) '() '(halt))
                    0)])
      (display opecode)
      (newline)
      (display (VM '() 0 0 '() 0 opecode))
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

(debug '(define func2
          (lambda (x y)
            (+ x y))))

(debug '(func2 3 9))

(debug '(define func
          (lambda (x y)
            (+ x (+ y 2) ))))
(debug '(func 1  5))
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
