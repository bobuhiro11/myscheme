(use gauche.test)
(load "./main.scm")

(test-start "scheme interpreter")

(test-section "main test")

(test* " #1" 2             (evaluate '((lambda (x y) y) 1 2)))
(test* " #2" #f            (evaluate '(call/cc (lambda (k)  (if (k #f) 10 20)))))
(test* " #3" 'hello        (evaluate '(quote hello)))
(test* " #4" 3             (evaluate '((lambda (x) x) 3)))
(test* " #5" 5             (evaluate '(if #t 5 0)))
(test* " #6" 11            (evaluate '(((call/cc (lambda (c) c)) (lambda (x) x)) 11)))
(test* " #7" 13            (evaluate '((lambda (f x) (f x)) (lambda (x) x) 13)))
(test* " #8" 17            (evaluate 17))
(test* " #9" 19            (evaluate '((lambda (x)
                                           ((lambda (y) x)
                                            (set! x 19)))
                                         29)))
(test* "#10" 100           (evaluate '(set! y 100) ))
(test* "#11" 100           (evaluate '((lambda (x) y) 10)))
(test* "#12" 100           (evaluate 'y))
(test* "#13" '#(1004)      (evaluate '+))
(test* "#14" 3             (evaluate '(+ 1 2)))

(test* "#15" '#(1006 1012) (evaluate '(define func2
                                             (lambda (x y)
                                               (+ x y)))))

(test* "#16"  12           (evaluate '(func2 3 9)))

(test* "#17" '#(1013 1025) (evaluate '(define func
                                            (lambda (x y)
                                              (+ x (+ y 2) )))))
(test* "#18" 8             (evaluate '(func 1  5)))
(test* "#19" #f            (evaluate '(= 1 2)))

(test* "#20" #(1026 1054)  (evaluate '(define sum
                                             (lambda (n s)
                                               (if (= n 0)
                                                 s
                                                 (sum (- n 1) (+ s n)))))))
(test* "#21" 55             (evaluate '(sum 10 0)))
(test* "#22" 22             (evaluate '((lambda (x)
                                               1 2 3
                                               (+ x x))
                                             11)))

(test* "#23" '()            (evaluate '(define-macro double
                                                          (lambda (x)
                                                            (list '+ x x)))))
(test* "#24" '()            (evaluate '(define-macro begin
                                                          (lambda exps
                                                            (list (append (list 'lambda '())
                                      exps))))))
(test* "#25" 20             (evaluate '(double 10)))
(test* "#26" 30             (evaluate '(begin 10 20 30)))

(test* "#27" '() (evaluate '(define-macro let
                                               (lambda (binds . bodies)
                                                 (cons (append (list 'lambda
                                                                     (map (lambda (x) (car x))
                                                                          binds))
                                                               bodies)
                                                       (map (lambda (x) (cadr x)) binds))))))

(test* "#28" 11             (evaluate '(let ((a 10)) (+ a 1))))
(test* "#29" 10             (evaluate '(define x 10)))

(test* "#30" 30             (evaluate '(+ (call/cc (lambda (c) (set! x c) (c 10))) 20)))
;(test* "#31" '#(19 21)      (evaluate 'x))

(test-end)
