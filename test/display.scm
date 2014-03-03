(letrec ([next (lambda (i)
                 (if (= i 0)
                   0
                   (begin (space i) (display "sintyokudamedesu") (newline) (next (- i 1)))))]
         [space (lambda (i)
                  (if (> i 20)
                    (space (- i 20))
                    (if (> i 10)
                      (_space (- i 10))
                      (_space (- 10 i)))))]
         [_space (lambda (i)
                   (if (= i 0)
                     0
                     (begin  (display ".")
                             (_space (- i 1)))))])
  (next 100))
