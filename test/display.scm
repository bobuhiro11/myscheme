(letrec ([next (lambda (i)
                 (if (= i 0)
                   0
                   (begin (display "sintyokudamedesu") (newline) (next (- i 1)))))])
  (next 10))
