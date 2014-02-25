(letrec ([reverse (lambda (in out)
                    (if (null? in)
                      out
                      (reverse (cdr in) (cons (car in) out))))]
         [makelist (lambda (x)
                     (if (= x 0)
                       (cons 0 '())
                       (cons x (makelist (- x 1)))))])
  (reverse (quote (1 1 2 3 5 8)) ()))
