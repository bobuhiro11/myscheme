(letrec ([reverse (lambda (in out)
                    (if (null? in)
                      out
                      (reverse (cdr in) (cons (car in) out))))])
  (reverse (cons 1 (cons 2 (cons 3 '()))) '()))
