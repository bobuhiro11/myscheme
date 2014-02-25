(letrec ([s (lambda (x)
             (if (= x 0)
              0
              (+ x (s (- x 1)))))])
 (s 10))
