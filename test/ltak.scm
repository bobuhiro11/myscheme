(letrec ([ltak (lambda (x y z)
                 (if (< (x) (y))
                   (y)
                   (if (= (x) (y))
                     (y)
                     (ltak (lambda () (ltak (lambda () (- (x) 1)) y z))
                           (lambda () (ltak (lambda () (- (y) 1)) z x))
                           (lambda () (ltak (lambda () (- (z) 1)) x y))))))]
         [tak (lambda (x y z)
                (ltak (lambda () x)
                      (lambda () y)
                      (lambda () z)))])
  (tak 20 10 0))