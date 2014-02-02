;;; Heap Base Scheme Interpreter

; (rec sum (lambda (x) (if (= x 0) 0 (+ x (sum (- x 1))))))
(define-syntax rec
  (syntax-rules ()
                ((_ a b) (let ((a '()))
                           (set! a b)))))

;(recur count ((x '(a b c d e))) (if (null? x) 0 (+ (count (cdr x)) 1)))
(define-syntax recur
  (syntax-rules ()
                ((_ f ((v i) ...) e ...)
                 ((rec f (lambda (v ...) e ...))
                  i ...))))

; (record (x y z) '(1 2 3) (+ x y z))
(define-syntax record
  (syntax-rules ()
                ((_ (var ...) val exp ...)
                 (apply (lambda (var ...) exp ...) val))))

; (record-case '(hello 1 2 3)
;              (hello (x y z)
;                     (+ x y z))
;              (bye   (x y z)
;                     (- x y z)))
(define-syntax record-case
  (syntax-rules (else)
                ((_ exp1 (else exp3 ...))
                 (begin exp3 ...))
                ((_ exp1 (key vars exp2 ...))
                 (if (eq? (car exp1) 'key) (record vars (cdr exp1) exp2 ...)))
                ((_ exp1 (key vars exp2 ...) c ...)
                 (if (eq? (car exp1) 'key)
                   (record vars (cdr exp1) exp2 ...)
                   (record-case exp1 c ...)))))

; (tail? '(return)) => #t
(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

; (display '(if 0 10 20)) => (constant 0 (test (constant 10 ()) (constant 20 ())))
(define display-compile
  (lambda (code)
    (display (compile code '(n)))
    (newline)))

(define compile
  (lambda (x next)
    (cond
      ((symbol? x)
       (list 'refer x next))
      ((pair? x)
       (record-case x
                    (quote   (obj)
                             (list 'constant obj next))
                    (lambda  (vars body)
                             (list 'close vars (compile body '(return)) next))
                    (if      (test then else)
                             (let ((thenc (compile then next))
                                   (elsec (compile else next)))
                               (compile test (list 'test thenc elsec))))
                    (set!    (var x)
                             (compile x (list 'assign var next)))
                    (call/cc (x)
                             (let ((c (list 'conti (list 'argument (compile x '(apply))))))
                               (if (tail? next)
                                 c
                                 (list 'frame next c))))
                    (else
                      (recur loop ((args (cdr x)) (c (compile (car x) '(apply))))
                             (if (null? args)
                               (if (tail? next)
                                 c
                                 (list 'frame next c))
                               (loop (cdr args)
                                     (compile (car args)
                                              (list 'argument c))))))))
      (else
        (list 'constant x next)))))

(display-compile '(if x 10 20))
(display-compile '(set! x 1))
(display-compile '(lambda (x y z) x))
(display-compile '(func 1 2))
