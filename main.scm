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
        0       ; argp pointer
        '()     ; closure
        0       ; stack pointer
        (3imp->linear ;code
         (compile (expand-traditional-macro x) '(() . ()) '() '(halt))
         0))))

(define debug
  (lambda (code)
    (let* ([opecode  (compile (expand-traditional-macro code) '(() . ()) '() '(halt))]
           [opecode2 (3imp->linear opecode 0)])
      (display opecode)
      (newline)
      (display opecode2)
      (newline)
      (display (VM '() 0 0 0 '() 0 opecode2))
      (newline))))

;(debug '(+ (call/cc (lambda (c) (set! x c) (c 10))) 20))
;(debug 'x)
;(debug '(+ 1 2))
;(debug '(+ (call/cc (lambda (c) (set! x c) (c 10))) 20))
;(debug '(+ 20 (call/cc (lambda (c) (set! x c) (c 10)))))
(debug '(+ (+ 1 2) (+ 3 4)))
;(debug 'x)
