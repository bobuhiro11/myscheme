#!/usr/bin/env gosh

(add-load-path "." :relative)
(load "util.scm")
(load "macro.scm")
(load "compile.scm")
(load "linear.scm")
(load "vm.scm")

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

;; format for C language
(define linear-compile
  (lambda (code)
    (map (lambda (x)
           (display (format #f "~3d  " (car x)))
           (display (cadr x))
           (newline))
        (3imp->linear
         (compile (expand-traditional-macro code) '(() . ()) '() '(halt))
         0))))

(define my-repl
  (lambda ()
    (let ([c (read)])
      (unless (eof-object? c)
        (display "#begin") (newline)
        (linear-compile c)
        (display "#end") (newline)
        (flush)
        (my-repl)))))

(my-repl)
