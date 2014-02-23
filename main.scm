#!/usr/bin/gosh

(load "./util.scm")
(load "./macro.scm")
(load "./compile.scm")
(load "./linear.scm")
(load "./vm.scm")
(use slib)
(require 'trace)

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

;; C$B8@8l$h$&$K%U%)!<%^%C%H$9$k!%(B
(define linear-compile
  (lambda (code)
    (map (lambda (x)
           (recur next ([x x])
                  (unless (null? x)
                    (display (car x))
                    (display " ")
                    (next (cdr x))))
           (newline))
        (3imp->linear
         (compile (expand-traditional-macro code) '(() . ()) '() '(halt))
         0))))

;(linear-compile '(letrec ([s (lambda (x)
;                               (if (= x 0)
;                                 0
;                                 (+ x (s (- x 1)))))])
;                   (s 10)))

(define my-repl
  (lambda ()
    (let ([c (read)])
      (unless (eof-object? c)
        (linear-compile c)))))

(my-repl)
