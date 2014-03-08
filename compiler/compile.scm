;;; Compilier
;;;
;;; compile from code that is expanded macro to 3imp VM bytecode

(add-load-path "." :relative)
(load "util.scm")

;; find free variable NOT contain global variable
;; (find-free
;;   '((lambda (x y) z) 10 20)
;;   '(x y)                         ; bound variable
;;   '(() . (x y z))                ; (local variable list . free variable list)
;;   )
;;
;;   => (z)
(define (find-free x b e)
  (cond [(symbol? x)
         ;(if (set-member? x b)
         (if (and (not (set-member? x b))
                  (or  (set-member? x (car e))
                       (set-member? x (cdr e))))
           (list x) ; free variable is not bound variable, and contained in "e" list.
           '())]
        [(pair? x)
         (record-case x
                      [quote (obj) '()]
                      [lambda (vars . bodies)
                        (find-free-bodies bodies (set-union vars b) e)]
                      [if (test then else)
                        (set-union (find-free test b e)
                                   (set-union (find-free then b e)
                                              (find-free else b e)))]
                      [set! (var exp)
                        (if (and (not (set-member? x b))
                                 (or  (set-member? x (car e))
                                      (set-member? x (cdr e))))
                          (set-cons var (find-free exp b e))
                          (find-free exp b e))]
                      [call/cc (exp)
                               (find-free exp b e)]
                      [else
                        (let next ([x x])
                          (if (null? x)
                            '()
                            (set-union (find-free (car x) b e)
                                       (next (cdr x)))))])]
        [else '()]))

;; find bound varibale by set! instruction
;; (find-sets '(lambda (x y) (set! x z))
;;            '(x y z)) ; list for search
;;
;; => (x)
(define find-sets
  (lambda (x v)
    (cond
      [(symbol? x)
       '()]
      [(pair? x)
       (record-case x
                    [quote (obj)
                           '()]
                    [lambda (vars . bodies)
                      (find-sets-bodies bodies (set-minus v vars))]
                    [if (test then else)
                      (set-union (find-sets test v)
                                 (set-union (find-sets then v)
                                            (find-sets else v)))]
                    [set! (var x)
                      (set-union (if (set-member? var v)
                                   (list var)
                                   '())
                                 (find-sets x v))]
                    [call/cc (exp)
                             (find-sets exp v)]
                    [else
                      (recur next ((x x))
                             (if (null? x)
                               '()
                               (set-union (find-sets (car x) v)
                                          (next (cdr x)))))])]
       [else
         '()])))

;; (compile-lookup
;;   'z
;;   '((x y) .  (z w))
;;   (lambda (x) (list 'local x))
;;   (lambda (x) (list 'free x))
;;   (lambda (x) (list 'global x))
;;   )
;;   =>
;;   (free 0)
(define compile-lookup
  (lambda (x e return-local return-free return-global)
    (recur nxtlocal ((locals (car e)) (n 0))
           (if (null? locals)
             (recur nxtfree ((free (cdr e)) (n 0))    ; free list
                    (if (null? free)
                      (return-global x)               ; global
                      (if (eq? (car free) x)
                        (return-free n)
                        (nxtfree (cdr free) (+ n 1)))))
             (if (eq? (car locals) x)                 ; local list
               (return-local n)
               (nxtlocal (cdr locals) (+ n 1)))))))

;; (compile-refer
;;   'z
;;   '((x y) .(z w))
;;   '(halt))
;;
;; => (refer-free 0)
(define compile-refer
  (lambda (x e next)
    (compile-lookup x e
                    (lambda (n) (list 'refer-local n next))
                    (lambda (n) (list 'refer-free  n next))
                    (lambda (n) (list 'refer-global  n next)))))

;; collect free variable for creating closure object
;; (collect-free
;;   '(x w)                    ; free variable list
;;   '((a b c) . (x y z w))    ; (local variable . free variable)
;;   '(halt))                  ; next instruction
;;
;;   =>
;;
;; (refer-free 3 (argument (refer-free 0 (argument (halt)))))
(define collect-free
  (lambda (vars e next)
    (if (null? vars)
      next
      (collect-free (cdr vars)
                    e
                    (if (or (set-member? (car vars) (car e))
                            (set-member? (car vars) (cdr e)))
                      (compile-refer (car vars)
                                     e
                                     (list 'argument next))
                      next)))))

;; (make-boxes
;;   '(y)
;;   '(x y z)
;;   '(halt))
;;
;; =>
;; (box 1 (halt))
(define make-boxes
  (lambda (sets vars next)
    (recur f ((vars vars) (n 0))
           (if (null? vars)
             next
             (if (set-member? (car vars) sets)
               (list 'box n (f (cdr vars) (+ n 1)))
               (f (cdr vars) (+ n 1)))))))

(define (tail? x)
  (eq? 'return (car x)))

;; (def-traditional-macro 'double
;;            (lambda (x)
;;             `(* ,x ,x)))
(define def-traditional-macro
  (lambda (name closure)
    (set! *traditional-macros*
      (cons (cons name closure)
            *traditional-macros*))))

; for multiple body of lambda expression
;
; (find-free-bodies
;   '((+ x y) 1 (+ z w))
;   '(x y z)
;   '(() . (z w)))
;
;   => (w)
(define (find-free-bodies bodies vars e)
  (recur next ((bodies bodies) (r '()))
         (if (null? bodies)
           r
           (next (cdr bodies)
                 (set-union r
                            (find-free (car bodies) vars e))))))


; for multiple body of lambda expression
;
; (find-sets-bodies
;   '((+ x y) (set! x y) (+ z w))
;   '(x y z))
;
; => (x)
(define (find-sets-bodies bodies vars)
  (recur next ((bodies bodies) (r '()))
         (if (null? bodies)
           r
           (next (cdr bodies)
                 (set-union r
                            (find-sets (car bodies) vars))))))

(define (compile-lambda e s next vars bodies)
  (let ([free (find-free-bodies  bodies vars e)] ; free varibale
        [sets (find-sets-bodies  bodies vars)])  ; bound variable
    (collect-free free e
                  (list 'close
                        (length free)
                        ; create box for parameter of closure
                        (make-boxes sets vars
                                    (recur next ([newe (cons vars free)]
                                                 [news (set-union sets (set-intersect s free))]
                                                 [bodies bodies])
                                           (compile (car bodies) newe news
                                                    (if (= (length bodies) 1)
                                                      (list 'return (length vars))
                                                      (next newe news (cdr bodies))))))
                        next))))

;; (compile '(if #t 1 2) '(() . ()) '() '(halt))
;;
;; e = (local variable list . free variable list)
;; s = (bound free variable list)
(define (compile x e s next)
  (cond
    [(null? x)
     (list 'constnil x next)]
    [(boolean? x)
     (list 'constboo x next)]
    [(symbol? x)
     (compile-refer x e
                    (if (set-member? x s)
                      (list 'indirect next)
                      next))]
    [(string? x)
     (list 'conststr x next)]
    [(number? x)
     (list 'constnum x next)]
    [(pair? x)
     (record-case x
                  [display (x)
                    (compile x e s (list 'display next))]
                  [disasm (x)
                    (compile x e s (list 'disasm next))]
                  [newline ()
                    (list 'newline next)]
                  [gcrun ()
                    (list 'gcrun next)]
                  [gcdump ()
                    (list 'gcdump next)]
                  [quote (obj)
                         (cond
                           [(null? x)
                            (list 'constnil x next)]
                           [(boolean? x)
                            (list 'constboo x next)]
                           [(pair? obj)
                            (compile (letrec
                                       ([next (lambda (args)
                                                (if (null? args)
                                                  '()
                                                  (list 'cons (list 'quote (car args)) (next (cdr args)))))])
                                       (next obj)) e s next)]
                           [(symbol? obj)
                            (list 'constsym obj next)]
                           [(string? obj)
                            (list 'conststr obj next)]
                           [(number? obj)
                            (list 'constnum obj next)]
                           [else
                             (compile (eval x null-environment) e s next)])]
                  [lambda (vars . bodies)
                    (compile-lambda e s next vars bodies)]
                  [if (test then else)
                    (let ([thenc (compile then e s next)]
                          [elsec (compile else e s next)])
                      (compile test e s (list 'test thenc elsec)))]
                  [set! (var x)
                        (compile-lookup var e
                                        (lambda (n) (compile x e s (list 'assign-local n next)))
                                        (lambda (n) (compile x e s (list 'assign-free n next)))
                                        (lambda (n) (compile x e s (list 'assign-global n next))))]
                      [define (var x)
                        (compile x e s (list 'define var next))]
                      [call/cc (x)
                               (let ([c (list 'conti
                                              (list 'argument
                                                    (compile x e s
                                                             (if (tail? next)
                                                               (list 'shift 1 (cadr next) '(apply 1))
                                                               '(apply 1)))))])
                                 (if (tail? next)
                                       c
                                       (list 'frame next c)))]
                      [define-macro (name closure)
                                    (def-traditional-macro name
                                               (eval closure null-environment))
                                    '(halt)]
                      [else
                        (let loop (;[func (car x)]
                                   [args (cdr x)]
                                   [c (compile (car x) e s
                                               (if (tail? next)
                                                 (list 'shift (length (cdr x)) (cadr next)
                                                       (list 'apply (length (cdr x))))
                                                 (list 'apply (length (cdr x)))))])
                          (if (null? args)
                            (if (tail? next)
                              c
                              (list 'frame next c))
                              ; (list 'frame c next))
                            (loop ;(car args)
                              (cdr args)
                              (compile (car args) e s (list 'argument c)))))])]

        [else (list 'constsym x next)]))
