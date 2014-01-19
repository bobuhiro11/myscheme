; http://d.hatena.ne.jp/yamanetoshi/20080329/p3より
 (define-syntax rec
   (syntax-rules ()
                 ((_ a b) (let ([a '()])
                            (set! a b)))))
 (define-syntax recur
   (syntax-rules ()
                 ((_ f ([v i] ...) e ...)
                  ((rec f (lambda (v ...) e ...))
                   i ...))))

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

(define compile
  (lambda (x next)
    (cond ((symbol? x)
           (list 'refer x next))
          ((pair? x)
           (cond
            ((eq? 'quote (car x))   (list 'constant (cadr x) next))
            ((eq? 'lambda (car x))  (list 'close (cadr x) (compile (caddr x) '(return)) next))
            ((eq? 'set! (car x))    (compile (caddr x) (list 'assign (cadr x) next)))
            ((eq? 'if (car x))
             (let ((thenc (compile (caddr  x) next))
                   (elsec (compile (cadddr x) next)))
               (compile (cadr x) (list 'test thenc elsec))))
            ((eq? 'call/cc (car x))
             (let ((c (list 'conti (list 'argument (compile (cadr x) '(apply))))))
               (if (tail? next)
                 c
                 (list 'frame next c))))
            (#t
               (recur loop ((args (cdr x))
                            (c (compile (car x) '(apply))))
                      (if (null? args)
                        (if (tail? next)
                          c
                          (list 'frame next c))
                        (loop (cdr args)
                              (compile (car args)
                                       (list 'argument c))))))
            ))
          (#t
            (list 'constant x next)))))

(display (compile 'x '()))
(newline)
(display (compile '(quote abc) '()))
(newline)
(display (compile '(lambda (x) x) '()))
(newline)
(display (compile '(if x 1 2) '()))
(newline)
(display (compile '(set! x 1) '()))
(newline)
(display (compile '(call/cc (lambda (x) 1)) '(return)))
(newline)
(display (compile '(set! func (lambda (x y) 10)) '(return)))
(newline)
(display (compile '(func 1 2) '(n)))
(newline)

(define-syntax record
    (syntax-rules ()
                      ((_ (var ...) val exp ...)
                            (apply (lambda (var ...) exp ...) val))))

(define-syntax record-case
  (syntax-rules (else)
                ((_ exp1 (else exp2 ...))
                 (begin exp2 ...))
                ((_ exp1 (key vars exp2 ...))
                 (let ((r exp1))
                   (if (eq? (car r) 'key)
                     (record vars (cdr r) exp2 ...))))
                ((_ exp1 (key vars exp2 ...) clause ...)
                 (let ((r exp1))
                   (if (eq? (car r) 'key)
                     (record vars (cdr r) exp2 ...)
                     (record-case r clause ...))))))

(define lookup
  (lambda (var e)
    (recur nxtrib ((e e))
           (recur nxtelt ((vars (caar e)) (vals (cdar e)))
                  (cond
                    ((null? vars) (nxtrib (cdr e)))
                    ((eq? (car vars) var) vals)
                    (else (nxtelt (cdr vars) (cdr vals))))))))

(define closure
  (lambda (body e vars)
    (list body e vars)))

(define continuation
  (lambda (s)
    (closure (list 'nuate s 'v) '() '(v))))

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

(define extend
  (lambda (e vars vals)
    (cons (cons vars vals) e)))


; http://uhideyuki.sakura.ne.jp/uDiary/?date=20110823により修正
(define VM
  (lambda (a x e r s)
    (record-case x
                 [halt () a]
                 [refer (var x)
                        (VM (car (lookup var e)) x e r s)]
                 [constant (obj x)
                           (VM obj x e r s)]
                 [close (vars body x)
                        (VM (closure body e vars) x e r s)]
                 [test (then else)
                       (VM a (if a then else) e r s)]
                 [assign (var x)
                         (set-car! (lookup var e) a)
                         (VM a x e r s)]
                 [conti (x)
                        (VM (continuation s) x e r s)]
                 [nuate (s var)
                        (VM (car (lookup var e)) '(return) e r s)]
                 [frame (ret x)
                        (VM a x e '() (call-frame ret e r s))]
                 [argument (x)
                           (VM a x e (cons a r) s)]
                 [apply ()
                        (record (body e vars) a
                                (VM a body (extend e vars r) '() s))]
                 [return ()
                         (record (x e r s) s
                                 (VM a x e r s))])))

(define evaluate
  (lambda (x)
    (VM '() (compile x '(halt)) '() '() '())))

(display (evaluate '(if '() 3 2)))
