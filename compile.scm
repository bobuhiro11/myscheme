;;; Compilier
;;;
;;; マクロ展開後のschemeコードを，3imp VM用コードへ変換

(load "./util.scm")

;; 自由変数(グローバル変数を含まない)を検索
;; (find-free
;;   '((lambda (x y) z) 10 20)
;;   '(x y)                         ; 束縛変数
;;   '(() . (x y z))                ; (ローカル変数リスト . 自由変数リスト)
;;   )
;;
;;   => (z)
(define (find-free x b e)
  (cond [(symbol? x)
         ;(if (set-member? x b)
         (if (and (not (set-member? x b))
                  (or  (set-member? x (car e))
                       (set-member? x (cdr e))))
           (list x) ; 自由変数は，束縛変数でなく環境のどちらかのリストには含まれている
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

;; set!により束縛される可能性のある変数
;; (find-sets '(lambda (x y) (set! x z))
;;            '(x y z)) ; 対象の変数
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

;; closure生成時にその自由変数を集める
;; (collect-free
;;   '(x w)                    ; 自由変数のリスト
;;   '((a b c) . (x y z w))    ; (ローカル変数 . 自由変数)
;;   '(halt))                  ; 次の命令
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
                      ; このどっちかにはあるはず(なければglobal)
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

; lambda式の複数のbodyに対して
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


; lambda式の複数のbodyに対して
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
  (let ([free (find-free-bodies  bodies vars e)] ; lambda式内の自由変数
        [sets (find-sets-bodies  bodies vars)])  ; lambda式内で，varsのうちset!される可能性のある変数
    (collect-free free e
                  (list 'close
                        (length free)
                        ; closure生成時に引数に対してboxを作る
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
;; e = (ローカル変数のリスト . 自由変数のリスト)
;; s = (set!される自由変数のリスト)
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
                          ; (list 'shift (length args) (cadr next) (list 'apply (length args)))
                          ; (list 'apply (length args))))])
                          (if (null? args)
                            (if (tail? next)
                              c
                              (list 'frame next c))
                              ; (list 'frame c next))
                            (loop ;(car args)
                              (cdr args)
                              (compile (car args) e s (list 'argument c)))))])]

        [else (list 'constsym x next)]))
