;;; Chap 4.7 Stack Base Scheme Interpreter

;; (rec sum (lambda (x) (if (= x 0) 0 (+ x (sum (- x 1))))))
(define-syntax rec (syntax-rules ()
                                 ((_ a b) (let ((a '()))
                                            (set! a b)))))

;; (recur count ((x '(a b c d e))) (if (null? x) 0 (+ (count (cdr x)) 1)))
(define-syntax recur
  (syntax-rules ()
                ((_ f ((v i) ...) e ...)
                 ((rec f (lambda (v ...) e ...))
                  i ...))))

;; (record (x y z) '(1 2 3) (+ x y z))
;(define-syntax record
;  (syntax-rules ()
;                ((_ (var ...) val exp ...)
;                 (apply (lambda (var ...) exp ...) val))))
(define-syntax record
  (syntax-rules ()
                ((_ args parm exps ...)
                 (apply (lambda args exps ...) parm))))

;; (record-case '(hello 1 2 3)
;;              (hello (x y z)
;;                     (+ x y z))
;;              (bye   (x y z)
;;                     (- x y z)))
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

;; (extend
;;   '((a b c) (u v))
;;   '(x y z))
;;
;; => ((x y z) (a b c) (u v))
(define extend
  (lambda (env rib)
    (cons rib env)))

(define closure
  (lambda (body n s)
    (let ((v (make-vector (+ n 1))))
      (vector-set! v 0 body)
      (recur f ((i 0))
             (unless (= i n)
               (vector-set! v (+ i 1) (index s i))
               (f (+ i 1))))
      v)))

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

;; get free variables in closure
(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

;; create continuation(closure object)
(define continuation
  (lambda (s)
    (closure
      (list 'refer-local
            0
            (list 'nuate  (save-stack s) (list 'return 0)))
      0
      '())))

(define set-member?
  (lambda (x s)
    (cond
      ((null? s) #f)
      ((eq? x (car s)) #t)
      (else (set-member? x (cdr s))))))

(define set-cons
  (lambda (x s)
    (if (set-member? x s)
      s
      (cons x s))))

(define set-union
  (lambda (s1 s2)
    (if (null? s1)
      s2
      (set-union (cdr s1)
                 (set-cons (car s1) s2)))))

(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
        (cons (car s1) (set-intersect (cdr s1) s2))
        (set-intersect (cdr s1) s2)))))

(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
      '()
      (if (set-member? (car s1) s2)
        (cons (car s1) (set-intersect (cdr s1) s2))
        (set-intersect (cdr s1) s2)))))

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
                      [lambda (vars body)
                        (find-free body (set-union vars b) e)]
                      [if (test then else)
                        (set-union (find-free test b e)
                                   (set-union (find-free then b e)
                                              (find-free else b e)))]
                      [set! (var exp)
                        (if (set-member? var b)
                          (find-free exp b e)
                          (set-cons var (find-free exp b e)))]
                      [call/cc (exp)
                               (find-free exp b e)]
                      [else
                        (let next ([x x])
                          (if (null? x)
                            '()
                            (set-union (find-free (car x) b e)
                                       (next (cdr x)))))])]
        [else '()]))

(display  (find-free
  '((lambda (x y) z) 10 20)
  '(x y)                         ; 束縛変数
  '(() . (x y z))                ; (ローカル変数リスト . 自由変数リスト)
  ))

;; set!により束縛される可能性のある変数
;; (find-sets '(lambda (x y) (set! x z))
;;            '(x y z)) ; 対象の変数
;;
;; => (x)
(define find-sets
  (lambda (x v)
    (cond
      ((symbol? x)
       '())
      ((pair? x)
       (record-case x
                    (quote (obj)
                           '())
                    (lambda (vars body)
                      (find-sets body (set-minus v vars)))
                    (if (test then else)
                      (set-union (find-sets test v)
                                 (set-union (find-sets then v)
                                            (find-sets else v))))
                    (set! (var x)
                      (set-union (if (set-member? var v)
                                   (list var)
                                   '())
                                 (find-sets x v)))
                    (call/cc (exp)
                             (find-sets exp v))
                    (else
                      (recur next ((x x))
                             (if (null? x)
                               '()
                               (set-union (find-sets (car x) v)
                                          (next (cdr x))))))))
       (else
         '()))))

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

(display (compile-lookup
  'y
  '(() .  (z w))
  (lambda (n) (list 'refer-local n ))
  (lambda (n) (list 'refer-free  n ))
  (lambda (n) (list 'refer-global  n ))
))

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

(display (compile-refer
  'y
  '(() .  (z w))
  '(halt)
  ))

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

(define (compile-lambda e s next vars bodies)
  (let ([free (find-free (car bodies) vars e)]
        [sets (find-sets (car bodies) vars)])
    (collect-free free e
                  (list 'close
                        (length free)
                        ; closure生成時に引数に大してboxを作る
                        (make-boxes sets vars
                                    (compile (car bodies)
                                             (cons vars free)
                                             (set-union
                                               sets
                                               (set-intersect s free))
                                             (list 'return (length vars))))
                        next))))

;; e = (ローカル変数のリスト . 自由変数のリスト)
;; s = (set!される自由変数のリスト)
(define (compile x e s next)
  (cond [(symbol? x)
         (compile-refer x e
                        (if (set-member? x s)
                          (list 'indirect next)
                          next))]
        [(pair? x)
         (record-case x
                      [quote (obj) (list 'constant obj next)]
                      [lambda (vars . bodies)
                        (compile-lambda e s next vars bodies)]
                      [if (test then else)
                        (let ([thenc (compile then e s next)]
                              [elsec (compile else e s next)])
                          (compile test e s (list 'test thenc elsec)))]
                      [set! (var x)
                        (compile-lookup var e
                                        (lambda (n)
                                          (compile x e s (list 'assign-local n next)))
                                        (lambda (n)
                                          (compile x e s (list 'assign-free n next)))
                                        (lambda (n)
                                          (compile x e s (list 'assign-global n next))))]
                      [define (var x)
                        (compile x e s (list 'define var next))]
                      [call/cc (x)
                               (let ((c (list 'conti
                                              (list 'argument
                                                    (compile x e s
                                                             (if (tail? next)
                                                               (list 'shift
                                                                     1
                                                                     (cadr next)
                                                                     '(apply))
                                                               '(apply)))))))
                                     (if (tail? next)
                                       c
                                       (list 'frame next c)))]
                      [else
                        (let loop (;[func (car x)]
                                   [args (cdr x)]
                                   [c (compile (car x) e s
                                               (if (tail? next)
                                                 (list 'shift (length (cdr x)) (cadr next) '(apply))
                                                 '(apply)))])
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

        [else (list 'constant x next)]))


(define *stack*
  (make-vector 100))

;; (set! s (push 1 s))
(define push
  (lambda (x s)
    (vector-set! *stack* s x)
    (+ s 1)))

;; (index s 0)
(define index
  (lambda (s i)
    (vector-ref *stack* (- (- s i) 1))))

;; (index-set! s 0 99)
(define index-set!
  (lambda (s i v)
    (vector-set! *stack* (- (- s i) 1) v)))

(define find-link
  (lambda (n e)
    (if (= n 0)
      e
      (find-link (- n 1) (index e -1)))))

;; スタックsの複製を作る
(define save-stack
  (lambda (s)
    (let ((v (make-vector s)))
      (recur copy ((i 0))
             (unless (= i s)
               (vector-set! v i (vector-ref *stack* i))
               (copy (+ i 1))))
      v)))

;; 継続オブジェクトからスタックを復元する
(define restore-stack
  (lambda (v)
    (let ((s (vector-length v)))
      (recur copy ((i 0))
             (unless (= i s)
               (vector-set! *stack* i (vector-ref v i))
               (copy (+ i 1))))
      s)))

(define (display-register a x f c s)
    (newline)
    (display "a=") (display a) (newline)
    (display "x=") (display x) (newline)
    (display "f=") (display f) (newline)
    (display "c=") (display c) (newline)
    (display "s=") (display s) (newline)
    (display "---bottom---\n")
    (map (lambda (x) (if (undefined? x)
                       '()
                       (begin
                         (display x)
                         (newline))))
         (vector->list *stack*))
    (display "--- top ----\n")
    (newline))

(define (box x)
    (cons 'box x))
(define (unbox x)
    (cdr x))
(define (set-box! b x)
    (set-cdr! b x))

;; スタックトップのn個をmだけ下にシフトする
(define (shift-args n m s)
  (let next-arg ((i (- n 1)))
  (unless (< i 0)
  (index-set! s (+ i m) (index s i))
  (next-arg (- i 1))))
    (- s m))

(define *global*
  (list '(x . 123)
        '(y . 256)
         (cons '= #((equal (return 2))))
         (cons '- #((minus (return 2))))
         (cons '+ #((plus  (return 2))))))

;; (refer-global 'x)
;;
;; => 123
(define (refer-global k)
  (cdr (assq k *global*)))

;; (assign-global 'x 800)
(define (assign-global k v)
  (set-cdr! (assq k *global*) v))

(define (define-global k v)
  (set! *global*
    (cons (cons k v) *global*)))

(define (VM a x f c s)
  ;(display-register a x f c s)
  (record-case x
               [halt () a]
               [refer-local (n x)
                            (VM (index f n) x f c s)]
               [refer-free (n x)
                           (VM (index-closure c n) x f c s)]
               [refer-global (n x)
                           (VM (refer-global n) x f c s)]
               [indirect (x)
                         (VM (unbox a) x f c s)]
               [constant (obj x)
                         (VM obj x f c s)]
               [close (n body x)
                      (VM (closure body n s) x f c (- s n))]
               [box (n x)
                    (index-set! s n (box (index s n)))
                    (VM a x f c s)]
               [test (then else)
                     (VM a (if a then else) f c s)]
               [plus (x)
                     (let ((a (index f 0))
                           (b (index f 1)))
                       (VM (+ a b) x f c s))]
               [minus (x)
                     (let ((a (index f 0))
                           (b (index f 1)))
                       (VM (- a b) x f c s))]
               [equal (x)
                     (let ((a (index f 0))
                           (b (index f 1)))
                       (VM (= a b) x f c s))]
               [assign-local (n x)
                             (set-box! (index f n) a)
                             (VM a x f c s)]
               [assign-free (n x)
                            (set-box! (index-closure c n) a)
                            (VM a x f c s)]
               [assign-global (n x)
                              (assign-global n a)
                              (VM a x f c s)]
               [define (n x)
                              (define-global n a)
                              (VM a x f c s)]
               [conti (x)
                      (VM (continuation s) x f c s)]
               [nuate (stack x)
                      (VM a x f c (restore-stack stack))]
               [frame (ret x)
                      (VM a x f c (push ret (push f (push c s))))]
               [argument (x)
                         (VM a x f c (push a s))]
               [shift (n m x)
                      (VM a x f c (shift-args n m s))]
               [apply ()
                      (VM a (closure-body a) s a s)]
               [return (n)
                       (let1 s (- s n)
                             (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]))

(define evaluate
  (lambda (x)
    (VM '() (compile x '(() . ()) '() '(halt)) 0 '() 0)))

(define debug
  (lambda (code)
    (let ((opecode (compile code '(() . ()) '() '(halt))))
      (display opecode)
      (newline)
      (display (VM '() opecode 0 '() 0))
      (newline))))

(debug '((lambda (x y) y) 1 2))
(debug '(call/cc (lambda (k)  (if (k #f) 10 20))))
(debug '(quote hello))
(debug '((lambda (x) x) 3))
(debug '(if #t 5 0))
(debug '(((call/cc (lambda (c) c)) (lambda (x) x)) 11))
(debug '((lambda (f x) (f x)) (lambda (x) x) 13))
(debug 17)
(debug '((lambda (x)
           ((lambda (y) x)
            (set! x 19)))
         29))
(debug '((lambda (x) y) 10))
(debug '(set! y 100) )
(debug 'y)
(debug '+)
(debug '(+ 1 2))
(debug '(define func
          (lambda (x y z)
            (+ x (+ y z)))))
(debug '(func 1 2 3))
(debug '(= 1 2))

(debug '(define sum
          (lambda (n s)
            (if (= n 0)
              s
              (sum (- n 1) (+ s n))))))
(debug '(sum 10 0))
(debug '((lambda (x)
           (+ x x))
         11))
