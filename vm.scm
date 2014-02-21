;;; Virtual Machine for linear format
;;;
;;;
;;; Compilierが吐きlinaerフォーマット変換後コードを実行する

(load "./util.scm")
(load "./linear.scm")

(define closure
  (lambda (bodyadr ebodyadr n s)
    (let ((v (make-vector (+ n 2))))
      (vector-set! v 0 bodyadr)
      (vector-set! v 1 ebodyadr)
      (recur f ((i 0))
             (unless (= i n)
               (vector-set! v (+ i 2) (index s i))
               (f (+ i 1))))
      v)))

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

;; get free variables in closure
(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 2))))


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

;(define find-link
;  (lambda (n e)
;    (if (= n 0)
;      e
;      (find-link (- n 1) (index e -1)))))

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
    (let ([s (vector-length v)])
      (recur copy ((i 0))
             (unless (= i s)
               (vector-set! *stack* i (vector-ref v i))
               (copy (+ i 1))))
      s)))

(define (display-register a pc f argp c s)
  (newline)
    (display "a=") (display a) (newline)
    (display "pc=") (display pc) (newline)
    (display "f=") (display f) (newline)
    (display "argp=") (display argp) (newline)
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
  (recur next-arg ([i (- n 1)])
         (unless (< i 0)
           (index-set! s (+ i m) (index s i))
           (next-arg (- i 1))))
  (- s m))

(define *global*
  (list '(x . 123)
        '(y . 256)
        '(= . #(1000))
        '(- . #(1002))
        '(+ . #(1004))))

;; (refer-global 'x)
;;
;; => 123
(define (refer-global k)
  (cdr (assq k *global*)))

;; (assign-global 'x 800)
(define (assign-global k v rom-code)
  (set-cdr! (assq k *global*)
            (if (vector? v)
              (save-closure-body v rom-code)   ; closureの場合は本体の命令コードをramに保存する．
              v)))

(define (define-global k v rom-code)
  (set! *global*
    (cons (cons k
                (if (vector? v)
                  (save-closure-body v rom-code)   ; closureの場合は本体の命令コードをramに保存する．
                  v))
          *global*)))

(define 1+
  (lambda (x)
    (+ x 1)))

(define 1-
  (lambda (x)
    (- x 1)))

(define lst-index 
  (lambda (lst i)
    (if (null? lst)
      #f
      (if (= i (caar lst))
        (car lst)
        (lst-index (cdr lst) i)))))

;; closureや組み込み関数のための命令コード領域
;; 普通の命令とはアドレス領域を区別する．
(define *ram-code*
  (list 
    '(1000 equal)
    '(1001 return 2)
    '(1002 minus)
    '(1003 return 2)
    '(1004 plus)
    '(1005 return 2)))

;; アドレスからコードを得る
(define code-index
  (lambda (rom-code i)
    (let ([code (lst-index rom-code i)])
      (if code
        code
        (lst-index *ram-code* i)))))

;; 最後のアドレス
(define last-address
  (lambda (code)
    (recur next ([code code]
                 [max 0])
           (if (null? code)
             max
             (next (cdr code)
                   (if (> max (caar code))
                     max
                     (caar code)))))))

;; ROMのn番地からm番地までを，RAMにコピーする．
;; 返値はRAM上での開始アドレス
;;
;; (move-ram
;;   3
;;   5
;;   '( (2 biz) (3 hoge) (4 foo) (5 bar) (6 baz))
;;   )
;;
(define move-ram
  (lambda (n m rom-code)
    (let ([adr-diff (- (1+ (last-address *ram-code*)) n)])
      (recur next ([i n])
             (if (> i m)
               (+ n adr-diff) ; RAM上での開始アドレス
               (let ([x (lst-index rom-code i)])
                 (set! *ram-code*
                   (cons (record-case (cdr x)
                                      ;本体がRAMに存在していない場合は変換が必要
                                      [close (n bodyadr ebodyadr)
                                             (if (> bodyadr 1000)
                                               (cons (+ i adr-diff)
                                                     (cdr x))
                                               (cons (+ i adr-diff)
                                                     (list 'close
                                                           n
                                                           (+ bodyadr adr-diff)
                                                           (+ ebodyadr adr-diff))))]
                                      [test (elsadr)
                                            (if (> elsadr 1000)
                                               (cons (+ i adr-diff)
                                                     (cdr x))
                                               (cons (+ i adr-diff)
                                                     (list 'test
                                                           (+ elsadr adr-diff))))]
                                      [frame (retadr)
                                             (if (> retadr 1000)
                                               (cons (+ i adr-diff)
                                                     (cdr x))
                                               (cons (+ i adr-diff)
                                                     (list 'frame
                                                           (+ retadr adr-diff))))]
                                      [else
                                        (cons (+ i adr-diff) (cdr x))])
                         *ram-code*))
                 (next (1+ i))))))))

;; スタックvector から，最小の戻りアドレスを探す
;;
;; #(end-of-frame 18 () 0 0 end-of-frame 13 () 0 0))
;; 
;; => 13
(define retadr-from-stack
  (lambda (stack)
    (recur next ([i (- (vector-length stack) 1)])
           (if (eq? (vector-ref stack i) 'end-of-frame)
             (vector-ref stack (1+ i))
             (next (- i 1))))))

;; スタックvector の中の戻りアドレスにoffsetを加える
;;
;; (addoffset
;;   '#(end-of-frame 18 () 0 0 end-of-frame 13 () 0 0)
;;   100)
;;
;;  =>
;; #(end-of-frame 118 () 0 0 end-of-frame 113 () 0 0)
(define addoffset
  (lambda (stack offset)
    (recur next ([i (- (vector-length stack) 1)])
           (if (< i 0)
             stack 
             (begin 
               (when (eq? (vector-ref stack i) 'end-of-frame)
                 (vector-set! stack
                              (1+ i)
                              (+ offset (vector-ref stack (1+ i)))))
               (next (- i 1)))))))


;; クロージャvをグローバルな空間に配置する場合は，
;; 本体をRAMに移動する．
;; 返値は，あらたなクロージャオブジェクト
;; (display (save-closure-body
;;            '#(3 5)
;;            '( (2 biz) (3 hoge) (4 foo) (5 bar) (6 baz))))
;; (display *ram-code*)
(define save-closure-body
  (lambda (v rom-code)
    (let* ([n (vector-ref v 0)]
           [m (vector-ref v 1)]
           [newn (move-ram n m rom-code)]
           [newm (+ m (- newn n))])
      (vector-set! v 0 newn)
      (vector-set! v 1 newm)
      (let ([ins (code-index *ram-code* (1+ newn))])
        ; もし対象のクロージャが継続であれば，スタックフレーム内に存在する
        ; 戻りアドレスについても，考慮する必要がある．
        (when (eq? (cadr ins) 'nuate)
          (cons (list (1+ newn)
                      'nuate
                      (addoffset (caddr ins)
                                 (- (move-ram (retadr-from-stack (caddr ins))
                                              (last-address rom-code)
                                              rom-code)
                                    (retadr-from-stack (caddr ins)))))
                *ram-code*)))
      v)))

;; create continuation(closure object)
;(define continuation
;  (lambda (s)
;    (closure
;      (list 'refer-local
;            0
;            (list 'nuate  (save-stack s) (list 'return 0)))
;      0
;      '())))

; rom-codeの最後に継続本体を追加する
; (display (continuation 3 '( (2 hoge) (3 hage))))
(define continuation
  (lambda (s rom-code)
    (let ([start-adr (1+ (last-address rom-code))])
      (append (list (list start-adr        'refer-local 0)
                    (list (+ start-adr 1)  'nuate (save-stack s))
                    (list (+ start-adr 2)  'return 0))
              rom-code))))

;;
;; a: accumlator
;; pc: プログラムカウンタ
;; f: フレームポインタ
;; argp: 引数ポインタ
;; c: 現在のクロージャ
;; s: スタックポインタ
;; code: コード
;;
(define (VM a pc f argp c s code)
  ;(display-register a pc f argp c s)
  ;(display code)
  ;(newline)
  ;(display "pc=")
  ;(display pc)
  ;(display "x=")
  ;(display (code-index code pc))
  ;(display "a=")
  ;(display a)
  ;(display "code=")
  ;(if (= pc 18)
  ;  (display code)
  ;  '())
  ;(newline)
  ;(display *stack*)
  ;(newline)
  ;(display "f=") (display f) (newline)
  ;(display "argp=") (display argp) (newline)
  (record-case ;(cdr (code-index code pc)) ; 先頭にはアドレスがある
               (let ([x (code-index code pc)])
                 (cdr x))
               [halt () a]
               [refer-local (n)
                            (VM (index argp n) (1+ pc) f argp c s code)]
               [refer-free (n)
                           (VM (index-closure c n) (1+ pc) f argp c s code)]
               [refer-global (n)
                           (VM (refer-global n) (1+ pc) f argp c s code)]
               [indirect ()
                         (VM (unbox a) (1+ pc) f argp c s code)]
               [constant (obj)
                         (VM obj (1+ pc) f argp c s code)]
               [close (n bodyadr ebodyadr)
                      (VM (closure bodyadr ebodyadr n s) (1+ pc) f argp c (- s n) code)]
               [box (n)
                    (index-set! s n (box (index s n)))
                    (VM a (1+ pc) f argp c s code)]
               [test (elsadr)
                     (VM a (if a (1+ pc) elsadr) f argp c s code)]
               [plus ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (+ a b) (1+ pc) f argp c s code))]
               [minus ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (- a b) (1+ pc) f argp c s code))]
               [equal ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (= a b) (1+ pc) f argp c s code))]
               [assign-local (n)
                             (set-box! (index argp n) a)
                             (VM a (1+ pc) f argp c s code)]
               [assign-free (n)
                            (set-box! (index-closure c n) a)
                            (VM a (1+ pc) f argp c s code)]
               [assign-global (n)
                              (assign-global n a code)
                              (VM a (1+ pc) f argp c s code)]
               [define (n)
                              (define-global n a code)
                              (VM a (1+ pc) f argp c s code)]
               [conti ()
                      (set! a  (closure
                                 (+ 1 (last-address code))
                                 (+ 3 (last-address code))
                                 0
                                 s))
                      (set! code (continuation s code))
                      (VM a (1+ pc) f argp c s code)]
               [nuate (stack)
                      (VM a (1+ pc) f argp c (restore-stack stack) code)]
               [frame (retadr)
                      (VM a (1+ pc) f argp c
                          (push f (push argp (push c (push retadr (push 'end-of-frame s))))) code)]
               [argument ()
                         (VM a (1+ pc) f argp c (push a s) code)]
               [shift (n m)
                      (VM a (1+ pc) f (+ argp (- n m)) c (shift-args n m s) code)]
               [apply (n)
                      (VM a (closure-body a) (- s n) s a s code)]
               [return (n)
                       (let1 s (- s n)
                             (VM a (index s 3) (index s 0) (index s 1) (index s 2) (- s 5) code))]))
