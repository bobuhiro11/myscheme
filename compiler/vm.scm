;;; Virtual Machine for linear format
;;;
;;;
;;; execute linaer formatted bytecode

(add-load-path "." :relative)
(load "util.scm")
(load "linear.scm")

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

;; duplicate stack
(define save-stack
  (lambda (s)
    (let ((v (make-vector s)))
      (recur copy ((i 0))
             (unless (= i s)
               (vector-set! v i (vector-ref *stack* i))
               (copy (+ i 1))))
      v)))

;; restore stack from stack object save by continuation
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

(define (shift-args n m s)
  (recur next-arg ([i (- n 1)])
         (unless (< i 0)
           (index-set! s (+ i m) (index s i))
           (next-arg (- i 1))))
  (- s m))

(define *global*
  (list '(x     . 123)
        '(y     . 256)
        '(=     . #(1000))
        '(-     . #(1003))
        '(+     . #(1006))
        '(>     . #(1009))
        '(<     . #(1012))
        '(cons  . #(1015))
        '(car   . #(1018))
        '(cdr   . #(1021))
        '(null? . #(1024))
        '(*     . #(1027))
        '(/     . #(1030))))

;; (refer-global 'x)
;;
;; => 123
(define (refer-global k)
  (cdr (assq k *global*)))

;; (assign-global 'x 800)
(define (assign-global k v rom-code)
  (set-cdr! (assq k *global*)
            (if (vector? v)
              (save-closure-body v rom-code)   ; save code in ram if closure object
              v)))

(define (define-global k v rom-code)
  (set! *global*
    (cons (cons k
                (if (vector? v)
                  (save-closure-body v rom-code)   ; save code in ram if closure object
                  v))
          *global*)))

(define 1+
  (lambda (x)
    (+ x 1)))

(define 2+
  (lambda (x)
    (+ x 2)))

(define 3+
  (lambda (x)
    (+ x 3)))

(define 4+
  (lambda (x)
    (+ x 4)))

(define 1-
  (lambda (x)
    (- x 1)))

;; return falsec if "lst" don't have "i"
(define lst-index
  (lambda (lst i)
    (if (null? lst)
      #f
      (if (= i (caar lst))
        (car lst)
        (lst-index (cdr lst) i)))))


;; code pool for closure and embed function
(define *ram-code*
  '((#x3E8 equal)
    (#x3E9 return)
    (#x3EA 2)
    (#x3EB minus)
    (#x3EC return)
    (#x3ED 2)
    (#x3EE plus)
    (#x3EF return)
    (#x3F0 2)
    (#x3F1 gt)
    (#x3F2 return)
    (#x3F3 2)
    (#x3F4 lt)
    (#x3F5 return)
    (#x3F6 2)
    (#x3F7 cons)
    (#x3F8 return)
    (#x3F9 2)
    (#x3Fa car)
    (#x3Fb return)
    (#x3Fc 1)
    (#x3Fd cdr)
    (#x3Fe return)
    (#x3Ff 1)
    (#x400 null?)
    (#x401 return)
    (#x402 1)
    (#x403 mul)
    (#x404 return)
    (#x405 2)
    (#x406 div)
    (#x407 return)
    (#x408 2)))

;; get code from address
(define code-index
  (lambda (rom-code i)
    (let ([code (lst-index rom-code i)])
      (if code
        code
        (lst-index *ram-code* i)))))

;; get second element of code-index
(define code-index-content
  (lambda (rom-code i)
    (cadr (code-index rom-code i))))

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

;; copy from n to m of ROM code to RAM
;; return start address of RAM
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
                                      [close ()
                                             (let ([bodyadr  (cadr (lst-index rom-code (+ i 2)))]
                                                   [ebodyadr (cadr (lst-index rom-code (+ i 3)))])
                                               (when (< bodyadr 1000)
                                                 (set! rom-code
                                                   (cons (list (+ i 2) (+ bodyadr adr-diff))
                                                         (cons (list (+ i 3) (+ ebodyadr adr-diff))
                                                               rom-code))))
                                               (cons (+ i adr-diff) (cdr x)))]
                                      [test ()
                                             (let ([elsadr  (cadr (lst-index rom-code (+ i 1)))])
                                               (when (< elsadr 1000)
                                                 (set! rom-code
                                                   (cons (list (+ i 1) (+ elsadr adr-diff))
                                                         rom-code)))
                                               (cons (+ i adr-diff) (cdr x)))]
                                      [frame ()
                                             (let ([retadr  (cadr (lst-index rom-code (+ i 1)))])
                                               (when (< retadr 1000)
                                                 (set! rom-code
                                                   (cons (list (+ i 1) (+ retadr adr-diff))
                                                         rom-code)))
                                               (cons (+ i adr-diff) (cdr x)))]
                                      [else
                                        (cons (+ i adr-diff) (cdr x))])
                         *ram-code*))
                 (next (1+ i))))))))

;; return the smallest return address from stack
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

;; add offset to return address in stack vector
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


;; return new closure object
;;
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
      (let ([ins (code-index *ram-code* (2+ newn))]
            [ins2 (code-index *ram-code* (3+ newn))])
        ; Be carefull return address in stack frame
        ; if the closure is for continuation
        (when (eq? (cadr ins) 'nuate)
          (cons (list (3+ newn)
                      (addoffset (cadr ins2)
                                 (- (move-ram (retadr-from-stack (cadr ins2))
                                              (last-address rom-code)
                                              rom-code)
                                    (retadr-from-stack (cadr ins2)))))
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

; add continuation body to last ROM
; (display (continuation 3 '( (2 hoge) (3 hage))))
(define continuation
  (lambda (s rom-code)
    (let ([start-adr (1+ (last-address rom-code))])
      (append (list (list start-adr        'refer-local)
                    (list (+ start-adr 1)  0)
                    (list (+ start-adr 2)  'nuate)
                    (list (+ start-adr 3)  (save-stack s))
                    (list (+ start-adr 4)  'return)
                    (list (+ start-adr 5)  0))
              rom-code))))

;;
;; a:    accumlator
;; pc:   program counter
;; f:    frame pointer
;; argp: arg pointer
;; c:    closure pointer
;; s:    stack pointer
;; code: code
;;
(define (VM a pc f argp c s code)
  ;(display-register a pc f argp c s)
  ;(display "ram=")
  ;(display *ram-code*)
  ;(newline)
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
  (record-case ;(cdr (code-index code pc))
               (let ([x (code-index code pc)])
                 (unless x
                   (display (format #f "cannot find code adr = ~D" pc)))
                 (cdr x))
               [halt () a]
               [refer-local ()
                            (let1 n (code-index-content code (1+ pc))
                                  (VM (index argp n) (2+ pc) f argp c s code))]
               [refer-free ()
                           (let1 n (code-index-content code (1+ pc))
                                 (VM (index-closure c n) (2+ pc) f argp c s code))]
               [refer-global ()
                             (let1 n (code-index-content code (1+ pc))
                                   (VM (refer-global n) (2+ pc) f argp c s code))]
               [indirect ()
                         (VM (unbox a) (1+ pc) f argp c s code)]
               [conststr ()
                         (let1 obj (code-index-content code (1+ pc))
                               (VM obj (2+ pc) f argp c s code))]
               [constnum ()
                         (let1 obj (code-index-content code (1+ pc))
                               (VM obj (2+ pc) f argp c s code))]
               [constsym ()
                         (let1 obj (code-index-content code (1+ pc))
                               (VM obj (2+ pc) f argp c s code))]
               [constnil ()
                         (let1 obj (code-index-content code (1+ pc))
                               (VM obj (2+ pc) f argp c s code))]
               [constboo ()
                         (let1 obj (code-index-content code (1+ pc))
                               (VM obj (2+ pc) f argp c s code))]
               [close ()
                      (let ([n        (code-index-content code (1+ pc))]
                            [bodyadr  (code-index-content code (2+ pc))]
                            [ebodyadr (code-index-content code (3+ pc))])
                        (VM (closure bodyadr ebodyadr n s) (4+ pc) f argp c (- s n) code))]
               [box ()
                    (let1 n (code-index-content code (1+ pc))
                          (index-set! s n (box (index s n)))
                          (VM a (2+ pc) f argp c s code))]
               [test ()
                     (let1 elsadr (code-index-content code (1+ pc))
                           (VM a (if a (2+ pc) elsadr) f argp c s code))]
               [plus ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (+ a b) (1+ pc) f argp c s code))]
               [minus ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (- a b) (1+ pc) f argp c s code))]
               [mul ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (* a b) (1+ pc) f argp c s code))]
               [div ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (/ a b) (1+ pc) f argp c s code))]
               [gt ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (> a b) (1+ pc) f argp c s code))]
               [lt ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (< a b) (1+ pc) f argp c s code))]
               [cons ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (cons a b) (1+ pc) f argp c s code))]
               [car ()
                     (let ([a (index argp 0)])
                       (VM (car a) (1+ pc) f argp c s code))]
               [cdr ()
                     (let ([a (index argp 0)])
                       (VM (cdr a) (1+ pc) f argp c s code))]
               [null? ()
                     (let ([a (index argp 0)])
                       (VM (null? a) (1+ pc) f argp c s code))]
               [display ()
                        (display a)
                        (VM a  (1+ pc) f argp c s code)]
               [newline ()
                        (newline)
                        (VM a  (1+ pc) f argp c s code)]
               [equal ()
                     (let ([a (index argp 0)]
                           [b (index argp 1)])
                       (VM (= a b) (1+ pc) f argp c s code))]
               [assign-local ()
                             (let1 n (code-index-content code (1+ pc))
                                   (set-box! (index argp n) a)
                                   (VM a (2+ pc) f argp c s code))]
               [assign-free ()
                            (let1 n (code-index-content code (1+ pc))
                                  (set-box! (index-closure c n) a)
                                  (VM a (2+ pc) f argp c s code))]
               [assign-global ()
                              (let1 n (code-index-content code (1+ pc))
                                    (assign-global n a code)
                                    (VM a (2+ pc) f argp c s code))]
               [define ()
                 (let1 n (code-index-content code (1+ pc))
                       (define-global n a code)
                       (VM a (2+ pc) f argp c s code))]
               [conti ()
                      (set! a  (closure
                                 (+ 1 (last-address code)) ; depend on continuation instruction
                                 (+ 6 (last-address code))
                                 0
                                 s))
                      (set! code (continuation s code))
                      (VM a (1+ pc) f argp c s code)]
               [nuate ()
                      (let1 stack (code-index-content code (1+ pc))
                            (VM a (2+ pc) f argp c (restore-stack stack) code))]
               [frame ()
                      (let1 retadr (code-index-content code (1+ pc))
                            (VM a (2+ pc) f argp c
                                (push f (push argp (push c (push retadr (push 'end-of-frame s)))))
                                code))]
               [argument ()
                         (VM a (1+ pc) f argp c (push a s) code)]
               [shift ()
                      (let ([n (code-index-content code (1+ pc))]
                            [m (code-index-content code (2+ pc))])
                        (VM a (3+ pc) f (+ argp (- n m)) c (shift-args n m s) code))]
               [apply ()
                      (let1 n (code-index-content code (1+ pc))
                            (VM a (closure-body a) (- s n) s a s code))]
               [return ()
                       (let* ([n (code-index-content code (1+ pc))]
                              [s (- s n)])
                         (VM a (index s 3) (index s 0) (index s 1) (index s 2) (- s 5) code))]
               [else ()
                     (display 'unknown-instruction)
                     (newline)
                     (display 'pc=)
                     (display pc)]))
