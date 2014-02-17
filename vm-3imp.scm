;;; Virtual Machine for 3imp format
;;;
;;;
;;; Compilierが吐いた3imp VM用コードを実行する

(load "./util.scm")

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
    (let ([s (vector-length v)])
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
  (recur next-arg ([i (- n 1)])
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
                     (let ([a (index f 0)]
                           [b (index f 1)])
                       (VM (+ a b) x f c s))]
               [minus (x)
                     (let ([a (index f 0)]
                           [b (index f 1)])
                       (VM (- a b) x f c s))]
               [equal (x)
                     (let ([a (index f 0)]
                           [b (index f 1)])
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
