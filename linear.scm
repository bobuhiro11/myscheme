;;; Linear
;;;
;;; Compilierが吐いた3imp VM用コードを線形なフォーマットの言語へ変換

(load "./util.scm")

(define 3imp->linear
  (lambda (code adr)
    (record-case code
                 [halt ()
                       (list (list adr 'halt))]
                 [refer-local (n x)
                              (cons (list adr 'refer-local n)
                                    (3imp->linear x (+ adr 1)))]
                 [refer-free (n x)
                              (cons (list adr 'refer-free n)
                                    (3imp->linear x (+ adr 1)))]
                 [refer-global (n x)
                              (cons (list adr 'refer-global n)
                                    (3imp->linear x (+ adr 1)))]
                 [indirect (x)
                              (cons (list adr 'indirect)
                                    (3imp->linear x (+ adr 1)))]
                 [constant (obj x)
                              (cons (list adr 'constant obj)
                                    (3imp->linear x (+ adr 1)))]
                 [close (n body x)
                        (let* ([nextc (3imp->linear x (+ adr 1))]
                               [bodyadr (+ 1 adr (length nextc))]
                               [bodyc (3imp->linear body bodyadr)])
                          (cons (list adr 'close n bodyadr)
                                (append nextc bodyc)))]
                 [box (n x)
                              (cons (list adr 'box n)
                                    (3imp->linear x (+ adr 1)))]
                 [test (then else)
                       (let* ([thenc (3imp->linear then (+ adr 1))]
                              [elsadr (+ 1 adr (length thenc))]
                              [elsec (3imp->linear else elsadr)])
                         (cons (list adr 'test elsadr)
                               (append thenc elsec)))]
                 [plus (x)
                       (cons (list adr 'plus)
                             (3imp->linear x (+ adr 1)))]
                 [minus (x)
                       (cons (list adr 'minus)
                             (3imp->linear x (+ adr 1)))]
                 [equal (x)
                       (cons (list adr 'equal)
                             (3imp->linear x (+ adr 1)))]
                 [assign-local (n x)
                       (cons (list adr 'assign-local n)
                             (3imp->linear x (+ adr 1)))]
                 [assign-free (n x)
                       (cons (list adr 'assign-free n)
                             (3imp->linear x (+ adr 1)))]
                 [assign-global (n x)
                       (cons (list adr 'assign-global n)
                             (3imp->linear x (+ adr 1)))]
                 [define (n x)
                       (cons (list adr 'define n)
                             (3imp->linear x (+ adr 1)))]
                 [conti (x)
                       (cons (list adr 'conti )
                             (3imp->linear x (+ adr 1)))]
                 [nuate (stack x)
                       (cons (list adr 'nuate stack)
                             (3imp->linear x (+ adr 1)))]
                 [frame (ret x)
                        (let* ([xcode (3imp->linear x (+ adr 1))]
                               [retadr (+ 1 adr (length xcode))]
                               [retcode (3imp->linear ret retadr)])
                          (cons (list adr 'frame retadr)
                                (append xcode retcode)))]
                 [argument (x)
                       (cons (list adr 'argument)
                             (3imp->linear x (+ adr 1)))]
                 [shift (n m x)
                       (cons (list adr 'shift n m)
                             (3imp->linear x (+ adr 1)))]
                 [apply ()
                       (list (list adr 'apply ))]
                 [return (n)
                         (list (list adr 'return n))])))

;; 3imp->linearの結果を整形・出力
(define display-linearcode
  (lambda (ccode)
    (if (null? ccode)
      '()
      (begin
        (map (lambda (x) (display x) (display " "))(car ccode))
        (newline)
        (display-linearcode (cdr ccode))))))

;(display-linearcode
;  (3imp->linear
;    '(frame (halt)
;            (conti
;              (argument
;                (close 0
;                       (frame (test (constant 10 (return 1)) (constant 20 (return 1)))
;                              (constant #f
;                                        (argument
;                                          (refer-local 0
;                                                       (apply)))))
;                       (apply)))))
;    0))
;(newline)
