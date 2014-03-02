;;; Linear
;;;
;;; Compilierが吐いた3imp VM用コードを線形なフォーマットの言語へ変換

(add-load-path "." :relative)
(load "util.scm")


(define 3imp->linear
  (lambda (code adr)
    (record-case code
                 [halt ()
                       (list (list adr 'halt))]
                 [newline (x)
                              (cons (list adr 'newline)
                                    (3imp->linear x (+ adr 1)))]
                 [display (x)
                              (cons (list adr 'display)
                                    (3imp->linear x (+ adr 1)))]
                 [disasm (x)
                              (cons (list adr 'disasm)
                                    (3imp->linear x (+ adr 1)))]
                 [refer-local (n x)
                              (cons (list adr 'refer-local)
                                    (cons (list (+ adr 1) n)
                                          (3imp->linear x (+ adr 2))))]
                 [refer-free (n x)
                              (cons (list adr 'refer-free)
                                    (cons (list (+ adr 1) n)
                                          (3imp->linear x (+ adr 2))))]
                 [refer-global (n x)
                              (cons (list adr 'refer-global)
                                    (cons (list (+ adr 1) n)
                                          (3imp->linear x (+ adr 2))))]
                 [indirect (x)
                              (cons (list adr 'indirect)
                                    (3imp->linear x (+ adr 1)))]
                 [conststr (obj x)
                              (cons (list adr 'conststr)
                                    (cons (list (+ adr 1) obj)
                                          (3imp->linear x (+ adr 2))))]
                 [constnum (obj x)
                              (cons (list adr 'constnum)
                                    (cons (list (+ adr 1) obj)
                                          (3imp->linear x (+ adr 2))))]
                 [constsym (obj x)
                              (cons (list adr 'constsym)
                                    (cons (list (+ adr 1) obj)
                                          (3imp->linear x (+ adr 2))))]
                 [constboo (obj x)
                              (cons (list adr 'constboo)
                                    (cons (list (+ adr 1) obj)
                                          (3imp->linear x (+ adr 2))))]
                 [constnil (obj x)
                              (cons (list adr 'constnil)
                                    (cons (list (+ adr 1) obj)
                                          (3imp->linear x (+ adr 2))))]
                 [close (n body x)
                        (let* ([nextc (3imp->linear x (+ adr 4))]
                               [bodyadr (+ 4 adr (length nextc))]
                               [bodyc (3imp->linear body bodyadr)]
                               [ebodyadr (- (+ bodyadr (length bodyc)) 1)])
                          (cons (list adr 'close)
                                (cons (list (+ adr 1) n)
                                      (cons (list (+ adr 2) bodyadr)
                                            (cons (list (+ adr 3) ebodyadr)
                                                  (append nextc bodyc))))))]
                 [box (n x)
                              (cons (list adr 'box)
                                    (cons (list (+ adr 1) n)
                                          (3imp->linear x (+ adr 2))))]
                 [test (then else)
                       (let* ([thenc (3imp->linear then (+ adr 2))]
                              [elsadr (+ 2 adr (length thenc))]
                              [elsec (3imp->linear else elsadr)])
                         (cons (list adr 'test)
                               (cons (list (+ adr 1) elsadr)
                                     (append thenc elsec))))]
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
                               (cons (list adr 'assign-local)
                                     (cons (list (+ adr 1) n)
                                           (3imp->linear x (+ adr 2))))]
                 [assign-free (n x)
                               (cons (list adr 'assign-free)
                                     (cons (list (+ adr 1) n)
                                           (3imp->linear x (+ adr 2))))]
                 [assign-global (n x)
                               (cons (list adr 'assign-global)
                                     (cons (list (+ adr 1) n)
                                           (3imp->linear x (+ adr 2))))]
                 [define (n x)
                               (cons (list adr 'define)
                                     (cons (list (+ adr 1) n)
                                           (3imp->linear x (+ adr 2))))]
                 [conti (x)
                       (cons (list adr 'conti )
                             (3imp->linear x (+ adr 1)))]
                 [nuate (stack x)
                        (cons (list adr 'nuate)
                              (cons (list (+ adr 1) stack)
                                    (3imp->linear x (+ adr 2))))]
                 [frame (ret x)
                        (let* ([xcode (3imp->linear x (+ adr 2))]
                               [retadr (+ 2 adr (length xcode))]
                               [retcode (3imp->linear ret retadr)])
                          (cons (list adr 'frame)
                                (cons (list (+ adr 1) retadr)
                                      (append xcode retcode))))]
                 [argument (x)
                       (cons (list adr 'argument)
                             (3imp->linear x (+ adr 1)))]
                 [shift (n m x)
                       (cons (list adr 'shift)
                             (cons (list (+ adr 1) n)
                                   (cons (list (+ adr 2) m)
                                         (3imp->linear x (+ adr 3)))))]
                 [apply (n)
                       (list (list adr 'apply)
                             (list (+ adr 1) n))]
                 [return (n)
                         (list (list adr 'return)
                               (list (+ adr 1) n))])))

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
