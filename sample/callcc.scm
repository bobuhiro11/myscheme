(+ (call/cc (lambda (c) (set! x c) (c 10))) 20)
