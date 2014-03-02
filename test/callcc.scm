(list 'a (call/cc (lambda (c) (set! p c) (c 'b))) 'c)
