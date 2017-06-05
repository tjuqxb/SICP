(define (integral delayed-dy y0 dt)
  (define m
    (cons-stream y0
                 (add-stream
                  (scale-stream (force delayed-dy) dt)
                  m)))
  m)



(define (RLC R L C dt)
  (lambda(vc0 iL0)
    (define vc
      (integral (delay (scale-stream iL (- (/ 1 C))))
                vc0 dt))
    
    (define iL
      (integral (delay (add-stream
                        (scale-stream 
                         vc (/ 1 L))
                        (scale-stream
                         iL (- (/ R L)))))
                iL0 dt))
    
    (map-stream (lambda(x y)
                  (cons x y))
                vc iL)))
    