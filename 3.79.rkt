(define (solve-2nd a b dt y0 dy0 f)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (map-stream f
                y
                dy))
  y)


(define (integral delayed-dy y0 dt)
  (define m
    (cons-stream y0
                 (add-stream
                  (scale-stream (force delayed-dy) dt)
                  m)))
  m)`