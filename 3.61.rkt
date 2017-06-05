(define (cal-1 s);(cal-1 s) 是一个不唯一的过程语句，每一次调用均产生一个新的环境？
  (define m
  (cons-stream
   1
   (mul-series
    (stream-map
     (lambda(x)
       (- 0 x))
     (stream-cdr s))
    m)))
  m)


