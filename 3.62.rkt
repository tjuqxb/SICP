(define (div-series s1 s2) ;s2参与计算时常数项必须是1
  (if (= 0 (stream-car s2))
      (error"s2的首次项为0"(stream-car s2))
      (scale-stream
       (mul-series s1
                  (cal-1(scale-stream s2 (/ 1 c))))
       (/ 1 c))))

(define ones
  (cons-stream 1
               ones))


(define int-s
  (cons-stream 1
               (add-streams int-s
                            ones)))

(define (integrate-series s)
  (stream-map * s
              (stream-map (lambda(x)
                            (/ 1 x))
                          int-s)))

(define cosine-series
  (cons-stream 1
               (integrate
                (map-stream
                 (lambda(x)
                   (- 0 x))
                   sine-series))))

(define sine-series
  (cons-stream 0
               (integrate
                consine-series)))

(define tan-series
  (div-series (sine-series)
              (cosine-series)))

(define (mul-series ss1 ss2)
  (con-stream 
     (* (stream-car ss1)
        (stream-car ss2))
     (add-streams
      (mul-series (stream-cdr s1)
                  (stream-cdr s2))
      (add-streams
       (stream-map (lambda(x)
                     (* x
                        (stream-car ss1)))
                   (stream-cdr ss2))
       (stream-map (lambda(x)
                     (* x
                        (stream-car ss2)))
                     (stream-cdr ss1))))))
(define d-tanx
  (cons-stream 1
               (mul-series
                (integrate d-tanx)
                (integrate d-tanx))))

(define tan-x
  (cons-stream 0
               (integrate d-tanx)))

