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


