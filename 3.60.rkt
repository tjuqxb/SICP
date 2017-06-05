(define (mul-series ss1 ss2)
  (con-stream 
     (* (stream-car ss1)
        (stream-car ss2))
     (add-streams
     (add-streams
     (stream-map (lambda(x)
                     (* x
                        (stream-car ss1)))
                   (stream-cdr ss2))
    
       (stream-map (lambda(x)
                     (* x
                        (stream-car ss2)))
                     (stream-cdr ss1)))
     (cons-stream
       0
       (mul-series (stream-cdr s1)
                  (stream-cdr s2))))))


                   
      