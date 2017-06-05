(define (mul-streams a b)
  (stream-map * a b))

(define factorials (cons-stream 1 (mul-streams 
                                   factorial
                                   (integers-starting-from 2))))



                                   
                                   

