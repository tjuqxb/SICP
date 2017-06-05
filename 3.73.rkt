(define (rc R C dt)
  (lambda(i-s v0)
    (define v1
      (cons-stream v0
                   (add-stream
                    (scale-stream i-s (/ dt C))
                    v1)))
    (add-stream (scale-stream i-s R)
                v1)))











