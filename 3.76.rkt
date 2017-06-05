(define (smooth ss)
  (let ((s (cons-stream 0 ss)))
  (let ((s0 (stream-car s))
        (s1 (stream-car (stream-cdr s))))
    (cons-stream
     (/ (+ s0 s1) 2)
     (smooth (stream-cdr s))))))


