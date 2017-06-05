(define (split f1 f2)
  (define (fs m n)
      (if (= 0 n)
           m
           (f1 m (f2 (fs m (- n 1)) (fs m (- n 1))))))
  (lambda(m n) fs))


 