(define (up-split m n)
  (let (up (up-split m (- n 1)))
    (if (= 0 n)
        m
        (below m (beside up up)))))


