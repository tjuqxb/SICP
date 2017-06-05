(define (adjoin-set x set)
  (define (iter m s)
    (cond
      ((null? s)
       #t)
      ((< m (car s))
        #f)
        ((= m (car s))
         #t)
        (else (iter m (cdr s)))))
  (if (iter x set)
      set
      (cons x set)))



  