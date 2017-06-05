(define (make-monitored f)
  (let ((n 0))
   (lambda(m)
     (cond((eq? m 'how-many-calls?)
           n)
          ((eq? m 'reset-count)
           (set! n 0))
          (else (begin (set! n (+ 1 n))
                       (f m)))))))

(define s (make-monitored sqrt))

(s 'how-many-calls?)

(s 100)

(s 'how-many-calls?)