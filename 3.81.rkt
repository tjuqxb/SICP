(define (create-rand init)
  (define random-num
    (cons-stream init
                 (stream-map rand-update random-num)))
  random-num)


(define randoms (create-rand 1))

(define (proc requests randoms)
  (if (stream-null? requests) the-empty-stream
      
  (if (eq? 'generate (stream-car requests))
      (cons-stream
          (stream-car randoms)
          (proc (stream-cdr requests)
                (stream-cdr randoms)))
      (let ((new-randoms
             (create-rand (stream-car requests))))
        (proc (stream-cdr requests) new-randoms)))))







      