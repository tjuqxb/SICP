(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(define (ge2 S T)
  (cons-stream (list (stream-car S)
                     (stream-car T))
               (interleave
                (stream-map
                 (lambda(x)
                   (list (stream-car S)
                         x))
                 (stream-cdr T))
                (ge2 (stream-cdr S) (stream-cdr T)))))


(define (ge3 S T U)
  (cons-stream (list (stream-car S)
                     (stream-car T)
                     (stream-car U))
               (interleave
                (stream-map (lambda(x)
                              (cons
                               (stream-car S)
                               x))
                            (ge2 (stream-cdr T) (stream-cdr U)))
                (ge3 (stream-cdr S) (stream-cdr T) (stream-cdr U)))))


(define (stream-filter f? S)
  (if (f? (stream-car S)
          (cons-stream (stream-car S)
                       (stream-filter f? (stream-cdr S)))
          (stream-filter f? (stream-cdr S)))))


(define (triples S T U)
  (stream-filter (lambda(x)
                   (= 0
                      (-(+(sqr (car x))
                          (sqr (cadr x)))
                        (sqr (caddr x)))))
                 (ge3 S T U)))
                             

(define (sqr x)
  (* x x))



                                    