(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))


(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

(define ln2-s
  (partial-sums (ln2-summands 1))) 


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* 02 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(define s1
  (euler-transform ln2-s))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (trans-form s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


(define s2
  (accelerated-sequence euler-transform ln2-s))


