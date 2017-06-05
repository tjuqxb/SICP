(define (make-accumulator x)
  (lambda(m)
    (begin (set! x (+ x m))
           x)))


(define A (make-accumulator 5))

(A 10)

(A 10)