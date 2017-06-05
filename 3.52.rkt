(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda(x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)

(display-stream z)

(define (stream-enmumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream
       a
       (stream-enumerate-interval (+ a 1) b))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argtreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref x n)
  (if (= 0 n)
      (stream-car x)
      (stream-ref (stream-cdr x) (- n 1))))

(stream-enumerate-interval 1 20)
(cons-stream
 1
 (stream-enumerate-interval 2 20))

seq
(cons-stream
 1
 (stream-map accum (stream-cdr (stream-enumerate-interval 1 20))));val;1

y
(cons-stream
 6
 (stream-filter even?
                (stream-map accum (stream-cdr (stream-enumerate-interval 3 20)))));val; 2


z
(cons-stream
 10
 (stream-filter (lambda(x) (= (remainder x 5) 0))
                (stream-map accum (stream-cdr (stream-enumerate-interval 4 20)))));val; 10



(stream-ref y 7)
;sum;136 

(display-stream z)
;10 15 45 55 105 120 190 210