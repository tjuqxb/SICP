(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argtreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define x (stream-map show (stream-enumerate-interval 0 10)));to calculate x;(show 0); (display-line 0) 0

(define (show x)
  (display-line x)
  x)

(define (stream-ref x n)
  (if (= 1 n)
      (stream-car x)
      (stream-ref (stream-cdr x) (- n 1))))


(define (stream-enmumerate-interval a b)
  (if (> a b)
      the-empty-stream
      (cons-stream
       a
       (stream-enumerate-interval (+ a 1) b))))


(stream-enumerate-interval 0 10)
(cons-stream
 0
 (stream-enumerate-interval 1 10))

x
(cons-stream
 (show (stream-car (stream-enumerate-interval 0 10))) 
 (stream-map show (stream-cdr (stream-enumerate-interval 0 10)))))

(cons-stream
 0
 (stream-map show (stream-cdr (stream-enumerate-interval 0 10))))

(stream-ref x 5)
(stream-ref (stream-map show (stream-cdr (stream-enumerate-inteval 0 10))) 4)
(stream-ref
 (cons-stream
  (show 1) ;(show 1); (display-line 1) 1
  (stream-map show (stream-cdr (stream-enumerate-interval 1 10))))
 4)
;show 1 2 3 4(n=1) (show 5) and return 5;result (display 1 2 3 4 5) return 5
;when n=7;some layers of x has halready been calculated and memorized.
x
(cons-stream
 0
 (stream-map show (stream-cdr (stream-enumerate-interval 0 10)))) ;this line has been memorized as below.

(cons-stream
 1
 (stream-map show (stream-cdr (stream-enumerate-inteval 1 10))))

;so on early stages, (show 0) (show 1) (show 2) (show 3) (show 4) (show 5) won't run again.


