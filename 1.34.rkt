(define (compose f g)
  (lambda(x) (f(g x))))


(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

((compose square inc)6)

(define (repeated f n)
  (if (= n 0) (lambda(x) x)
      (compose f (repeated f (- n 1)))))

((repeated square 2)5)

(define (smooth f)
  (lambda(x) (average3 (f (- x dx) (f x) (f (+ x dx))))))

(define (dx)
  0.000001)

(define (average3 a b c)
  (/ (+ a b c) 3))

(define (smoothn f n)
  (reapeated (smooth f) n))

(define (average-damp f)
  (lambda(x) (/ ( + (f x)  x) 2)))
"""
(define (average-damp-n f n)
  ((repeated average-damp n ) f ))


"""
(define (average-damp-n f n)
  (define (iter n s )
    (define (g f)
       (lambda(x)(/ (+ x (f x)) 2)))
    (if (= 0 n) 
        (s f) 
        (iter (- n 1) (compose g s))))
(iter n (lambda(x) x)))


(define (fixed-point f start s n)
  (if (< (abs ( - (f start) start)) s)
      start
      (fixed-point f ((average-damp-n f n)start) s n)))

(define (xn n s)
  (if (= 0 n) (lambda(x) s)
      (lambda(x) (/ ((xn (- n 1) s) x) x)))) 


(exact->inexact(fixed-point (xn 4 4) 1 0.001 3))

