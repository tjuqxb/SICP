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
"""

(define (fixed-point f start s n)
  (if (< (abs ( - (f start) start)) s)
      start
      (fixed-point f ((average-damp-n f n)start) s n)))

(define (xn n s)
  (if (= 0 n) (lambda(x) s)
      (lambda(x) (/ ((xn (- n 1) s) x) x)))) 


(exact->inexact(fixed-point (xn 4 4) 1 0.001 3))

((average-damp-n (lambda(x) (/ 4 x)) 0) 1)

((average-damp-n (lambda(x) (/ 4 x)) 1) 1)

((average-damp-n (lambda(x) (/ 4 x)) 2) 1)

((average-damp-n (lambda(x) (/ 4 x)) 3) 1)


(define (iter-improve good? improve)
  (define (iter x)
    (if (good? x) 
        x
        (iter (improve x))))
  (lambda(x) (iter x)))

(define (sqrt-good? x)
  (define (in-good? s m)
    (<(abs (- x (/ s x))) m))
  (in-good? 5 0.0001))

(define (sqrt-improve x)
  (define (in-improve s)
    (/(+ x (/ s x)) 2))
  (in-improve 5))

((iter-improve sqrt-good? sqrt-improve) 4)

(define (fix-good? x)
  (define (in-good? f m)
    (<(abs(- x (f x))) m))
  (in-good? (lambda(x) (/ 5 x)) 0.0001))

(define (fix-improve x)
  (define (in-improve f)
    (/(+ x (f x)) 2))
  (in-improve (lambda(x) (/ 5 x))))

((iter-improve fix-good? fix-improve) 3)



