(define (make-segment a b c d)
  (cons (cons a b) (cons c d)))

(define (start-segment x)
 (car x) )

(define (end-segment x)
  (cdr x))

(define (make-point)
  (cons))

(define (x-point x)
  (car x))

(define (y-point x)
  (cdr x))

(define (midpoint-segment x)
  (cons (/(+ (x-point (start-segment x)) 
             (x-point (end-segment x)))
          2)
        (/(+ (y-point (start-segment x)) 
             (y-point (end-segment x)))
          2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment (make-segment 1 3 4 6)))


(define (make-rectangle-a a0 b0 a1 b1 a2 b2 a3 b3 )
  (cons (cons(make-segment a0 b0 a1 b1)
             (make-segment a2 b2 a3 b3))
        (cons(make-segment a0 b0 a2 b2)
             (make-segment a1 b1 a3 b3))))

(define (length x)
  (let ((a (x-point(start-segment x)))
        (b (y-point(start-segment x)))
        (c (x-point(end-segment x)))
        (d (y-point(end-segment x))))
    (