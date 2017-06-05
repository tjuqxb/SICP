(define (make-vect x y)
  (cons x y))

(define (xcor-vect m)
  (car m))

(define (ycor-vect m)
  (cdr m))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
                (xcor-vect b))
             (- (ycor-vect a)
                (ycor-vect b))))


(define (make-segment a b)
  (cons (sub-vect a origin)
        (sub-vect b origin)))

(define (start-segment m)
  (car m))

(define (end-segment m)
  (cdr m))

