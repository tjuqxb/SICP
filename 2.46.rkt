(define (make-vect x y)
  (cons x y))

(define (xcor-vect m)
  (car m))

(define (ycor-vect m)
  (cdr m))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
                (xcor-vect b))
             (+ (ycor-vect a)
                (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
                (xcor-vect b))
             (- (ycor-vect a)
                (ycor-vect b))))


(define (scale-vect a m)
  (make-vect (* a (xcor-vect m))
             (* a (ycor-vect m))))

