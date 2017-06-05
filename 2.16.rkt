(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (max (car x)
       (cdr x)))

(define (lower-bound x)
  (min (car x)
       (cdr x)))

(define (sub-interval x y)
  (cons (- (lower-bound x) 
           (lower-bound y))
        (- (upper-bound x)
           (upper-bound y))))


(define (width x)
  (/ (- (upper-bound x) 
        (lower-bound x))2))


"""(add-interval x y)
(cons (+ (lower-bound x)
         (lower-bound y))
      (+ (upper-bound x)
         (upper-bound y)))

(width (add-interval x y))
(/(-(upper-bound(add-interval x y))
    (lower-bound(add-interval x y)))
  2)

(/(+(-(upper-bound x) (lower-bound x))
    (-(upper-bound y) (lower-bound y)))
  2)

(+(width x) (width y))

(-(width x) (width y))
"""

(define (div-interval x y)
  (if (not( >(*(lower-bound y)
             (upper-bound y))
           0))
      (error "0 should not be within the range of y")
  (mul-interval x
                (make-interval (/1.0(upper-bound y))
                               (/1.0(lower-bound y))))))


(define (mul-interval-a x y)
  