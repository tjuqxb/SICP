;add to complex package
(define (p1 x)
  (make-real (real-part x)))

(put 'project 'complex p1)

;add to real package
(define (p2 x)
  (let ((m (inexact->exact x)))
    (make-rat (numerator m)
              (denominator m))))

(put 'project 'real p2)

;add to rational package
(define (p3 x)
  (make-int (round x)))

(put 'project 'rational p3)


