;;Lower types(such as rational)when to be computed with higer types(such as scheme-number)should be coerced into scheme-number type in the apply-generic process.
;;Scheme-numer processes are original system processes.
;;'add 'sub 'mul and 'div have already been defined in the previous form(especially when the type is 'rational).  
;;Complex-rect and complex-polar are columns of another form. 
;;Thus we can call them sub-types.When computing them in the form,we actually meet some linkages.
;;In this programme,the issue of the domain of the definition should be taken into consideration.  
;;To solve this problem,I deem that the complex-rect and the complex-polar should be included in the working form.
"""
                          complex                 coplex-rect      complex-polar

make-from-real-imag      tag(make-from-real-imag)  func-a1              func-b1
make-from-mag-ang        tag(make-from-mag-ang)    func-a2              func-b2
real-part                 real-part                func-a3              func-b3
.                          .
.                          .
.                          .

"""
;;add into global
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-genercic 'cosine x))
(define (arctan x) (apply-generic 'arctan x))
(define (exp x y) (apply-generic 'exp x y))

;;add into rational package
(put 'sine '(rational) (lambda(x) (tag (sin (/ (numer x) (denom x))))))
(put 'cosine '(rational) (lambda(x) (tag(cos(/ (numer x) (denom x))))))
(put 'arctan '(rational) (lambda(x) (tag(atan(/(numer x) (denom x))))))
(put 'exp '(rational rational) (lambda(x y) (tag (exp (/ (numer x) (denom x)) (/(numer y) (denom y))))))

;;complex-rect package
(define (square x) (mul x x))
(define (sqrt x) (exp x 0.5))
(define (magnitude z) (sqrt (add (square (real-part z)) (square (imag-part z)))))
(define (angle z) (arctan (div (imag-part z) (real-part z))))

;;complex-polar package
(define (real-part z) (mul (magnitude z) (cosine (angle z))))
(define (imag-part z) (mul (magnitude z) (sine(angle z))))

;;complex package
(put 'real-part 'complex real-part)
(put 'ima-part complex imag-part)
(put 'magnitude 'complex magnitude)
(put 'angle 'complex angle)
(define (add-complex z1 z2)
  (make-from-real-imag (add(real-part z1) (real-part z2)) (add (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (sub(real-part z1) (real-part z2))
                       (sub(imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                     (add (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                     (sub (angle z1) (angle z2))))




