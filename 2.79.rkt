(define (install-equ?-package)
  (define (equ-rat? x y)
    (and (= (numer x) (number y))
         (= (denom x) (denom y))))
  (define (equ-number? x y)
    (= x y))
  (define (equ-complex? x y)
    (and (= (real-part x)
            (real-part y))
         (= (imag-part x)
            (imag-part y))))
  
  ;;interface
  (put 'equ? ('rational 'rational) equ-rat?)
  (put 'equ? ('scheme-number 'scheme-number) equ-number?)
  (put 'equ? ('complex 'complex) equ-complex?)
  'done)

