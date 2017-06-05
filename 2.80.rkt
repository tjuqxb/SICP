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


(define (intstall-zero?-package)
  (define (zero-rat? x)
    (= (numer x) 0))
  (define (zero-number? x)
    (= x 0))
  (define (zero-complex? x)
    (and (=(real-part x)
           0)
         (=(imag-part x)
           0)))
  ;interface
  (put 'zero? ('rational) zero-rat?)
  (put 'zero? ('scheme-number) zero-number?)
  (put 'zero? ('complex) zero-complex?)
  'done)

;;两个操作应该分别在对应TYPE列的内部添加定义，因为无法在同一个层级的表格内部再次引用其他列的函数（除了无标识数外，有标识数的最外层标识已经剥离）；另外一种做法是将所有需要引用的函数在PACKAGE内再写一次。
  
