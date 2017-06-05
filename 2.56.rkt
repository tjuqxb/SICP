(define (exponentiation? m)
  (and (pair? m) (eq? (car x) '**)))


(define (base m)
  (cadr m))

(define (exponent m)
  (caddr m))


(define (make-expoentiation m n)
  (cond ((=number? n 0)
         1)
        ((=number? n 1)
         m)
        (else(list '** m n))))


(cond ((exponentiation? m)
       (make-product
        (make-product
         (exponent m)
         (make-exponentiation 
           (base m) 
           (make-sum
           (exponent m)
            -1)))
        (deriv (base m) var))))