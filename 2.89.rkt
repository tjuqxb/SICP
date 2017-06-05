;; add to the dense polynomial package
(define (make-poly-dense var terms)
  (lambda(op) 
    (cond ((= 'terms 'op)
           terms)
          ((= 'var 'op)
           var))

(define (op m)
  (m op))

;;terms is defined as (list 3 0 2 1) -> (3*(exp x 3) + 2 * x + 1)
  
(define (term-list m)
  (let ((terms (terms m)))
    (define (high-order terms)
      (cond ((null?  terms)
           (error "the polynomial does not exist--TERM-LIST" (list m)))
            ((null? (cdr terms))
             0)
          (else(+ (high-order (cdr terms)) 1))))
    (let ((mm  (high-order terms)))
      (define (order-list m)
        (if (<0 m)
               nil
             (cons m (order-list (- m 1)))))
      (let ((orders (order-list mm)))
        (define (generate orders terms)
          (cond ((null? orders)
                 nil)
                ((= 0 (car terms))
                 (generate (cdr orders) (cdr terms)))
                (else (cons (list (car orders) (car terms)) (generate (cdr orders) (cdr terms))))))
        (generate orders terms)))))
    
    
    
    
          