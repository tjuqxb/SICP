;;This would take much work ,but I deemed that the key was the transformation process of sparse polynomials between dense polynomials.


;;add to dense-polynomial package
(define (make-poly-dense var terms)
  (tag-dense 
   (lambda(op) 
    (cond ((= 'terms 'op)
           terms)
          ((= 'var 'op)
           var)))))

(define (op m)
  ((content m) op))

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

(define (transform-to-sparse m)
  (make-poly-sparse (var m) (term-list m)))

(put 'transform 'poly-dense transform-to-sparse)

;;add to sparse-polynomial package
(define (make-poly-sparse var terms)
  (tag-sparse 
   (lambda(op) 
    (cond ((= 'terms 'op)
           terms)
          ((= 'var 'op)
           var)))))

(define (op m)
  ((content m) op))

(define (term-list m)
  (terms m))

;;Terms is defined as (list (3 3) (1 2) (0 1)) -> (3*(exp x 3) + 2 * x + 1). 
;;While in dense-polynomial,terms should be defined as (list  3 0 2 1). 

(define (get-dense-terms m)
  (let ((terms (terms m)))
    (let ((high-order (car (car terms))))
      (define (fill terms)
        (define (fill-once terms)
          (if (= (order (car terms))
                 (order (cadr terms)))
              terms
              (let (t1 (- (order (car terms))
                          (order (cadr terms))))
                (define (iter-fill  terms t1)
                  (if (= 1 t1)
                      (cons (car terms)
                            (cons (make-term (+(order (cadr terms) 1))
                                             0)
                                  (cdr terms)))
                      (cons (car (iter terms (- t1 1)))
                            (cons (make-term (+ (order (cadr (iter-fill terms (- t1 1)))) 1)
                                             0)
                                  (cdr (iter-fill terms (- t1 1)))))))
                (iter-fill terms t1))))
        (define (fill-before-end terms)
          (if (null? (cdr terms))
                     (fill-end terms)
              (fill-once (cons (car terms)
                               (fill-before (cdr terms))))))
        (define (fill-end terms)
          (if (null? (cdr terms))
              (if (= 0 (order (car terms)))
                     terms
                  (fill-once (list (car terms)
                                   (make-term 0 0))))
              (cons (car terms) (fill-end (cdr terms)))))
        (fill-before-end terms))
      (let ((full-terms (fill-terms m)))
        (define (transform full-terms)
          (if (null? full-terms)
              nil
              (cons (coeff (car full-terms)) (transform (cdr full-terms)))))
        (transform full-terms)))))

(define (transform-to-dense m)
  (make-poly-dense (var m) (get-dense-terms m)))

(put 'transform 'poly-sparse transform-to-dense)



               
        


               
               
                                   
                
