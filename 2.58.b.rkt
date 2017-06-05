(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) 
              (number? a2))
         (+ a1 a2))
        (else  (list a1 '+ a2))))


(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (sum-operation x)
  (define (iter a b c)
    (cond ((null? b)
            (list #f b c))
          ((eq? (car b) a)
           (list #t (cdr b) c))
           (else (iter a (cdr b) (append c (list (car b)))))))
  (iter '+ (cdr x) (list (car x))))

(define (sum? x)
  (and (pair? (cdr x))
       (car (sum-operation x))))

(define (addend x)
  (let ((m (caddr (sum-operation x))))
    (if (not(pair? (cdr m)))
        (car m)
        m)))


(define (augend x)
  (let ( (m (cadr (sum-operation x))))
    (if (not (pair? (cdr m)))
        (car m)
        m)))
               


(define (product? x)
  (and (pair? (cdr x))
       (not (sum? x))
       (eq? (cadr x) '*)))
 

(define (multiplier x)
  (car x))

(define (multiplicand x)
  (if (not (pair? (cdr(cddr x))))
      (car(cddr x))
      (cddr x)))
         
       

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2) 
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type--DERIV" exp))))


(display (deriv '(x * x + 3 * (x + x * y + 2 + x + x * x)) 'x))

(newline)

(display (sum-operation '(x * x + 3 * (x + x * y + 2 + x + x * x))))
(newline)
(display (multiplicand (addend '(x * x + 3 * (x + y + 2)))))
(newline)
(display (multiplicand (augend '(x * x + 3 * (x + y + 2)))))
(newline)
(display (deriv '(x * x) 'x))
(newline)


 
(display (make-sum 'x 'x))