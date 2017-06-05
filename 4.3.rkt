(define eval-table
  nil)

(define (put func sign table)
  (cons (cons sign func) table))

(define (look-up sign table)
  (cond
    ((null? table)
     (error "not a clear type--L00K-UP" sign))
    ((eq? (caar table) sign)
        (cdar table))
       (else
        (look-up sign (cdr table)))))

(define (eval exp env)
  ((look-up (eval-type exp) eval-table)
   exp env))

(define (eval-type exp)
  (car exp))

(define (make-self-evaluating exp)
  (cons 'self-evaluating exp))

(define (put-self-evaluate-in)
(define (self-evaluate exp env)
  (cond ((number? (cdr exp) exp))
        ((string? (cdr exp) exp))
        (else false)))
  (put self-evaluate 'self-evaluating eval-table))
  

