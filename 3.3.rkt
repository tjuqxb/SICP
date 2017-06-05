(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
   ((set! banlance (+ balance amount))
    balance))
  (define (dispatch m n)
    (cond ((and (eq? m password)
                (eq? n 'withdraw))
           withdraw)
          ((and (eq? m password)
                (eq? n 'deposit))
           deposit)
          ((not (eq? m password))
           (error "incorrect-password" m))
          (else (error"Unknown requese" n))))
  dispatch)


 (define acc (make-account 100 'secret))
 
 ((acc 'secret 'withdraw) 40)
 
 ((acc 'secret 'with) 40)
 
 ((acc 'some 'deposit) 50)
 
 
    