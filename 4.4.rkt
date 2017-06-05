(cond ((and? exp)
       (eval (and->and exp env) env))
      ((or? exp)
       (eval (or->or exp env) env)))


(define (and->and exp env)
  (cond ((null?  exp)
        
             #t)
       ((false? (eval (and-first-exp exp) env))
       #f)
       (else 
        (make-and (and-rest-exp exp)) env)))

(define (eval-or exp env)
   (cond ((null? exp)
          
              #f)
         ((true? (eval (and-first-exp exp) env))
              #t)
         (else 
           (make-or (or-rest-exp exp) env))))
       

(define (eval-and0 exp env)
 (define (eval-a seq env)
   (cond ((null? (cdr seq))
          (if (false? (eval (car seq) env))
              #f
              #t))
         ((false? (eval (car seq) env))
          #f)
         (else
          (eval-a (cdr seq) env))))
  (eval-a (and-clause exp) env))

(define (eval-or0 exp env)
  (define (eval-o seq env)
    (cond ((null? (cdr seq))
           (if (true? (eval (car seq) env))
               #t
               #f)
           ((true? (eval (car seq) env))
            #t)
           (else
            (eval-o (cdr seq) env)))))
  (eval-o (or-clause exp) env))

