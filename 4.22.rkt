(load "语法分离.rkt")


(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp)
         (analyze-assignement exp))
        ((definition? exp)
         (analyze-definition exp))
        ((if? exp)
         (analyze-if exp))
        ((let? exp)
         (analyze-let exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begine-actions exp)))
        ((cond? exp)
         (analyze (cond->if exp)))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Unkown expression type -- ANANLYZE" exp))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-parameters exp)
  (define (get-1st list)
    (cond ((not (pair? list))
           (error "bad syntax--LET-PARAMETERS"exp))
          ((not (pair? (car list)))
           (error "bad syntax--LET-PARAMETERS"exp))
          ((null? (cdr list))
           (cons (caar list) nil))
          (else (cons (caar list) (get-1st (cdr list))))))
  (get-1st (cadr exp)))

(define (let-body exp)
  (cond ((null? (cddr exp))
         (error "no expression in body" exp))
        (else (cddr exp))))

(define (let-vals exp)
  (define (get-2nd list)
    (cond ((not (pair? list))
           (error "bad sytax--LET-PARAMETERS"exp))
          ((not (pair? (car list)))
           (error "bad sytax--LET-PARAMETERS"exp))
          ((null? (cdr list))
           (cons (cadar list)nil))
          (else(cons (cadar list) (get-2nd (cdr list))))))
  (get-2nd (cadr exp)))


(define (analyze-let exp)
  (let ((vars (let-parameters exp))
        (vals (map analyze (let-vals exp)))
        (body (analyze-sequence (let-body exp))))
    (lambda(env)
      (execute-application
       (make-procedure vars body env)
       (map (lambda(value) (value env)) vals)))))

(driver-loop)




