(load "元循环求值器.rkt")
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((while? exp) (eval-while exp env))
        ((lambda? exp)
         (make-porocedure (lambda-parameters exp)
                          (lambda-body exp)
                          env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        
                    
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
         (else
          (error "Unkown expression type--EVAL" exp))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-parameters exp)
  (define (get-1st list)
    (cond ((not (pair? list))
           (error "bad sytax--LET-PARAMETERS"exp))
          ((not (pair? (car list)))
           (error "bad sytax--LET-PARAMETERS"exp))
          ((null? (cdr list))
           (list (caar list)))
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
           (list (cadar list)))
          (cons (cadar list) (get-2nd (cdr list)))))
  (get-2nd (cadr exp)))

 
(define (let->combination exp)
 (cons (make-lambda (let-parameters exp) (let-body exp))
                     (let-vals exp)))

(define (make-let pair-lists body-exp)
  (cons 'let pair-lists body-exp))

;4.7
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*pair-lists exp)
  (cadr exp))

(define (let*body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (let ((pairs (let*pair-lists exp))(body (let*body exp)))
    (define (modify pairs body)
      (cond ((symbol? (car pairs)) (error "no val of the 1st var" exp))
            ((null? (cdr (car pairs))) (error"no val of the 1st var" exp))
            ((not (null? (cddr (car pairs)))) (error"bad syntax 1st pair"exp))
            ((null? (cdr pairs)) (make-let (list (car pairs)) body))
            (else (make-let (list (car pairs)) (modify (cdr pairs)) body))))
    (modify pairs body)))

;够了，因为会一直驱动递归求值

;4.8
(define (let-name->combination exp)
  (cond ((pair? (cadr exp))
         (cons (make-lambda (let-parameters exp) (let-body exp))
                     (let-vals exp)))
        ((symbol? (cadr exp))
         (let ((name (cadr exp))
               (parameters (let-parameters (cdr exp)))
               (body (let-body (cdr exp)))
               (vals (let-vals (cdr exp))))
           (sequence->exp
            (cons 'define (cons (cons name parameters) body))
            (cons name vals))))))

;4.9



(define (while? exp)
  (tagged-list? exp 'while))

(define (while-cond exp)
  (cadr exp))

(define (while-body exp)
  (cddr exp))

(define (eval-while exp env)
  (if (true? (eval (while-cond) env))
      ((eval (sequence-exp (while-body exp)) env)
       (eval-while exp env))
      'done))

(define (expand-while exp)
  (seqnuence->exp
   (list 'define (list 'while 'x 'y)
         (make-if  'x
                  (list 'y 
                        (list 'while 'x 'y))))
   (list 'while (while-cond exp) (while-body exp))))



;4.10

;(x1 f x2)

;(driver-loop)


           
           
         
  



