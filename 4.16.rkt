(load "元循环求值器.rkt")
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
       
        ((lambda? exp)
        (scan-out-defines (make-procedure (lambda-parameters exp)
                                            (lambda-body exp)
                                             env)))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        
        
                    
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
           (cons (cadar list) nil))
          (else (cons (cadar list) (get-2nd (cdr list))))))
  (get-2nd (cadr exp)))

 
(define (let->combination exp)
 (cons (make-lambda (let-parameters exp) (let-body exp))
                     (let-vals exp)))

(define (make-let pair-lists body-exp)
  (cons 'let (cons pair-lists body-exp))) 


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vals)
            (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (cond ((eq? (car vals) '*unassigned)
                    (error "Unassigned variable" var ))
                    
                   (else (car vals))))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))



(define (definition->set! exp)
  (list 'set! (definition-variable exp) (definition-value exp)))

(define (scan-out-defines procedure)
  (define (transform let-pairs set!-sequence non-set!-sequence body-exp)
    (cond ((null? body-exp)
           (cond ((null? let-pairs)
                  procedure)
           
                 (else
                  (make-procedure (procedure-parameters procedure)
                          (list(let->combination (make-let let-pairs (cons (sequence->exp set!-sequence) 
                                                               non-set!-sequence))))
                                
                          (procedure-environment procedure)))))
          ((definition? (car body-exp))
             (cond ((null? set!-sequence)
                    (transform (cons (list (definition-variable (car body-exp)) (quote'*unassigned)) nil)
                               (cons (definition->set! (car body-exp)) nil)
                                non-set!-sequence
                                (cdr body-exp)))
                   (else
                    (set-cdr! let-pairs
                              (cons (list (definition-variable (car body-exp)) (quote'*unassigned)) (cdr let-pairs))) 
                    (set-cdr! set!-sequence
                              (cons (definition->set! (car body-exp)) (cdr set!-sequence))) 
                    (transform let-pairs set!-sequence non-set!-sequence (cdr body-exp))))) 
          (else
           (cond ((null? non-set!-sequence)
                  (transform let-pairs set!-sequence
                             (cons (car body-exp) nil)
                             (cdr body-exp)))
                 (else
                  (set-cdr! non-set!-sequence (cons (car body-exp) (cdr non-set!-sequence)))
                  (transform let-pairs set!-sequence non-set!-sequence (cdr body-exp)))))))
  (transform nil nil nil (procedure-body procedure)))






;make-procedure里好一些，一次性scan完成，如果放在procedure-body里，每次调用函数都要进行scan，会重复scan。

;(driver-loop)         
                              
                              



