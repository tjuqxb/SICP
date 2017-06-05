(require "元循环求值器.rkt")

;no add-binding-to-frame method

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment nil)

(define (empty-frame nil))

(define (make-frame variables values)
  (cond ((and (null? variables) (null? values))
         nil)
        ((and (null? varirables) (not (null? values)))
         error "too many values" variables values)
        ((and (not (null? variables)) (null? values))
         error "too many variables" variables values)
        (else (cons (cons (car variables) (car values)))
              (make-frame (cdr variables) (cdr values)))))

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (cdar frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound Variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (set-cdr! (car frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variabl--SET!" var)
        (let ((frame (first-frame env))
              (scan frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame) env))
    (define (scan frame)
      (cond ((null? frame)
             (set-car! env
                       (cons (cons var val) frame)))
            ((eq? var (caar frame))
             (set-cdr! (car frame) vaal))
            (else (scan (cdr frame)))))
    (scan frame)))



