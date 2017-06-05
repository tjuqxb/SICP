;只删除第一个框架里的约束是安全的；覆盖上层是模型的特点，所以只删除底层约束，类似DEFINE。
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars  var-next vals val-next)
      (cond ((eq? var (car var-next))
             (set-cdr! vars (cdr var-next))
             (set-cdr! vals (cdr val-next)))
            ((null? var-next)
             (error "Unbounde var" var))
            (else 
             (scan (cdr vars) (cdr var-next) (cdr vals) (cdr val-next)))))
    (let ((vars (frame-variables frame)) (vals (frame-values frame)))
      (if (eq? var (car vars))
          (set-car! env (cons (cdr vars) (cdr vals)))
          (scan vars (cdr vars) vals (cdr vals))))))


  