(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ;changed(actual values)
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
            arguments                             ;changed
           (procedure-environment procedure))))
        (else
         (error
          "Unkown procedrue type --APPLY" procedure))))


(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp)))) 


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame-lazy vars vals base-env) base-env) ;modified
      (if (< (length var) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (make-frame-lazy variables env)
  (let (( to-process (make-frame-lazy (cdr variables) (cdr values))))
  (cond ((and (null? variables) (null? values))
         (cons nil nil))
        ((lazy? (car variables))
         (cons (cons (get-variable (car variables))
                     (car to-process)))
               (cons (normal-thunk (car values) env)
                     (cdr to-process)))
        ((lazy-memo? (car variables))
         (cons (cons (get-variable (car variables))
                     (car to-process)))
               (cons (memo-thunk (car values) env)
                     (cdr to-process)))
        (else
         (cons (cons (car variables)
                     (car to-process))
               (cons (car values)
                     (cdr to-process)))))))


(define (lazy? variable)
  (and (and (pair? variable) (pair? (cdr variable)))
       (eq? (cadr variable) 'lazy)))


(define (lazy-memo? variable)
  (and (and (pair? variable) (pair? (cdr variable)))
       (eq? (cadr variable) 'lazy-memo)))

(define (get-variable variable)
  (cond ((pair? variable)
         (car variable))
        variable))

(define (normal-thunk exp env)
  (list 'thunk exp env))

(define (memo-thunk exp env)
  (list 'memo-thunk exp env))


(define (actual-value exp env)    ;cooperate with force-it,looping
  (force-it (eval exp env)))


(define (force-it obj)                      ;cooperate with actual-value,looping
  (cond ((normal-thunk? obj)
         (actual-value
          (thunk-exp obj)
          (thunk-env obj)))
         
         ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)       ;replace exp with its value
           (set-cdr! (cdr obj) '())          ;forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))


              
