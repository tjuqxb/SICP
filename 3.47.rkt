(define (make-serializer0 n)
  (let ((mutex0 (make-mutex)))
    (let ((m 0))
      (define (ss p)
        (mutex0 'acquire)
        (cond ((= m n)
               (ss p))
              ((< m n)
               (set! m (+ m 1))))
        (mutex0 'release)
        (define (ss-p .args)
         (let ((val (apply p args)))
              (mutex0 'acquire)
              (set! m (- m 1))
              (mutex0 'release)
               val))
        ss-p)
      ss)))

(define (make-mutex1 n)
  (let ((cell (list false)))
    (let ((p 0))
      
      (define (test-and-set-n!)
        (define (iter cell)
          (without-interrupts
          (lambda()
           (cond ((= p n)
                  (begin (set-car! cell #t) #t))
                 ((< p n)
                  (begin (set! p(+ p 1))
                        #f))))))
             iter)
      
      (define (clear! cell)
        (without-interrupts
         (begin (set-car! cell #f)
                (set! p (- p 1)))))
  
      (define (the-mutex m)
        (cond ((eq? m 'acquire)
               (if (test-and-set-n! cell)
                   (ther-mutex 'acquire))
               ((eq? m 'release) (clear! cell)))))
  the-mutex)))
      
                   
                   
                   

  
         
