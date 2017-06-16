(define (wrapper vars vals body)
  (cond ((and (null? vars) (null? vals))
         body)
        ((and (not (null? vars)) (not (null? vals)))
         (let ((car vars) (car vals))
           body))
        (else
         (error "mismatch" vars vals))))

(define (substitute-proc body part0 part1)) ;implemented in complier
  

(define (class-wrapper static-vars static-vals p-static-body super)
  (if (null? super)
      (wrapper static-vars static-vals p-static-body)
      (wrapper
       (concatenate (get-static-vars super) static-vars)
       (concatenate (ger-static-vals super) static-vals)
       (substitute-proc p-static-body (get-instance-body p-static-body)
        (pass-super (get-instance-body super) (get-instance-body p-static-body))))))

(define (pass-super super-ins-body instance-body)
  (wrapper
   (concatenate (get-dynamic-vars super-ins-body) (get-dynamic-vars instance-body))
   (concatenate (get-dynamic-vals super-ins-body) (get-dynamic-vals instance-body))
   (substitute-proc (get-dynamic-body instance-body) (get-constructor get-dynamic-body)
                    (pass-constructor (get-constructor super-ins-body) (get-constructor get-dynamic-body)))))


