(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'mag-part) (* r (sin a)))
          (else
           (error "Unkown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)



