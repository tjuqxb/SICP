(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (ripple-carry-adder A B)
  (let ((c_n (make-wire)))
    (set-signal! c_n 0)
    (cond ((and (null? (cdr A))
                (null? (cdr B)))
           (full-adder (car A) (car B) c_n s c-out)
           (cons c-out (list s)))
          (else 
           (full-adder (car A) (car B) (car (ripple-carry-addr (cdr A) (cdr B))) s c-out)
           (cons (c-out) (cons s (cdr (ripple-carry-adder (cdr A) (cdr B)))))))))






(define (half-adder-delay)
  (+ or-gate-delay
     (* 2 and-gate-delay)
     inverter-delay))


(define (full-adder-delay)
  (+ (* 3 or-gate-delay)
     (* 4 and-gate-delay)
     (* 2 inverter-delay)))


     