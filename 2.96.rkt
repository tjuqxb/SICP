(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the empty-term-list))
      (let(((t1 (first-term L1))
            (t2 (first-term L2)))
           (if (>(order t2) (order t1))
               (list (the-empyt-termlist) L1)
               (let ((new-c (div (ceff t1) (c0eff t2)))
                     (new-o (- (order t1) (order t2))))
                 (let ((rest-of-result
                        (div-terms (sub-terms L1
                                              (mul-terms (list (make-term new-o new-c))
                                                         L2))
                                   L2)))
                   (list (add-terms (list (make-term new-o new-c)) (car rest-of-result))
                         (cadr rest-of-result)))))))))

(define (mul-c L1 L2)
  (let ((O1 (order (car L1)))
        (O2 (order (car L2)))
        (c (coeff (car L2))))
    (let ((cm (exp c (+ 1 (- O1 O2)))))
      (mul-terms (list (make-term 0 cm))
                 L1))))

(define (pseudoremainder-terms L1 L2)
  (cadr (div-terms (mul-c L1 L2) L2)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      (cull-coeff a)
      (gcd-terms b (pseudoremainder-terms a b))))

(define (coeff-list terms)
  (map coeff terms))

(define (cull-coeff a)
  (let ((m (coeff-list a)))
    (let ((n (gcd-m m)))
      (map (lambda(term) (make-term (order term) (/ (coeff term) n))) a))))

(define (gcd-m m)
  (cond ((null? m)
         error"gcd not exist--GCD-M")
        ((null? (cdr m))
         (car m))
        ((else (gcd (car m) (gcd-m (cdr m)))))))


                