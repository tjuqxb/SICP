;;added to polynomial package
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


(define (remainder-terms a b)
  (cadr (div-terms a b)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (ramainder-terms a b))))

(define (gcd-poly a b)
  (let ((m1 (term-list a))
        (m2 (term-list b)))
    (make-poly (var a) (gce-terms m1 m2))))

(put 'gcd ('polynomial 'polynomial ) gcd-poly)



;;modify the rational package
(define (make-rat n d)
  (cond ((or
          (and (poly? n)
               (poly? d))
          (and (number? n)
               (number? d)))
           (let ((g (gcd n d)))
           (cons (div n g) (div d g))))
        ((and (number? n)
             (poly? d))
         (let ((g (gcd (iter-change n (var d)) d)))
           (cons (div n g) (div d g))))
        ((and (poly? d)
              (number? n))
         (let ((g (gcd n (iter-change d (var n)))))
           (cons (div n g) (div d g))))
         (else error"can not make rat--MAKE-RAT"(list n d))))



         







