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


