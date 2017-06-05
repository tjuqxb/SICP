(define (union-set0 set1 set2)
  (define (iter s1 s2 s3)
    (cond ((null? s1)
           (append s3 s2))
          ((null? s2)
           (append s3 s2))
          ((= (car s1)
              (car s2))
           (iter (cdr s1)
                 (cdr s2)
                 (append s3 (list (car s1))
                   )))
          ((<(car s1)
             (car s2))
           (iter (cdr s1)
                 s2
                 (append s3
                         (list (car s1)))))
          (else (iter s1
                      (cdr s2)
                      (append s3
                              (list (car s2)))))))
  (iter set1 set2 nil))



(display (union-set0 (list 1 2) (list 1 3 5 7 9)))



(define (union-set set1 set2)
  (cond ((null? set1)
        set2)
        ((null? set2)
         set1)
        (else
         (let ((m1 (car set1))
               (m2 (car set2)))
           (cond ((= m1 m2)
                  (cons m1
                        (union-set (cdr set1)
                                   (cdr set2))))
                 ((< m1 m2)
                  (cons m1
                        (union-set (cdr set1)
                                   set2)))
                 ((> m1 m2)
                  (cons m2
                        (union-set set1
                                   (cdr set2)))))))))

(newline)
(display (union-set (list 1 2) (list 1 3 5 7 9)))              



        
     