"""
(define (loop? m)
  (let ((pair-list (list m)))
    (define (add-pair pair)
      (set! (pair-list)
            (cons pair pair-list)))
    (define (judge? m)
      (if (null? (cdr m))
          #f
          (cond ((not (memq (cdr m) pair-list))
                 (add-pair (cdr m))
                 (judge? (cdr m)))
                (else #t))))
    (judge? m)))
"""
 

(define (loop? m)
  (define (iter lis rec)
    (if (not (pair? lis))
        #f
        (cond ((memq lis rec)
               #t)
              (else(or (iter (car lis)
                        (cons lis rec))
                   (iter (cdr lis)
                        (cons lis rec)))))))
  (iter m nil))

    