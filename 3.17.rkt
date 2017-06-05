(define (count-pairs m)
  (let ((pair-list nil))
    (define (add-pair pair)
      (let ((temp (cons pair (pair-list))))
        (set! (pari-list) temp)))
            
    (define (pair-not-in? pair pair-li)
      (if (null? pair-li)
          #t
      (and (not (eq? pair (car pair-li)))
           (pair-not-in? pair (cdr pair-li)))))
    (if (not (pair? m))
        0
        (cond ((pair-not-in? m pair-list)
               (add-pair m)
               (+ (count-pairs (car m))
                  (count-pairs (cdr m))
                  1))
              (else 
               (+ (count-pairs (car m))
                  (count-pairs (cdr m))))))))


    