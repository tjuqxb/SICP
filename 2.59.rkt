(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (union-set set1 set2)
  (cond ((null? set1)
         set2)
        ((element-of-set? 
          (car set1)
          set2)
         (union-set (cdr set1)
                    set2))
        (else (union-set
               (cdr set1)
               (cons (car set1) set2)))))

(display (union-set '(2 3 5) '(2 4 7)))

