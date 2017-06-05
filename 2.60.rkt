(define (element-of-set? x set)
  (cond ((null? set)
         #f)
        ((euqal? x (car set))
         #t)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (cons x set))


(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1)
             (null? set2))
         nil)
        ((elment? (car set1)
                  set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (intersection-set (cdr set1) set2)))


