(define (last-pair-iter x)
  (if (null? (cdr x))
      x
      (last-pair-iter (cdr x))))


(last-pair-iter (list 23 34))

(define (reverse x)
  (if (null? (cdr x))
       x
      (cons (reverse (cdr x)) (car x))))


(reverse (list 1 5 8 9))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append(cdr list1) list2))))

(append (list 1 5 5 9) (list 4 6))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                 (first-denomination coin-values))
                coin-values)))))
     

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null?  coin-values))

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins)

(define (same-parity x . w)
  (define (iter a s)
  (let (( m  (remainder x 2)))
    
     (if (null? s)
        a
        (iter  (cons a (if (= (remainder (car s) 2) m)
                                (car s)
                                nil))
                     (cdr s)))))
  (iter x w))
  
  
   

(same-parity 2 3 4 5 6 7)
                            
      

