;;a
((lambda(n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
   10)
 
((lambda (fact)
  (fact fact 10))
 (lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1))))))



((lambda (ft k)
  (if (= k 1)
      1
      (* k (ft ft (- k 1)))))
 (lambda (ft k)
  (if (= k 1)
      1
      (* k (ft ft (- k 1)))))
 10)

(if (= 10 1)
    1
    (* 10 ((lambda(ft k)
             (if (= k 1)
             1
             (* k (ft ft (- k 1)))))
           (lambda(ft k)
             (if (= k 1)
                 1
                 (* k (ft ft (- k 1)))))
           9)))


((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (cond ((= k 0)
             0)
            ((= k 1)
             1)
            (else 
             (+ (ft ft (- k 1))
                (ft ft (- k 2))))))))
 3)


;;b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t
         (od? od? ev? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f
         (od? od? ev? (- n 1))))))

(f 1)
(f 2)
(f 3)


                 



 