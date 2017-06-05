(define (f-accumulate filter? value-null combiner term a next b)
  (define (iter a s)
    (if (> a b) 
        s
        (if  (filter?　(term a))
               (iter (next a) (combiner (term a) s))
              (iter (next a) s))))
  (iter a value-null))




(define (prime-accumulate a b)
  (define (term a)
    a)
  (define (next a)
    (+ a 1))
  (f-accumulate prime? 0 + term a next b))

(define (prime? n)
  (define (prime-in n)
    (define (test? n a)
      (if (> (* a a) n) 1
          (if (= (remainder n a) 0) 0
              (test? n (+ a 2)))))

    (if (= n 2) 1
        (if (= (remainder n 2) 0) 0
            (test? n 3))))
  (= (prime-in n) 1))





(define (product-gcd n)
  (define (term a)
    a)
  (define (next a)
    (+ 1 a))
  (define (gcd? a)
    (gcd-base? n a))
  (f-accumulate gcd? 1 * term 1 next (- n 1)))


(define (gcd-base? n a)
  (define (gcd-a n a)
    (if (= 0 a) n
        (gcd-a a (remainder n a))))
  (= (gcd-a n a) 1))

(product-gcd 10)
(prime? 8)