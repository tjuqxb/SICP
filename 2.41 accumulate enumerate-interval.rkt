(define (accumulate op initial lst)
  (if (null? lst)
      initial
      (op (car lst)
          (accumulate op initial (cdr lst)))))

(define (enumerate-interval a b)
  (define (iter x y)
    (if (< x a)
        y
        (iter (- x 1)
              (cons x y))))
  (iter b nil))


(define (equal-s? lst s)
  (= (accumulate + 0 lst)
     s))  ;the evaluation function

(define (generate n)
  (map (lambda(i)
      (map (lambda(j)
           (map (lambda(k)
                (list i j k))
                (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n))) ;to genrate the structured list


(define (flatten list)
  (accumulate append nil list))

(define (double-flatten list) 
  (flatten (flatten list)))

 
(define (search n s)
  (filter (lambda(lst)
            (equal-s? lst s))
          (double-flatten (generate n))))


(define (filter f? s)
  (cond ((null? s)
        nil)
        ((f? (car s))
         (cons (car s) (filter f? (cdr s))))
        (else (filter f? (cdr s)))))


(display (search 6 10))

  
          
         
           



  