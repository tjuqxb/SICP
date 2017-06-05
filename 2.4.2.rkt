(load "D:/CS/SICP/filter.rkt")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board
  nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons   new-row rest-of-queens))

(define (not-equal-element? s lst)
  (define (iter a b) 
    (if (null? b)
        #t
        (if (= a (car b))
            #f
            (iter a (cdr b)))))  
  (iter s lst)) 


(define (not-able-attack? k lst)
  (define (iter step b)
    (if (null? (cdr b))
           #t
         (if (= (abs(-(car lst) (car(cdr b)))) 
                step)
             #f
             (iter (+ 1 step) (cdr b)))))
  (iter 1 lst))
       

(define (safe? k positions)
  (if (null? (cdr positions))
         #t
        (if (and (not-equal-element?
                  (car positions)
                  (cdr positions))
                 (not-able-attack? k positions))
            #t
            #f)))
                 
  
(display (queens 8))






