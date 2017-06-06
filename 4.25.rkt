(define (unless condition usual-value exceptional-value)
  (if condition
      exceptional-value
      usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))



 (define select-y '(#t #f #t #t)) 
 (define xs '(1 3 5 7)) 
 (define ys '(2 4 6 8)) 
 (define selected (map unless select-y xs ys)) 
 
(map unless (list #t #f #f #t) (list 1 3 5 7) (list 2 4 6 8))

;求值factorial过程体里 unless函数参数里有factorial函数，会求值，求值过程中又引用unless函数，从而使求值过程一直进行，无法终止。
;正则序里，需要求值factorial时函数才会求值，不会出问题。