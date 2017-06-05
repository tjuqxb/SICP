(define (loop-end? m)
  (define (iter mm n)
   (cond ((not (pair? mm)) #f)
         ((not (pair? (cdr mm))) #f)
         ((not (pair? (cdr(cdr mm)))) #f)
         ((eq? mm (cdr mm)) #t)
         ((eq? mm (cdr (cdr mm))) #t)
         ((not (pair? ((walk-m-2 m) n))) #f)    
         ((eq? mm ((walk-m-2 m) n)) #t)
         (else (iter (cdr mm) (+ 1 n))))) 
  (iter m 1))
"""        
(define (loop-end? m)
  (define (iter mm nn)
   (cond ((not (pair? mm)) #f)
         ((not (pair? (cdr mm))) #f)
         ((not (pair? (cdr(cdr mm)))) #f)
         ((eq? mm (cdr mm)) #t)
         ((eq? mm (cdr (cdr mm))) #t)
         ((not (pair? (safe-cdr(safe-cdr nn)))) #f)    
         ((eq? mm (safe-cdr(safe-cdr nn))) #t)
         (else (iter (cdr mm) (safe-cdr(safe-cdr nn)))))) 
  (iter m (safe-cdr(safe-cdr m))))
                
(define (safe-cdr m)
  (if (not(pair? m))
      nil
      (cdr m)))
"""



(define (walk m step) 
  (define (iter0 m step)
    (cond ((not (pair? m))
          nil) 
        ((= 1 step)
        (cdr m))
        (else (iter0 (cdr m) (- step 1))))) 
 (define (iter1 n)
  (define (iter2 mm n)
    (cond ((= 0 n)
           mm)
          ((= 1 n)
           (iter0 mm step))
        (else (iter2 (iter0 mm step) (- n 1)))))
   (iter2 m n))
  iter1)
   

(define (walk-m-1 m) 
  (walk m 1))         ;以n为变量，每次递进1步的函数

(define (walk-m-2 m)
  (walk m 2))         ;以n为变量，每次递进2步的函数

 ; Tested with mzscheme implementation of R5RS: 
 (define x '(1 2 3 4 5 6 7 8)) 
 (define y '(1 2 3 4 5 6 7 8)) 
 (set-cdr! (cdddr (cddddr y)) (cdddr y)) 
 (define z '(1)) 
 (set-cdr! z z) 
 x ; (1 2 3 4 5 6 7 8) 
 y ; (1 2 3 . #0=(4 5 6 7 8 . #0#)) 
 z ; #0=(1 . #0#) 
 (loop-end? x) ; #f 
 (loop-end? y) ; #t 
 (loop-end? z) ; #t 
((walk-m-2 x) 3)

