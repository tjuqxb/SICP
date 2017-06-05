(define (append-s a b)
  (if ((null? a)
       b)
     (cons (car a) (append-s (cdr a) b))))
   
(define (reverse x)
  (define (iter a b)
   (cond  ((null? a)
           b)
          ((not (pair? a))
           (cons a b))
                
      (else (iter (cdr a)
             (cons (car a) b)))))
  (iter (cdr x) (car x)))

(display (cons 1 2)) 
(display "\n")
       
(display (list (cdr (list 2))))
(display "\n")
             

(define m
  (list 1 2 3 4))

(display (reverse m))
(display "\n")
(display (reverse (list 1 nil)))
(display "\n")

(define (deep-reverse-a x)
    (cond ((null? x)
           nil)
          ((not (pair? x))
           x)
          (else (reverse (list (deep-reverse-a (car x))
                               (deep-reverse-a (cdr x)))))))
         


(display (deep-reverse-a (cons m (cons 5 6))))
                   
(display  "\n")

           
           
(define x (list (list 1 2) (list 3 4)))

(display (deep-reverse-a x))
(display "\n")


(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) 
               (list x))
        (else 
               (append (fringe (car x))
                       (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))



(display (fringe (list x x)))

(display "\n")

(display (deep-reverse-a (list (list 1 2) (list 3 4) (list 5 6))))

(define (tree-reverse lst)
    (define 
      (iter remained-items result)
        (if (null? remained-items)
            result
            (iter (cdr remained-items)
                  (cons (if (pair? (car remained-items))
                            (tree-reverse (car remained-items))
                            (car remained-items))
                        result))))
    (iter lst nil))
 
(display "\n")
(display (tree-reverse (list m (list 5 6))))
      
          

