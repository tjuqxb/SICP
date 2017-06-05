(define (fold-right op initial lst)
  (if (null? lst)
      initial
      (op (car lst)
          (fold-right op initial (cdr lst)))))


(define (fold-left op initial lst)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial lst))

(fold-right / 1 (list 1 2 3))
(newline)
(/ 3 2)

(newline)
(fold-left / 1 (list 1 2 3))
(newline)
(/ 1 6)
(newline)
(display (fold-right list nil (list 1 2 3)))
(newline)
"(1(2(3 nil)))"
(newline)
(display (fold-left list nil (list 1 2 3)))
(newline)
"(((nil 1) 2) 3)"
"""
(= (op a (op b c)) (op (op a b) c))
"""
(define (reverse sequence)
  (fold-right (lambda(x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda(x y) (cons y x)) nil sequence))

 
