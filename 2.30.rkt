(define (square-tree-a m)
  (cond ((null? m)
        nil)
        ((not (pair? m))
         (* m m))
        (else (cons (square-tree-a (car m))
                    (square-tree-a (cdr m))))))

(display (square-tree-a 
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))

(display "\n")

(define (square-tree-b m)
  (map (lambda(sub-tree)
         (if (pair? sub-tree)
             (square-tree-b sub-tree)
             (* sub-tree sub-tree)))
       m))

(display (square-tree-b
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))


(define (tree-map f tree)
  (map (lambda(sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square x)
  (* x x))


(define (square-tree-c tree)
  (tree-map square tree))

(display (square-tree-c
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda(m)
                            (append (list (car s))
                                    m))
                          rest)))))

(display "\n")
(display (subsets (list 1 2 3)))



