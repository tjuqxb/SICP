(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))


(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if(leaf? tree)
     (list (symbol-leaf tree))
     (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree))

(define (encode message tree)
  (if (null? message)
      nil 
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
 (cond ((not(contain? s (symbols tree)))
          (error "tree does not contain symbol" s ))
         ((and (leaf? tree)
               (eq? s (symbol-leaf tree)))
          nil)
         (else (if (contain? s (symbols (left-branch tree)))
                      (cons '0 (encode-symbol s (left-branch tree)))
                      (cons '1 (encode-symbol s (right-branch tree)))))))


(define (contain? s set)
  (cond ((null? set)
       #f)
      ((eq? s (car set))
       #t)
      (else (contain? s (cdr set)))))

(display (encode '(A D A B B C A) sample-tree))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (cond ((null? leaves)
          nil)
        ((null? (cdr leaves))
         (car leaves))
        (else
         (successive-merge 
          (adjoin-set (make-code-tree
                        (car leaves)
                        (cadr leaves))
                        (cddr leaves))))))

(display (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))


(define rock-tree (generate-huffman-tree '((a 2) (na 16) (boom 1) (Sha 3) (Get 2) (yip 9) (job 2) (Wah 1))))
(newline)
(display (encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom) rock-tree))
(newline)
(display (length (encode '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom) rock-tree)))    
(newline)         
(display (length '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom)))
                                 
                                      
      