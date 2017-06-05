(define (group a b c)
  (define (set-head! item)
    (set! a item))
  (define (set-body! item)
    (set! b item))
  (define (set-end! item)
    (set! c item))
  (define (dispatch m)
    (cond ((eq? m 'head) a)
          ((eq? m 'body) b)
          ((eq? m 'end) c)
          ((eq? m 'set-head!) set-head!)
          ((eq? m 'set-body!) set-body!)
          ((eq? m 'set-end!) set-end!)
          (else (error"no such command"))))
  dispatch)

(define (set-head! mm item)
  ((mm 'set-head!) item))

(define (set-body! mm item)
  ((mm 'set-body!) item))

(define (set-end! mm item)
  ((mm 'set-end!) item))

(define (head mm)
  (mm 'head))

(define (body mm)
  (mm 'body))

(define (end mm)
  (mm 'end))

(define (make-deque)
  (group nil nil nil))

(define (empty-deque? mm)
  (null? (body mm)))

(define (front-deque mm)
  (body mm))

(define (rear-deque mm)
  (end mm))

(define (insert-front-deque! item mm)
  (let ((g1 (group nil item nil))) 
   (cond ((empty-deque? mm)
          (set-head! mm g1)
          (set-body! mm g1)
          (set-end! mm g1))
         ((null? (head (body mm)))
          (set-end! g1 (body mm))
          (set-head! mm g1)
          (set-head! (body mm) g1))
         (else
          (set-head! (head mm) g1)
          (set-end! g1 (head mm))
          (set-head! mm g1)))))

(define (insert-rear-deque! item mm)
  (let ((g1 (group nil item nil)))
    (cond ((empty-deque? mm)
           (set-head! mm g1)
           (set-body! mm g1)
           (set-end! mm g1))
          ((null? (end (body mm)))
           (set-head! g1 (body mm))
           (set-end! mm g1)
           (set-end! (body mm) g1))
          (else
           (set-end! (end mm) g1)
           (set-head! g1 (end mm))
           (set-end! mm g1)))))

(define (delete-front-deque! mm)
  (cond ((empty-deque? mm)
         (error"no elements"))
        ((and (null? (end (body mm))) (null? (head (body mm))))
         (set-head! mm nil)
         (set-body! mm nil)
         (set-end! mm nil))
        ((null? (head (body mm)))
         (set-head! (end (body mm)) nil)
         (set-head! mm (end (body mm)))
         (set-body! mm (end (body mm))))
        (else 
         (set-head! (end (head mm)) nil)
         (set-head! mm (end (head mm))))))

(define (delete-rear-deque! mm)
  (cond ((empty-deque? mm)
         (error"no elements"))
        ((and (null? (head (end mm))) (null? (end (head mm))))
         (set-head! mm nil)
         (set-body! mm nil)
         (set-end! mm nil))
        ((null? (end (body mm)))
         (set-end! (head (body mm)) nil)
         (set-end! mm (head (body mm)))
         (set-body! mm (head (body mm))))
        (else
         (set-end! (head (end mm)) nil)
         (set-end! mm (head (end mm))))))

(define (print0 mm)
  (cond ((not (procedure?  mm))
         (display mm))
        (else
         (display (body mm))
         (print0 (end mm)))))

(define (print mm)
  (print0 (head mm)))

(define q (make-deque)) 
(print q)
; 创建队列
(newline)
;Value: q
(insert-front-deque! 2 q)     ; 插入三个元素
(print q)
(newline)
;Value 11: ((2) 2)
(insert-front-deque! 1 q)
(print q)
;Value 11: ((1 2) 2)
(newline)
(insert-rear-deque! 3 q)
(print q)
(newline)
(insert-front-deque! 4 q)
(print q)
(newline)
(insert-rear-deque! 5 q)
;Value 11: ((1 2 3) 3)
(newline)

(print q)
;Value 12: (1 2 3)
(newline)
(delete-front-deque! q)       ; 从前端删除
(print q)
;Value 11: ((2 3) 3)


(newline)
;Value 13: (2 3)
(delete-rear-deque! q)        ; 从后端删除

;Value 11: ((2) 2)

(print q)
(newline)
;Value 13: (2)

(empty-deque? q)              ; 空队列测试

;Value: #f
(newline)
(delete-rear-deque! q)
(print q)
(delete-rear-deque! q)
(delete-front-deque! q)
(newline)

;Value 11: (() 2)

(empty-deque? q)

;Value: #t      
        

