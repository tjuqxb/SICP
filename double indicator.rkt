(define (make-deque)
  (cons nil (cons nil nil)))

(define (empty-deque? queue)
  (null? (car queue))) 


(define (front-deque queue)
  (car queue))

(define (rear-deque queue)
  (cadr queue))


(define (insert-front-deque! item queue)
  (let ((new-pair (list item)))
   (cond ((empty-deque? queue)
          (set-car! queue new-pair)
          (set-car! (cdr queue) new-pair)
          (set-cdr! (cdr queue) new-pair))
         (else
          (set-car! queue (cons item (car queue)))
          (set-cdr! (cdr queue) (cons (cddr queue) (car queue)))))))

(define (insert-rear-deque! item queue)
  (let ((new-pair (list item)))
    (cond ((empty-deque? queue)
           (set-car! queue new-pair)
           (set-car! (cdr queue) new-pair)
           (set-cdr! (cdr queue) new-pair))
          (else
           (set-cdr! (cadr queue) new-pair)
           (set-car! (cdr queue) new-pair)
           (set-cdr! (cdr queue) (cons new-pair (cddr queue)))))))


(define (delete-front-deque! queue)
  (cond ((empty-deque? queue)
         (error"empty-queue"))
        (else
         (set-car! queue (cdr (car queue))))))


(define (delete-rear-deque! queue)
  (cond ((empty-deque? queue)
         (error "empty-queue"))
        (else
         (set-cdr! (cadddr queue) nil)
         (set-cdr! (cdr queue) (cdddr queue))
         (set-car! (cdr queue) (caddr queue)))))

(define (print queue)
  (display (car queue)))


           

;Loading "23-deque.scm"... done
;Value: delete-rear-deque!

(define q (make-deque)) 
(print q)
; 创建队列

;Value: q
(insert-front-deque! 2 q)     ; 插入三个元素
(print q)
;Value 11: ((2) 2)
(insert-front-deque! 1 q)
(print q)
;Value 11: ((1 2) 2)

(insert-rear-deque! 3 q)
(print q)
;Value 11: ((1 2 3) 3)


(print q)
;Value 12: (1 2 3)

(delete-front-deque! q)       ; 从前端删除
(print q)
;Value 11: ((2 3) 3)



;Value 13: (2 3)
(delete-rear-deque! q)        ; 从后端删除

;Value 11: ((2) 2)

(print q)

;Value 13: (2)

(empty-deque? q)              ; 空队列测试

;Value: #f

(delete-rear-deque! q)
(print q)

;Value 11: (() 2)

(empty-deque? q)

;Value: #t      
        