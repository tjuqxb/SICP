(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons nil nil))

(define (front-queue queue)
  (if (empty-queue? queque)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
;因为后端指针在删除过程中不做修改，包括在成为空表前的一步，所以依然留下了。

(define (print-queue queue)
  (display (front-ptr queue)))
(define q1 (make-queue))
(print-queue q1)
(newline)
(insert-queue! q1 'a)
(newline)
(print-queue q1)
(newline)
(insert-queue! q1 'b)
(newline)
(print-queue q1)
(newline)
(delete-queue! q1)
(newline)
(print-queue q1)
(newline)
(delete-queue! q1)
(newline)
(print-queue q1)

