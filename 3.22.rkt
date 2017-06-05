;procedure 一个过程，单指这个过程
;(procedure) 一个无参数过程，并被执行

(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))
    
    
    (define (set-front-ptr! item)
      (set! front-ptr item))
      
    
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    
    (define (empty?)
      (null? front-ptr))
     
    
    (define (front-queque)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" queue)
          (car front-ptr)))
    
    (define (insert-queue! item)
      (let((new-pair (cons item nil)))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    
      (define (delete-queue!)
      (cond ((empty?)
            (error"DELETE called with an empty queue" queue))
            (else
             (let ((re 
                    ( cdr front-ptr)))
             (set-front-ptr! re)))))
             
                   
    
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else (error "Unkown-requeset"m))))
    dispatch))

(define (front-ptr m) (m 'front-ptr))
(define (rear-ptr m)  (m 'rear-ptr))
(define (set-front-ptr! m) (m 'set-front-ptr!))
(define (set-rear-ptr! m) (m 'set-rear-ptr!))
(define (empty? m) (m 'empty?))
(define (front-queque m) (m 'front-queue))
(define (insert-queue! m) (m 'insert-queue!))
(define (delete-queue! m) (m 'delete-queue!))
(define (print-queue queue)
  (display (front-ptr queue)))

(define q1 (make-queue))
(print-queue q1)
(newline)
((insert-queue! q1) 'a)
(newline)
(print-queue q1)
(newline)
((insert-queue! q1) 'b)
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

