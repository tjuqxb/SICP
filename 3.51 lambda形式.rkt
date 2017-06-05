(define (constream a b)
  (cons a 
        (delay b)))

(define (memo proc)
  (let ((cal? #f)
        (result #f))
    (lambda()
      (if (not cal?)
          (begin (set! result (proc))
                 (set! cal? #t)
                 result)
          result))))

(define (delay b)
  (memo-proc (lambda() b)))
;the original delay procedure was set as below
(define (delay0 b)
  (lambda() 
    b))

(define (stream-cdr m)
  (force (cdr m)))

(define (force b)
  (b))

(define (stream-car m)
  (car m))

;The (lambda() x) form has been ultilized twice to prevent the calculation of memo process and b process. 

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda()
      (if (not alreasy-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result) ;第一遍运行时返回计算过的result
          result)))) ;第二遍运行直接返回result 

;确定同一性的方法
(define q (m x))
;此后对q的引用一定是同一个对象
(let ((q (m x)))
;let内部对q的引用一定是同一个对象

