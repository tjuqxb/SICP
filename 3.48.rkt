;这样同时操作两个账户时的顺序必然是一致的
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (id account1) (id account2))
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))

(define (id-record m)
  (define (add1)
    (set! m (+ m 1)))
  (define (data)
    m)
  (define (dispatch re)
    (cond ((eq? re 'add1)
           (add1))
          ((eq? re 'data)
           (data))))
  dispatch)

(define count (id-record 1))


(define (make-account balance)
  (let ((number #f))
    (set! number (count 'data))
    (count 'add1)
  (define (id)
    number)
  (define (withdraw amount)
    (display "withdraw"))
  (define (deposit amount)
    (display "deposit"))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'id) (id))))
  dispatch))

(define (id account)
  (account 'id))
           
           
(define m1 (make-account 10))
(define m2 (make-account 10))
(define m3 (make-account 10))
(id m1)
(id m2)
((m1 'withdraw) 100)
(newline)
(id m1)
(id m2)
(id m3)