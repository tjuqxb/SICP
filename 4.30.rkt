;(proc (car items))
;((list 'thunk proc) (car (list 'thunk items-list)))
;eval上面这个表达式，EVAL认为它是一个application,函数是以actual value来EVAL的，而car 作为基本过程，item-list也可以以actual-value EVAL.
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))


(p1 1)

(1 . 2)

;(p2 1) 会报错
;修改后为1 2.

;正确是因为(force-it obj)不是thunk的话会返回obj

;Cy是对的。SEQUENCE前置语句直接求值应该的，且应该配合槽来使用。





