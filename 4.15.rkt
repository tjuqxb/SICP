(define (run-forever) (run-forever))

;(halts? p a)为真，则程序会有正常返回值，为假则永远运行或错误

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(try try)


(try try)
(if (halts? try try)
    (run-forever)
    'halted)

;(try try)如果永远运行或错误，那么(halts? try try)为假，那么(try try)程序内部来看应该‘halted,与前提矛盾。
;从有正常返回值的前提推导来看也是矛盾的。

