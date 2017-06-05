(define(list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (firs-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define(list-of-values-left exps env)
  (if (no-operands? exps)
      nil
      (let((m false))
        (define (quest)
         (set! m (eval (first-operand exps) env)))
        (quest)
        (cons m
              (list-of-values (rest-operands-left exps) env)))))


(define(list-of-values-right exps env)
  (if (no-operands? exps)
      nil
      (let((m false))
        (define (quest)
         (set! m (list-of-values (rest-operands-left exps) env)))
        (quest)
        (cons (eval (first-operand exps) env)
              m))))


