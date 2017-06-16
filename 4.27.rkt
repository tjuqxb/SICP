count->eval 0:0
count->0
count lookup 0
;count lookup variable

'id -> (closure x ((set! count (+ count 1)) x) (env (count 0))) ;call it closure a 




w-> eval (id (id 10)) env(count 0)
         (apply (actual value 'id)
                '(id 10)
                env)
         
         (apply (closure a)
                '(id 10)
                env)
         
         (eval-sequence
          '((set! count (+ count 1))
           x)
          (extend-env
           (x
           (list 'thunk '(id 10) env0))
           (env (count 1))))
         
       (eval x
             (env0
              (x (list 'thunk '(id 10) (env (count 11))))
              (count 1)))
       
w -> (list 'thunk '(id 10) (env (count 11)))

w

(thunk' proc env)

count 1



    