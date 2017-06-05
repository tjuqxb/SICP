(define '*unassigned '*unassigned)


((lambda(a)
  (define (f x)
    
    (define (b) (+ (a) x))
    (define (a) 5)
    (+ (a) (b)))
  (f 10)) 1)










;;元循环求值器给出16，元循环求值存在不统一定义的情况，它的模型可以向外找到a。在RACKET环境中会报错,undefined a.
;;本章的set!模式和RACKET一样，会报错，uassigned a
;;只有不先求值，用闭包将其包裹，避免提前eval,才能得到20的答案。
;;需要注意的是，编译器程序中的‘a需写成(quote 'a)格式，可以用（quote? exp) eval出'a这个结果，即加上quote编译器才认为它是我们通常意义上认为的符号'a。
;;否则’a在编译器中会被认为是symbol，即我们通常意义上认为的符号a,会进行环境扫描求值，并报错。