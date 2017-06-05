;程序本身逻辑上并没有问题，然而由于采用了非cons-stream开头的程序语句，没有DELAY延时会造成对实参的不断求值从而陷入循环之中。
;分析如下
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;interleave会先对S1做判断，再用cons-stream生成一个首项为(stream-car s1)的流，由于它引用的是cons-stream,只有用stream-cdr
;才能继续对(interleave s2 (stream-cdr s1))求值
(define (pairs0 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interlieave 
    (stream-map (lambda(x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
;以cons-stream开头，只有不断地用stream-cdr驱动才能对第二项求值，引发对(pairs (stream-cdr s) (stream-cdr t))的求值
(define (pairs1 s t)
  (interleave
   (stream-map (lambda(x)
                 (list (stream-car s) x) t)
               (pairs1 (stream-cdr s) (stream-cdr t)))))
;函数体中，第二个实参会引发对(pairs1 (stream-cdr s) (stream-cdr t))的求值，而这种对实参的求值会引发连续求值，由于没有使用
;delay阻断，它不会得出结果，会无限循环下去。

