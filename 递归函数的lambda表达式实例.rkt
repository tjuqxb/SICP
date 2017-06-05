
(define (rec1 f)
  ((lambda(a)
    (lambda(x)
      (f (a a) x)))
   (lambda(a)
     (lambda(x)
       (f (a a) x)))))

(define s1
  (lambda(s x)
    (if (= 1 x)
        1
        (* x (s (- x 1))))))

((rec1 s1) 4)


;s1代入(rec1 f) (rec1 s1)
(lambda (s x)
  (if (= 1 x)
      1
      (* x ((a a) (- x 1)))))
"""
令k=(lambda(a)
     (lambda(x)
       (s1 (a a)  x)))

将(k k)代入s
s=(k k) =(lambda(x)
           (s1 (k k) x))=((lambda(a)
                           (lambda(x)
                             (f (a a) x)))k)
                        =(rec1 s1) ;这样用(k k)替代S(即self)就能返回原函数

"""
;测试一下另一种变形：发现f的边界条件无法定义  
(define (recursion f g)
 ((lambda(a)
   (lambda(x)
     (f ((a a) (g x)))))
 (lambda(a)
   (lambda(x)
     (f ((a a) (g x)))))))

(define s2
  (lambda(s x)
    (if (= 1 x)
        1
        (* 3 (s (- x 1))))))

((rec1 s2) 3)
    




