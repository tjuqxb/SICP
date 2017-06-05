
Y combinator
如何用LAMBDA匿名函数表达递归
我们理解的一个普通递归表达式为（当(g x)到达某个值时，函数会返回一个我们之前规定的值，然后通过我们之前展开的空间进行还原，我们得到所求的值。
(lambda(x)
  (f (self (g x))))

其中self为指向自身的指针，如下方所示。
我们用LAMBDA匿名函数展开这个表达式，发现这是个无限展开的表达式，原因是它采用了self指针。(然而如下这种写法仍然是不合法的，因为这样会陷入无限的loop)


(lambda(x)
  ((lambda(s)
     (f (s (g x))))
   self))

=>

(lambda(x)
  ((lambda(s)
    (f (s (g x))))
    (lambda(x) 
      (lambda(s)
        (f s (g x)))
   ......

我们的目标是寻求这样一个匿名函数的不包含self指针的写法(即它们是等价的LAMBDA表达式，我们所求的不包含self指针，所以是有限的。)
      
                           
令没有 self形式的匿名函数为 
    (lambda(x) (M x))

令 m = (lambda(x)(M x))
由于m是我们之前设定的匿名函数的等价形式,我们可以用m代替原来的自指形式匿名函数self
为了能在最外层代入m，原函数被改写为
   ((lambda(s)
      (lambda(x)
        (f s (g x))))self)
     


用m代替SELF代入上方函数
我们发现该匿名函数也是一个不包含self指针的有限形式了，那么此时的匿名函数和m事实上是等价的。

((lambda (s)
  (lambda (x)
    (f (s (g x)))))m)=m           .....................1

上式可以看出是一个函数接收了函数m，返回了函数m
      
m = (p m) 
我们构造一个k
k = (lambda(a) (p (a a)))
(k k) = (p (k k))

那么 m = (k k)
      
实际上，我们要定义的是(k k)，以f为参数，返回一个不带自指的递归函数
 
(define (rec1 f)
  ((lambda(a)
    (lambda(x)
      (f (a a) x)))
   (lambda(a)
     (lambda(x)
       (f (a a) x)))))

其中f以(f self x) 形式写出,self指代自身，代入rec1之后(a a)替代self能得到递归函数，自身递归下去。
在式1中，k 的形式是

(lambda(a)
  (lambda(x)
    (f ((a a) (g x)))))

(k k)
((lambda(a)
   (lambda(x)
     (f ((a a) (g x)))))
 (lambda(a)
   (lambda(x)
     (f ((a a) (g x))))))

由于f


;;;test
      
(define (recursion f g)
 ((lambda(a)
   (lambda(x)
     (f ((a a) (g x)))))
 (lambda(a)
   (lambda(x)
     (f ((a a) (g x)))))))

(define s1
  (lambda(x)
    (if (= x 1)
        1
        (* 3 x))));s1这样写是错误的，边界条件涉及递归式实际上是无法取得的。

(define s2
  (lambda (x)
    (- x 1)))

((recursion s1 s2) 4)



      
      
      

    
     
      