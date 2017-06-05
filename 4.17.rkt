;;由let作为lambda的语法糖，编译时会作为compound-procedure求值，lambda 会 make-procedure，求值时会增加一个框架。
;;求值不会不同，因为只是同样的值绑定到深度更深一层的框架中，根据look-up定义，不会出现不同行为方式。
;;make-procedure时让它自带一个空框架，scan将define的绑定填充进去，其他函数体不动，求值时不新建框架，将求值参数绑定加入到这个define框架中，这样整体来看就没有增加框架。