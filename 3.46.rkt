;FALSE代表过程可以运算，同时访问，在其中一个程序SET为TRUE之前都取得FALSE状态，跳出acquire过程，进入((lambda(val) (mutex 'release) val))(apply p args))
;val值同时被两个过程干扰。