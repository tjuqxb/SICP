(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'mag-part) (* r (sin a)))
          (else
           (error "Unkown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)


;;显式操作 都不适合
;;数据导向 加入新类型 定义新的INSTALL-PACKAGE   加入新操作 定义函数 使用PUT   改变数据类型，可以使用TAG操作  （适合加入新类型，可以改变已有数据类型，也比较适合加入新操作）
;;消息传递 加入新类型 定义新函数   加入新操作 加入一个COND   无法使用TAG改变数据类型  （不适合加入新操作，会涉及到重新实例化的问题）   