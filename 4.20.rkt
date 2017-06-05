(load "4.16.rkt")

(define (letrec-parameters exp)
  (let-parameters exp))

(define (letredc-vals exp)
  (let-vals exp))

(define (letrec-body exp)
  (let-body exp))

(define (letrec->combination exp)
  (set! parameters (letrec-parameters exp))
  (define (transform vars vals body out-vals)
    (cond ((null? vars)
           (cons (make-lambda parameters
                              body)
                 out-vals))
          ((null? out-vals)
           (transform (cdr vars) (cdr vals)
                      (cons (list 'set! (car vars) (car vals))
                            body)
                      (cons (quote '*unassigned) out-vals)))
          (else
           (set-cdr! body
                     (cons (list 'set (car vars) (car vals))
                          (cdr body)))
           (set-cdr! out-vals
                     (cons (quote '*unassigned) (cdr out-vals)))
           (transform (cdr vars) (cdr vals) body out-vals))))
  (transform parameters (letrec-vals exp) (letrec-body exp) nil))

;;define 不会多创建一个环境，因此有可能将原环境的同名变量（即f的形参）覆盖
;;let 和let-rec都会创建新的环境，但let的eval环境中不包括本层环境变量，无法本层递归定义，只会向f的形参和更高层次去寻找绑定，也无法做出*’unassigned警报。
;;let-rec的eval环境是本层环境，所以可以有'*unassigned警报，也可以在本层中实现递归。