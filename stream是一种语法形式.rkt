;默认的Racket是要对函数参数进行求值的, 例如(f 1 (+ 1 2))里面,(+ 1 2)要先求值为3,变为(f 1 3)再进行下一步操作.
;因此, Racket若按照SICP使用define关键字来定义延时计算的关键函数delay和cons-stream是不可行的, 需要用宏来定义,绕过求值.

#lang racket

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if already-run?
          result
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)))))

(define-syntax-rule (delay exp) 
    (memo-proc (lambda () exp)))

(define-syntax-rule (cons-stream a b) 
  (cons a (delay b))) 


(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if already-run?
          result
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)))))

(define-syntax-rule (delay exp) 
    (memo-proc (lambda () exp)))

(define-syntax-rule (cons-stream a b) 
  (cons a (delay b)))

(define (stream-map f s)
  (cons-stream (f (stream-car s))
               (stream-map f (stream-cdr s))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(define (ge2 S T)
  (cons-stream (list (stream-car S)
                     (stream-car T))
               (interleave
                (stream-map
                 (lambda(x)
                   (list (stream-car S)
                         x))
                 (stream-cdr T))
                (ge2 (stream-cdr S) (stream-cdr T)))))


(define (ge3 S T U)
  (cons-stream (list (stream-car S)
                     (stream-car T)
                     (stream-car U))
               (interleave
                (stream-map (lambda(x)
                              (cons
                               (stream-car S)
                               x))
                            (ge2 (stream-cdr T) (stream-cdr U)))
                (ge3 (stream-cdr S) (stream-cdr T) (stream-cdr U)))))


(define (stream-filter f? S)
  (if (f? (stream-car S))
          (cons-stream (stream-car S)
                       (stream-filter f? (stream-cdr S)))
          (stream-filter f? (stream-cdr S))))


(define (triples S T U)
  (stream-filter (lambda(x)
                   (= 0
                      (-(+(sqr (car x))
                          (sqr (cadr x)))
                        (sqr (caddr x)))))
                 (ge3 S T U)))
                             

(define (sqr x)
  (* x x))

 



(define s1
  (cons-stream 1
               s1))

(define M
  (cons-stream 1
               (add-stream s1
                           M)))

(triples M M M)
 

                                    