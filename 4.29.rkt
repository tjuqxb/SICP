(define (a x)
  (* x x))

(define (b x)
  (* x x))

(define (c x)
  (* x x))

(c (b (a 30)))
;有记忆时
;response: 100
;count: 1
;无记忆时
;response: 100
;count: 2

