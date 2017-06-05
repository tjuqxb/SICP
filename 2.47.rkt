(define (make-frame-a origin edge1 edge2)
  (list origin edge1 edge2))

(define (edge1-frame-a frame)
  (cadr frame))

(define (edge2-frame-a frame)
  (caddr frame))

(define (make-frame-b origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (edge1-frame-b frame)
  (cadr frame))

(define (edge2-frame-b frame)
  (cddr frame))


