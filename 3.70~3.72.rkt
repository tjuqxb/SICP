(define (merge-weighted weight s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    ((< (weight (stream-car s1))
        (weight (stream-car s2)))
     (cons-stream
      (stream-car s1)
      (merge-weighted weight (stream-cdr s1) s2)))
    ((< (weight (stream-car s2))
        (weight (stream-car s1)))
     (cons-stream
      (stream-car s2)
      (merge-weighted weight s1 (stream-cdr s2))))
    ((= (weight (stream-car s1))
        (weight (stream-car s2)))
     (if 
      (eq-s? (stream-car s1) (stream-car s2))
      (cons-stream
       (stream-car s1)
       (merge-weighted weight (stream-cdr s1) (stream-cdr s2)))
      (cons-stream
      (stream-car s1)
      (cons-stream
       (stream-car s2)
       (merge-weighted weight (stream-cdr s1) (stream-cdr s2))))))))

(define (eq-s? a b)
  (and (= (car a) (car b))
       (= (cadr a) (cadr b))))





(define ones
  (cons-stream 1
               ones))

(define ns
  (cons-stream 1
               (add-streams 
                ones
                ns)))

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

(define int-s
  (ge2 ns ns))

(define (weight-f1 m)
  (+ (car m) (cadr m)))

(define answer1
  (merge-weighted weight-f1 int-s int-s))

(define ff? a
  (cond ((=0 (remainder a 2)) #t)
        ((=0 (remainder a 3)) #t)
        ((=0 (remainder a 5)) #t)
        (else #f)))

(define ss
  (stream-filter (lambda(x) ((or (ff? (car x))
                                (ff? (cadr x))))) int-s))

(define (weight-f2 x)
    (+(* 2 (car x))
      (* 3 (cadr x))
      (* 5 (car x) (cadr x))))

(define answer2
  (merge-weighted weight-f2 ss ss))

int-s

(define (weight-f3 x)
  (+ (* (car x) (car x) (car x))
     (* (cadr x) (cadr x) (cadr x))))


(define as3-pre
  (merge-weighted weight-f3 int-s int-s))


(define (get-1 s)
  (let ((m0 (stream-ref s 0))
        (m1 (stream-ref s 1)))
    (if (= (weight-f3 m0)
           (weight-f3 m1))
        (cons-stream
         (weight-f3 m0)
         (get-1
          (stream-cdr
          (stream-cdr s))))
        (get-1
         (stream-cdr s)))))


(define (simp s)
  (let ((m0 (stream-ref s 0))
        (m1 (stream-ref s 1))
        (if (eq-s? m0 m1)
            (simp (stream-cdr s))
            (cons-stream
             mo
             (simp (stream-cdr s)))))))

(define as3
  (simp (get-1 as3-pre)))

(define (weight-f4 x)
  (+ (* (car x) (car x))
     (* (cadr x) (cadr x))))
           

(define as4-pre
  (merge-weighted weight-f4 int-s int-s))


(define (get2 s)
  (let ((m0 (stream-ref s 0))
        (m1 (stream-ref s 1))
        (m2 (stream-ref s 2))
    (if (and(= (weight-f4 m0)
             (weight-f4 m1))
            (= (weight-f4 m1)
               (weight-f4 m2)))
        (cons-stream
         (list (weight-f3 m0) m0 m1 m2)
         (get-1
          (stream-cdr
           (stream-cdr 
            (stream-cdr s)))))
        (get-1
         (stream-cdr s))))))


