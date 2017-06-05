(define (poly? m)
  (eq? ('poly) (car m)))


(define (coeff-list terms)
  (map coeff terms))

(define (all-not-poly? co)
  (if (null? co)
      #t
      (and (not (poly? (car co)))
           (all-not-poly? (cdr co)))))

(define (make-poly var terms)
  (cons 'poly (list var terms)))

(define empty-term-list nil)

(define (iter-change m var)
  (let ((terms (term-list m)))
    (if (null? (term-list m))
        (make-poly var empty-term-list)
        (if (all-not-poly? (coeff-list terms))
            (make-poly var (list (make-term 0 m)))
            (if (not (poly? (coeff (car terms))))
                (make-poly var (add-terms (list (make-term 0
                                                           (make-poly (var m) (list (car terms)))))
                                          (term-list (iter-change (make-poly (var m) (cdr terms)) var))))
                (let((new-coeff (iter-change (coeff (car terms)) var))) 
                  (if (eq? (var (coeff (car terms))) var)
                         (make-poly var (add-terms (mul-terms (term-list (coeff (car terms)))
                                                              (make-term 0 (make-poly (var m) (list (car terms)))))
                                                   (term-list (iter-change (make-poly (var m) (cdr terms)) var))))
                         (make-poly var (add-terms (mul-terms (term-list new-coeff)
                                                              (make-term 0 (make-poly (var m) (list (car terms)))))
                                                   (term-list (iter-change (make-poly (var m) (cdr terms)) var)))))))))))


;;modify the rational package
(define (make-rat n d)
  (if (and (number? n)
           (number? d))
      (let ((g (gcd n d)))
        (cons (div n g) (div d g)))
      (cons n d)))

(define (add-rat x y)
  (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
            (mul (denom x) (denom y))))







                                        


      

