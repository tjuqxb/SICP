(define (assoc key records)
  (cond ((null? records) #F)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))


(define (lookup key-list table)
  (define (iter list table)
    (cond  ((null? list)
            false)
           ((null? (cdr list))
            (let ((record (assoc (car list) (cdr table))))
             (cond ((and record (not (pair? (cdr record))))
                    (cdr record))
                   ((and record (not (pair? (cadr record))))
                    (cadr record))
                   (else
                    false))))
           ((pair? (cdr list))
            (let ((subtable (assoc (car list) (cdr table))))
              (if (and subtable (pair? (cdr subtable)))
                  (iter (cdr list) (cdr subtable))
                  false)))))
  (iter key-list table))


(define (insert! key-list value table)
  (cond ((null? (cdr key-list))
         (let ((record (assoc (car key-list) (cdr table))))
           (if record
           (cond ((pair? (cdr record))
                  (set-cdr! record (cons value (cdr record))))
                 ((not (pair? (cdr record)))
                  (set-cdr! record value)))
           (set-cdr! table (cons (cons (car key-list) value) (cdr table))))))
        ((pair? key-list)
         (let ((subtable (assoc (car key-list) (cdr table))))
           (if subtable
            (cond ((pair? (cdr subtable))
                  (insert! (cdr key-list) value (cdr subtable)))
                  ((null? (cdr subtable))
                  (begin (set-cdr! subtable (cons (cons (cadr key-list) nil) (cdr subtable)))
                  (insert! (cdr key-list) value (cdr subtable))))
                  (else
                   (set-cdr! subtable
                            (list (cdr subtable)
                                  (cons (cadr key-list) nil)))
                   (insert! (cdr key-list) value (cdr subtable))))
            (begin (set-cdr! table (cons (cons (car key-list) nil) (cdr table)))
            (insert! key-list value table)))))))



         
                
  
    
   
(define (make-table . table-name) 
    (if (null? table-name)
        (list '*table*)
        table-name))
    
    
(define t (make-table))

;Value: t

(insert! (list 'a-single-key) 10086 t)                                               ; 一维表插入和查找

;Value 26: (*table* (a-single-key . 10086))

(lookup (list 'a-single-key) t)

;Value: 10086

(insert! (list 'key-1 'key-2 'key-3) 123 t)

;Value 26: (*table* (key-1 (key-2 (key-3 . 123))) (a-single-key . 10086))           ; 三维表插入和查找

(lookup (list 'key-1 'key-2 'key-3) t)

;Value: 123

(insert! (list 'key-1 'key-2 'key-3) 'hello-moto t)                           ; 修改三维表中元素的值

;Value 26: (*table* (key-1 (key-2 (key-3 . hello-moto))) (a-single-key . 10086))

(lookup (list 'key-1 'key-2 'key-3) t)

;Value: hello-moto

                       