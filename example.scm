(define (simple-for-each proc lst)
  (if (not (null? lst)) ; Check if the list is not empty
      (begin
        (proc (car lst)) ; Apply proc to the first element
        (simple-for-each proc (cdr lst)))))
(define (find-first pred lst)
  (call/cc
   (lambda (return)
     (simple-for-each
      (lambda (x)
        (if (pred x)
            (return x)))
      lst)
     #f)))

(find-first even? '(1 3 5 6 7 8 9))

(define (divide a b)
  (call/cc
   (lambda (k)
     (if (= b 0)
         (k '(error "Division by zero"))
         (k `(ok ,(/ a b)))))))
(define (handle-result result)
  (if (eq? (car result) 'error)
      (cadr result)
      (cadr result)))

(handle-result (divide 10 2))

(handle-result (divide 10 0))
