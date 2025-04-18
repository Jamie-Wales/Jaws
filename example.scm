(define (merge-sort lst)
  (define (merge lst1 lst2)
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          ((< (car lst1) (car lst2))
           (cons (car lst1) (merge (cdr lst1) lst2)))
          (else
           (cons (car lst2) (merge lst1 (cdr lst2))))))
  
  (define (split lst)
    (if (or (null? lst) (null? (cdr lst)))
        (values lst '())
        (let-values (((first-half second-half)
                      (split (cddr lst))))
          (values (cons (car lst) first-half)
                  (cons (cadr lst) second-half)))))
  
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let-values (((first-half second-half) (split lst)))
        (merge (merge-sort first-half)
               (merge-sort second-half)))))

(merge-sort '(3 1 4 1 5 9 2 6 5))
