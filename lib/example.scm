(define map1
  (lambda (p ls)
    (if (null? ls)
        ls
        (cons (p (car ls))
              (map1 p (cdr ls))))))

(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))
