(define-syntax begin
  (syntax-rules ()
    ((begin expr)
     expr)
    ((begin expr expr2 ...)
     (let ((dummy expr))
       (begin expr2 ...)))))

(define-syntax cond
  (syntax-rules (else)
    ((cond) #f)
    ((cond (else expr ...))
     (begin expr ...))
    ((cond(test expr ... ) clause ...)
     (if test
         (begin expr ...)
         (cond clause ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (let () body ...))
    ((let* ((var1 val1) (var2 val2) ...) body ...)
     (let ((var1 val1))
       (let* ((var2 val2) ...) body ...)))))
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
(define-syntax when
  (syntax-rules ()
    ((when condition body ...)
     ((if condition (body ...) )))))

(define (integer value)
  (lambda (words)
    (if (eqv? words "jamie")
        (display value)
        (display "poop"))))

