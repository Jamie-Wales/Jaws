(define-syntax begin
  (syntax-rules ()
    ((begin expr)
     (expr))
    ((begin expr expr2 ...)
     ((let ((dummy expr))
        (begin expr2 ...))))))

(define-syntax cond
  (syntax-rules (else)
    ((cond) (#f))
    ((cond (else expr ...))
     (begin expr ...))
    ((cond (test expr ... ) clause ...)
     (if test
         (begin expr ...)
         (cond clause ...)))))
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

(define-syntax myLet
  (syntax-rules ()
    ((myLet ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

(define-syntax when
  (syntax-rules ()
    ((when condition body ...)
     ((if condition (body ...) )))))

(define (integer value)
  (lambda (words)
    (if (eqv? words "jamie")
        (display value)
        (display "poop"))))

