(define-syntax do
 (syntax-rules ()
  ((do ((var init step ...) ...)
    (test expr ...)
    command ...)
   (letrec
    ((loop
      (lambda (var ...)
       (if test
        (begin
         (if #f #f)
         expr ...)
        (begin
         command
         ...
         (loop (do "step" var step ...)
          ...))))))
    (loop init ...)))
  ((do "step" x)
   x)
  ((do "step" x y)
   y)))

(define-syntax begin
  (syntax-rules ()
    ((begin exp ...)
     ((lambda () exp ...)))))

(define (values . things)
  (call-with-current-continuation
   (lambda (cont) (apply cont things))))

(define call-with-values
  (lambda (producer consumer)
    ((lambda (args)
       (apply consumer args))
     (producer))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))



(define-syntax let*
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((name value)) body ...)
     (let ((name value))
       body ...))
    ((_ ((name1 value1) (name2 value2) rest ...) body ...)
     (let ((name1 value1))
       (let* ((name2 value2) rest ...)
         body ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bin"
       (binding ...) () (begin body0 body1 ...)))
    ((let-values "bind" () tmps body)
     (let tmps body))
    ((let-values "bind" ((b0 e0)
                         binding ...) tmps body)
     (let-values "mktmp" b0 e0 ()
       (binding ...) tmps body))
    ((let-values "mktmp" () e0 args
       bindings tmps body)
     (call-with-values
      (lambda () e0)
      (lambda args
        (let-values "bind"
          bindings tmps body))))
    ((let-values "mktmp" (a . b) e0 (arg ...)
       bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... x)
       bindings (tmp ... (a x)) body))
    ((let-values "mktmp" a e0 (arg ...)
       bindings (tmp ...) body)
     (call-with-values
      (lambda () e0)
      (lambda (arg ... . x)
        (let-values "bind"
          bindings (tmp ... (a x)) body))))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var val) ...) body ...)
     ((lambda ()
        (define var #f) ...
        (set! var val) ...
        body ...)))))

(define-syntax lej
  (syntax-rules ()
    ((lej ((name val) ...) body ...)
     ((lambda (name ...) body ...) val ...))
    ((lej tag ((name val) ...) body ...)
     ((letrec ((tag (lambda (name ...) body ...)))
        tag)
      val ...))))



(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))



(define (zero? x) (if (eqv? x 0) #t #f))

(define (not x)
  (if x #f #t))
