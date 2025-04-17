(define-library (base)

  (export do cond let* let-values letrec lej define-values
          and or when unless zero? not print println)

  (begin
    (define-syntax do
      (syntax-rules ()
        ((do ((var init step ...) ...) (test expr ...) command ...)
         (letrec ((loop (lambda (var ...)
                          (if test
                              (begin (if #f #f) expr ...)
                              (begin command ... (loop (do "step" var step ...) ...))))))
           (loop init ...)))
        ((do "step" x) x)
        ((do "step" x y) y)))


(define-syntax define-values
(syntax-rules ()
((define-values () expr)
(define dummy
(call-with-values (lambda () expr)
(lambda args #f))))
((define-values (var) expr)
(define var expr))
((define-values (var0 var1 ... varn) expr)
(my-begin
(define var0
(call-with-values (lambda () expr)
list))
(define var1
(let ((v (cadr var0)))
(set-cdr! var0 (cddr var0))
v)) ...
(define varn
(let ((v (cadr var0)))
(set! var0 (car var0))
v))))
((define-values (var0 var1 ... . varn) expr)
(my-begin
(define var0
(call-with-values (lambda () expr)
list))
(define var1
(let ((v (cadr var0)))
(set-cdr! var0 (cddr var0))
v)) ...
(define varn
(let ((v (cdr var0)))
(set! var0 (car var0))
v))))
((define-values var expr)
(define var
(call-with-values (lambda () expr)
list)))))

    (define-syntax cond
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...)) (begin result1 result2 ...))
        ((cond (test => result)) (let ((temp test)) (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...) (let ((temp test)) (if temp (result temp) (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...) (let ((temp test)) (if temp temp (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...)) (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...) clause1 clause2 ...) (if test (begin result1 result2 ...) (cond clause1 clause2 ...)))))

    (define-syntax let*
      (syntax-rules ()
        ((let* () body ...) (begin body ...))
        ((let* ((name value)) body ...) (let ((name value)) body ...))
        ((let* ((name1 value1) (name2 value2) rest ...) body ...)
         (let ((name1 value1)) (let* ((name2 value2) rest ...) body ...)))))

    (define-syntax let-values
       (syntax-rules ()
         ((let-values (binding ...) body0 body1 ...) (let-values "bin" (binding ...) () (begin body0 body1 ...)))
         ((let-values "bin" () tmps body) (let tmps body))
         ((let-values "bin" ((b0 e0) binding ...) tmps body) (let-values "mktmp" b0 e0 () bindings tmps body))
         ((let-values "mktmp" () e0 args bindings tmps body) (call-with-values (lambda () e0) (lambda args (let-values "bin" bindings tmps body))))
         ((let-values "mktmp" (a . b) e0 (arg ...) bindings (tmp ...) body) (let-values "mktmp" b e0 (arg ... x) bindings (tmp ... (a x)) body))
         ((let-values "mktmp" a e0 (arg ...) bindings (tmp ...) body) (call-with-values (lambda () e0) (lambda (arg ... . x) (let-values "bin" bindings (tmp ... (a x)) body))))))

    (define-syntax letrec
       (syntax-rules ()
         ((letrec ((var val) ...) body ...)
          (let ((var #f) ...) (set! var val) ... body ...))))

    (define-syntax lej ;; Assumed 'let' for direct lambda call
      (syntax-rules ()
        ((lej ((name val) ...) body ...) ((lambda (name ...) body ...) val ...))
        ((lej tag ((name val) ...) body ...) ((letrec ((tag (lambda (name ...) body ...))) tag) val ...))))

    (define-syntax and
      (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...) (if test1 (and test2 ...) #f))))

    (define-syntax or
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...) (let ((x test1)) (if x x (or test2 ...))))))

    (define-syntax when
      (syntax-rules ()
        ((when test result1 result2 ...) (if test (begin result1 result2 ...)))))

    (define-syntax unless
      (syntax-rules ()
        ((unless test result1 result2 ...) (if (not test) (begin result1 result2 ...)))))

    (define (zero? x) (eqv? x 0))

    (define (not x) (if x #f #t))

    ;; --- NEW Macro Definitions ---
    (define-syntax print
      (syntax-rules ()
        ((print expr)
         (display expr))))

    (define-syntax println
      (syntax-rules ()
        ((println expr)
         (begin (display expr) (newline)))))))

   ;; end begin
 ;; end define-library
