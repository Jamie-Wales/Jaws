(import base)

(define (values . things)
  (call-with-current-continuation
    (lambda (cont) (apply cont things))))

(define call-with-values
        (lambda (producer consumer)
          ((lambda (args)
             (apply consumer args))
            (producer))))

(call-with-values (lambda () (values 4 5))
  (lambda (a b) (+ a b)))

(cond ((< 1 10) (display "hello")))
(newline)

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
      (let-values "bind"
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

;(let-values (((a b) (values 10 20)))
;  (+ a b))



(define-syntax my-begin
  (syntax-rules ()
    ((my-begin exp ...)
      ((lambda () exp ...)))))



(my-begin (display "hello johnny") (newline) (display "poop"))








