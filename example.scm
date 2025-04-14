(define-syntax define-values
(syntax-rules ()
((define-values () expr)
(define dummy
(call-with-values (lambda () expr)
(lambda args #f))))
((define-values (var) expr)
(define var expr))
((define-values (var0 var1 ... varn) expr)
(begin
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
(begin
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


(begin (define-values (sum diff) (values (+ 10 5) (- 10 3))) (display sum))


(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((temp a))  ; temp is an internal variable
       (set! a b)
       (set! b temp)))))
(define temp 100)      ; User defines a variable called 'temp'
(define x 10)
(define y 20)

(swap! x y)            ; Should swap x and y without affecting temp
(display temp)         ; Should still be 100
(display x)            ; Should be 20
(display y)            ; Should be 10
