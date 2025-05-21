(define-syntax swap
  (syntax-rules ()
    ((_ var1 var2)
     (let ((temp var1)) 
       (set! var1 var2)  
       (set! var2 temp))))) 


(define a 10)
(define b 20)

(define temp 99)

(display "Before swap:\n")
(display "  a = ") (display a) (newline)
(display "  b = ") (display b) (newline)
(display "  temp = ") (display temp) (newline)

(swap a b)
