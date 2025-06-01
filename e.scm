(define (make-adder x) (lambda (y) (+ y x)))


(define add5 (make-adder 5))

(display (add5 3))
