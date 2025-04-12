(define (closure-test n)
  (let ((make-adder (lambda (x) (lambda (y) (+ x y)))))
    (let ((add5 (make-adder 5)))
      (let loop ((i 0) (sum 0))
        (if (< i n)
            (loop (+ i 1) (+ sum (add5 i)))
            sum)))))


(closure-test 1000000)
