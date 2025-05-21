(define (closure-test n)
  (let ((make-adder (lambda (x) (lambda (y) (+ x y)))))
    (let ((add-1 (make-adder 1)))
      (let loop ((i 0) (sum 0))
        (if (< i n)
            (loop (+ i 1) (+ sum (add-1 i)))
            sum)))))


(closure-test 10000000)
