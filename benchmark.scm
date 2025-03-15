;; Environment lookup speed
(define (env-test n)
  (let ((a 1) (b 2) (c 3))
    (let ((d 4) (e 5))
      (let loop ((i 0) (sum 0))
        (if (< i n)
            (loop (+ i 1) (+ sum a b c d e))
            sum)))))

;; Closure creation and access
(define (closure-test n)
  (let ((make-adder (lambda (x) (lambda (y) (+ x y)))))
    (let ((add5 (make-adder 5)))
      (let loop ((i 0) (sum 0))
        (if (< i n)
            (loop (+ i 1) (+ sum (add5 i)))
            sum)))))



(closure-test 1000)
