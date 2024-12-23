
(define (test-cond x)
  (cond
    ((< x 0) 
     (begin
       (display "x is negative")
       'negative))
    ((> x 0)
     (begin
       (display "x is positive")
       (newline)
       'positive))
    (else
     (begin
       (display "x is zero")
       'zero))))

(define (test-begin x)
  (begin
    (display "Starting computation...")
    (display "Input x = ")
    (display x)
    (let ((result (* x x)))
      (display "Square of x = ")
      (display result)
      result)))

;; Test cases
(display "Testing cond:\n")
(test-cond -5)
(test-cond 3)
(test-cond 0)

(display "\nTesting begin:\n")
(test-begin 4)
