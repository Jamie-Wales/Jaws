;; Test program for macros

;; Test while
(display "=== while test ===")
(newline)
(let ((i 0))
  (while (< i 5)
    (display i)
    (display " ")
    (set! i (+ i 1))))
(newline)

;; Test for
(display "=== for test ===")
(newline)
(display (for x in '(1 2 3 4 5)
           (* x x)))
(newline)

;; Test repeat
(display "=== repeat test ===")
(newline)
(repeat 5
  (display "*"))
(newline)

;; Test until
(display "=== until test ===")
(newline)
(let ((i 5))
  (until (<= i 0)
    (display i)
    (display " ")
    (set! i (- i 1))))
(newline)

;; Test do-while
(display "=== do-while test ===")
(newline)
(let ((i 0))
  (do-while (< i 3)
    (display i)
    (display " ")
    (set! i (+ i 1))))
(newline)

;; Test for-each-with-index
(display "=== for-each-with-index test ===")
(newline)
(for-each-with-index (item idx) in '(a b c d)
  (display idx)
  (display ":")
  (display item)
  (display " "))
(newline)

;; Test for-range
(display "=== for-range test ===")
(newline)
(for-range i 1 5
  (display i)
  (display " "))
(newline)

;; Test iterate
(display "=== iterate test ===")
(newline)
(iterate x from 0 to 10 by 2
  (display x)
  (display " "))
(newline)

;; Test fold-loop
(display "=== fold-loop test ===")
(newline)
(display (fold-loop total init 0 element in '(1 2 3 4 5) 
           (+ total element)))
(newline)

;; Test select-case with else clause
(define (day-type day)
  (select-case day
    ((monday tuesday wednesday thursday friday)
     "Weekday")
    ((saturday sunday)
     "Weekend")
    (else
     "Invalid day")))
(display "Holiday is: ")
(display (day-type 'holiday))
(newline)

;; Testing select-case without else clause
(define (number-size n)
  (select-case n
    ((1 2 3 4 5) "small")
    ((6 7 8 9) "medium")
    ((10 11 12) "large")))
(display "Number 3 is: ")
(display (number-size 3))
(newline)
(display "Number 7 is: ")
(display (number-size 7))
(newline)
(display "Number 20 is: ")
(display (number-size 20))  
(newline)

;; Test nested-loop
(display "=== nested-loop test ===")
(newline)
(nested-loop (outer i 1 3)
             (inner j 1 3)
  (display "(")
  (display i)
  (display ",")
  (display j)
  (display ") "))
(newline)
