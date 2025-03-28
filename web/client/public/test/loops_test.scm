(import loops)
(display "=== while test ===")
(newline)
(let ((i 0))
  (while (< i 5)
    (display i)
    (display " ")
    (set! i (+ i 1))))
(newline)

(display "=== for test ===")
(newline)
(display (for x in '(1 2 3 4 5)
           (* x x)))
(newline)

(display "=== repeat test ===")
(newline)
(repeat 5
  (display "*"))
(newline)

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

(display "=== for-range test ===")
(newline)
(for-range i 1 5
  (display i)
  (display " "))
(newline)

(display "=== iterate test ===")
(newline)
(iterate x from 0 to 10 by 2
  (display x)
  (display " "))
(newline)

(display "=== fold-loop test ===")
(newline)
(display (fold-loop acc init 0 in '(1 2 3 4 5)
           (+ acc element)))
(newline)
(display "=== select-case test ===")
(newline)

(define (day-type day)
  (select-case day
    ((monday tuesday wednesday thursday friday)
     "Weekday")
    ((saturday sunday)
     "Weekend")
    (else
     "Invalid day")))

(display "Monday is a: ")
(display (day-type 'monday))
(newline)

(display "Saturday is a: ")
(display (day-type 'saturday))
(newline)

(display "Invalid day: ")
(display (day-type 'holiday))
(newline)

;; Testing without else clause
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
