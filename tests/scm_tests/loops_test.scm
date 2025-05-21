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

