(import (math))
;;;; Scheme Test Program
;;; This program defines and tests several common Scheme features.
;;; It's designed to be R7RS compliant and runnable in Chez Scheme.

;;; 1. Basic Arithmetic and Helper for Displaying Test Results
(define (test-result test-name expected actual)
  (display test-name)
  (display ": Expected <")
  (write expected)
  (display ">, Got <")
  (write actual)
  (display ">. Pass: ")
  (display (equal? expected actual))
  (newline))

;;; 2. Factorial function (recursive)
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;;; 3. Fibonacci function (recursive, not tail-recursive for this example)
(define (fibonacci n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

;;; 4. List processing: sum of elements in a list
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

;;; 5. List processing: map a procedure onto a list
;;; (Implementing a simple version of map for testing,
;;;  as 'map' is already a standard procedure)
(define (my-map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
            (my-map proc (cdr lst)))))

;;; 6. Function using let for local bindings
(define (quadratic-roots a b c)
  (let* ((discriminant (- (* b b) (* 4 a c)))
         (sqrt-discriminant (if (>= discriminant 0) (sqrt discriminant) #f)))
    (if sqrt-discriminant
        (list (/ (+ (- b) sqrt-discriminant) (* 2 a))
              (/ (- (- b) sqrt-discriminant) (* 2 a)))
        "No real roots")))

  (define-syntax when
   (syntax-rules ()
    ((when test
      result1
      result2
      ...)
     (if test
      (begin
       result1
       result2
       ...)))))

;;; --- Test Cases ---
(display "--- Running Scheme Test Program ---")
(newline)
(newline)

(display "Section 1: Basic Tests (Implicit)")
(newline)
(test-result "Addition" 5 (+ 2 3))
(test-result "Subtraction" 2 (- 5 3))
(newline)

(display "Section 2: Factorial Tests")
(newline)
(test-result "Factorial of 0" 1 (factorial 0))
(test-result "Factorial of 1" 1 (factorial 1))
(test-result "Factorial of 5" 120 (factorial 5))
(test-result "Factorial of 7" 5040 (factorial 7))
(newline)

(display "Section 3: Fibonacci Tests")
(newline)
(test-result "Fibonacci of 0" 0 (fibonacci 0))
(test-result "Fibonacci of 1" 1 (fibonacci 1))
(test-result "Fibonacci of 6" 8 (fibonacci 6))
(test-result "Fibonacci of 10" 55 (fibonacci 10))
(newline)

(display "Section 4: Sum List Tests")
(newline)
(test-result "Sum of empty list" 0 (sum-list '()))
(test-result "Sum of (1 2 3 4 5)" 15 (sum-list '(1 2 3 4 5)))
(test-result "Sum of (10 20 -5)" 25 (sum-list '(10 20 -5)))
(newline)

(display "Section 5: My-Map Tests")
(newline)
(define (square x) (* x x))
(test-result "My-Map square on (1 2 3)" '(1 4 9) (my-map square '(1 2 3)))
(test-result "My-Map increment on (10 20 30)" '(11 21 31) (my-map (lambda (x) (+ x 1)) '(10 20 30)))
(test-result "My-Map on empty list" '() (my-map square '()))
(newline)

(display "Section 6: Quadratic Roots Tests")
(newline)
(test-result "Quadratic roots (x^2 - 3x + 2 = 0)" '(2.0 1.0) (quadratic-roots 1 -3 2))
(test-result "Quadratic roots (x^2 - 1 = 0)" '(1.0 -1.0) (quadratic-roots 1 0 -1))
(test-result "Quadratic roots (x^2 + 1 = 0)" "No real roots" (quadratic-roots 1 0 1))
(newline)

(display "Section 7: 'when' Macro Tests")
(newline)
(define x 10)
(when (> x 5)
  (display "'when' test 1: x is greater than 5. x = ")
  (write x)
  (newline)
  (set! x (* x 2)))
(test-result "'when' test 1: x after modification" 20 x)

(define y 3)
(when (> y 5) ; This body should not execute
  (display "'when' test 2: This should not print.")
  (newline)
  (set! y (* y 10)))
(test-result "'when' test 2: y should be unchanged" 3 y)
(newline)

(display "--- Test Program Finished ---")
(newline)
