(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(display (factorial 5))
(newline)

; Dead code - never used
(define (unused-square x)
  (* x x))

(define (unused-cube x)
  (* x (unused-square x)))

; Live code - mutually recursive and called
(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))

(define (odd? n)
  (if (= n 0)
      #f
      (even? (- n 1))))

(display "Is 4 even? ")
(display (even? 4))
(newline)

; Dead code - mutually recursive but never called
(define (dead-ping n)
  (if (= n 0)
      'ping
      (dead-pong (- n 1))))

(define (dead-pong n)
  (if (= n 0)
      'pong
      (dead-ping (- n 1))))

; Live code - has side effects
(define (greet name)
  (display "Hello, ")
  (display name)
  (newline))

(greet "World")

; Dead code - pure functions never called
(define (dead-sum-squares x y)
  (+ (unused-square x)
     (unused-square y)))

(define (dead-average x y)
  (/ (+ x y) 2))

; Live code - pure function but used
(define (double x)
  (* x 2))

(display (double 5))
(newline)
