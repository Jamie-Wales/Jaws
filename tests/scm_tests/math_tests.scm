(import (math))
(display (+ 10 5 3))     ; Expected: 18
(newline)
(display (- 20 8))       ; Expected: 12
(newline)
(display (- 5))          ; Expected: -5
(newline)
(display (* 2 3 10))     ; Expected: 60
(newline)
(display (/ 100 10 2))   ; Expected: 5
(newline)
(display (+ 1.5 2.5))    ; Expected: 4.0
(newline)

;; --- Comparisons ---
(display (< 5 10 15))    ; Expected: #t
(newline)
(display (>= 10 10 9))   ; Expected: #t
(newline)
(display (= 5 5.0))      ; Expected: #t (numeric value equality)
(newline)

;; --- Constants ---
(display pi)             ; Expected: 3.1415...
(newline)

;; --- Scheme-defined Functions ---
(display (square 7))     ; Expected: 49
(newline)
(display (abs -12.5))    ; Expected: 12.5
(newline)
(display (magnitude (make-rectangular 3 -4))) ; Expected: 5.0 (magnitude of 3-4i)
(newline)
(display (gcd 54 24))    ; Expected: 6
(newline)
(display (lcm 6 10))     ; Expected: 30
(newline)
(display (max 10 3 15 8)) ; Expected: 15
(newline)
(display (min 10 -3 15 8)) ; Expected: -3
(newline)
(display (modulo 13 5))  ; Expected: 3
(newline)
(display (modulo -13 5)) ; Expected: 2 (R7RS modulo sign matches divisor)
(newline)
(display (remainder -13 5)) ; Expected: -3 (R7RS remainder sign matches dividend)
(newline)

(display (zero? (- 10 10))) ; Expected: #t
(newline)
(display (positive? pi))   ; Expected: #t
(newline)
(display (negative? -0.0)) ; Expected: #f (zero isn't negative)
(newline)
(display (odd? 7))         ; Expected: #t (Requires integer? primitive)
(newline)
(display (even? 7))        ; Expected: #f (Requires integer? primitive)
(newline)
(display (integer? 5))     ; Expected: #t (Requires integer? primitive)
(newline)
(display (integer? 5.0))   ; Expected: #t/#f (Depends on integer? impl for inexact) - R7RS: #t
(newline)
(display (integer? 5.1))   ; Expected: #f
(newline)
(display (real? (make-rectangular 5 0))) ; Expected: #t (Requires real? primitive)
(newline)
(display (real? (make-rectangular 5 1))) ; Expected: #f (Requires real? primitive)
(newline)

;; --- Transcendental / Complex ---
(display (sqrt -9))      ; Expected: 0.0+3.0i (complex result)
(newline)
(display (imag-part (sqrt -9))) ; Expected: 3.0
(newline)
(display (exp 0))        ; Expected: 1.0
(newline)
(display (log (exp 1)))  ; Expected: 1.0 (approx)
(newline)
(display (sin pi))       ; Expected: 0.0 (approx)
(newline)
(display (atan 1))       ; Expected: 0.78539... (pi/4)
(newline)

;; --- Exactness ---
(display (exact? 5))           ; Expected: #t (Requires exact? primitive)
(newline)
(display (exact? 5.0))         ; Expected: #f (Requires exact? primitive)
(newline)
(display (inexact? (+ 1.0 2))) ; Expected: #t (Requires inexact? primitive)
(newline)
(display (exact->inexact (gcd 12 18))) ; Expected: 6.0
(newline)

;; --- Random ---
(display (number? (random)))   ; Expected: #t (result is 0.0 <= x < 1.0)
(newline)
(let ((r (random 100)))
  (display (and (integer? r) (>= r 0) (< r 100)))) ; Expected: #t
(newline)
