(define test-list '(a b c d c e))
(define test-alist '((a . 1) (b . 2) (c . 3) (b . 4)))
(define test-symbols '(#\a "a" 'a 1 1.0))
(define bad-alist '((a . 1) b (c . 3)))

(display "=== memq tests ===")
(newline)
(display (memq 'c test-list)) ; Should return (c d c e)
(display "memq 'c test-list: ")
(newline)

(display "memq 'x test-list: ")
(newline)
(display (memq 'x test-list)) ; Should return #f
(newline)

(display "memq 'a '(): ")
(newline)
(display (memq 'a '())) ; Should return #f
(newline)

;; memv tests
(display "=== memv tests ===")
(newline)
(display "memv 'c test-list: ")
(newline)
(display (memv 'c test-list)) ; Should return (c d c e)
(newline)

(display "memv 1 '(0 1 2): ")
(newline)
(display (memv 1 '(0 1 2))) ; Should return (1 2)
(newline)

(display "memv 1.0 '(0 1 2): ")
(newline)
(display (memv 1.0 '(0 1 2))) ; Should return (1 2) - eqv? treats 1 and 1.0 as equivalent
(newline)

;; assoc tests
(display "=== assoc tests ===")
(newline)
(display "assoc 'b test-alist: ")
(display (assoc 'b test-alist)) ; Should return (b . 2)
(newline)

(display "assoc 'x test-alist: ")
(newline)
(display (assoc 'x test-alist)) ; Should return #f
(newline)

(display "assoc 'a '(): ")
(newline)
(display (assoc 'a '())) ; Should return #f
(newline)

;; assv tests
(display "=== assv tests ===")
(newline)
(display "assv 'c test-alist: ")
(display (assv 'c test-alist)) ; Should return (c . 3)
(newline)

(display "assv 'x test-alist: ")
(newline)
(display (assv 'x test-alist)) ; Should return #f
(newline)

;; Error test (uncomment to test)

;; Difference between equal? and eqv?
(display "=== Equal vs Eqv Tests ===")
(newline)
(define str-alist '(("apple" . 1) ("banana" . 2)))
(display "assoc \"apple\" str-alist: ")
(display (assoc "apple" str-alist)) ; Should find it with equal?
(newline)
(display "assv \"apple\" str-alist: ")
(display (assv "apple" str-alist)) ; Likely won't find it with eqv?
(newline)
;; Test member
(display "=== member tests ===")
(newline)
(display "member 'c test-list: ")
(display (member 'c test-list)) ; Should return (c d c e)
(newline)
(display "member 'x test-list: ")
(display (member 'x test-list)) ; Should return #f
(newline)
(display "member \"apple\" '(\"orange\" \"apple\" \"banana\"): ")
(display (member "apple" '("orange" "apple" "banana"))) ; Should return ("apple" "banana")
(newline)

;; Test assq (should behave like existing assq test)
(display "=== assq tests ===")
(newline)
(display "assq 'b test-alist: ")
(display (assq 'b test-alist)) ; Should return (b . 2)
(newline)
(display "assq 'x test-alist: ")
(display (assq 'x test-alist)) ; Should return #f
(newline)
(display "assq \"apple\" str-alist: ")
(display (assq "apple" str-alist)) ; Should NOT find it with eq?
(newline)

;; Test length
(display "=== length tests ===")
(newline)
(display "length test-list: ")
(display (length test-list)) ; Should return 6
(newline)
(display "length '(): ")
(display (length '())) ; Should return 0
(newline)
(display "length test-alist: ")
(display (length test-alist)) ; Should return 4
(newline)
