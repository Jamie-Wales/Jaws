(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

; Create a counter
(define counter (make-counter))

; Use the counter
(display (counter)) ; Output: 1
(display (counter)) ; Output: 2
(display (counter)) ; Output: 3

; Create another independent counter
(define counter2 (make-counter))
(display (counter2)) ; Output: 1
(display (counter))  ; Output: 4 (counter continues from where it left off)