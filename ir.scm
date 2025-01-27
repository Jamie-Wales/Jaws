(define x 42)              ; Expression 1
(let ((temp (+ x 1)))       ; Expression 2
  (display temp))          ; temp is used, so keep it

(let ((a (+ 1 2)))
  (let ((b (+ a 3)))    ; b is used
    (let ((c (* a 2)))  ; c is unused
      (+ b 4))))
