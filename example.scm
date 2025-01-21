(import base)

(cond
  ((< 1 10) => (display "hello"))
  ((< 1 42) => (lambda (x) (* x 2)))
  (else (display "world")))





