(define (even? n)
  (if (= n 0) 0 (begin (display n) (odd? (- n 1)))))

(define (odd? n)
  (if (= n 0) 0 (begin (display n) (even? (- n 1)))))


(even? 1000)
