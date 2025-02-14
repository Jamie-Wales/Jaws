(define (square sqr) (* sqr sqr))


(define (sum-list lst)
  (if (= lst 0)
      0
      (+ (car lst)
         (sum-list (cdr lst)))))

(define (map-square lst)
  (if (= lst 0)
      0
      (cons (square (car lst))
            (map-square (cdr lst)))))

(define (process-list lst)
  (let ((squared (map-square lst))
        (total (sum-list lst)))
    (if (> total 10)
        squared
        lst)))



(process-list '(1 2 3 4 5))
