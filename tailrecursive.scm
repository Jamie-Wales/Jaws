#lang scheme
(define (countdown n)
    (if (= n 1000000000) 
        0
            (begin (display n) (countdown (+ n 1)))))


(countdown 0)
