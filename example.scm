(import base)
(call-with-values (lambda () (values 4 5))
                  (lambda (a b) (+ a b)))

(cond ((< 1 10) (display "hello")))

(newline)


(begin (display "hello johnny") (newline) (display "poop") (newline))


(letrec ((even?
          (lambda (n)
            (if (= n 0)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (= n 0)
                #f
                (even? (- n 1))))))
  (begin
    (display "Is 4 even? ")
    (display (even? 4))
    (newline)
    (display "Is 7 even? ")
    (display (even? 7))
    (newline)))








