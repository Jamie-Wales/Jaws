(define (find-first pred lst)
  (call/cc (lambda (return)
             ;; Uses the for-each below
             (for-each (lambda (x)
                         (if (pred x) ; If this is true...
                             (return x))) ; ...execution jumps out via return
                       lst)
             ;; This #f is only reached if the loop finishes without 'return' being called
             #f)))

;; Assuming corrected for-each using your 'when'
(define (for-each proc lst)
  (if (not (null? lst))
      ; Then-expression (list is not empty)
      (begin
        (proc (car lst))         ; Apply proc to the first element
        (for-each proc (cdr lst))) ; Recurse on the rest of the list
      ; Else-expression (list is empty)
      #f                        ; Return #f (or similar) for the base case
  ))
(define (even? x) (letrec ((is-even? (lambda (n)
                                      (if (zero? n)
                                          #t
                                          (is-odd? (- n 1)))))
                           (is-odd? (lambda (n)
                                     (if (zero? n)
                                         #f
                                         (is-even? (- n 1))))))
                    (is-even? x)))

(find-first even? '(1 3 5 6 7 8 9))


`(1 ,@(list 1 2) 4)
