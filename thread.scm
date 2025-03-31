(import base)

(define (for-each proc lst)
  (if (null? lst)
      #f
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))))


(display "Starting concurrent counter test")
(newline)

(define counter 0)
(define counter-mutex (mutex-create))

(define (increment-counter! n)
 (display "TRACE: About to increment counter ")
 (display n)
 (display " times")
 (newline)

 (do ((i 0 (+ i 1)))
     ((= i n))
   (mutex-lock! counter-mutex)
   (set! counter (+ counter 1))
   (display "Counter incremented to: ")
   (display counter)
   (newline)
   (mutex-unlock! counter-mutex))
  
 (display "Thread finished incrementing")
 (newline)
 counter)

(display "Creating 5 threads to increment counter")
(newline)
(define threads
  (let loop ((n 5) (threads '()))
    (if (= n 0)
        threads
        (loop (- n 1) 
              (cons (thread-spawn (lambda () (increment-counter! 10))) 
                    threads)))))

(display "Joining threads")
(newline)
(for-each (lambda (thread)
            (display "Joining a thread")
            (newline)
            (thread-join thread)
            (display "Thread joined")
            (newline))
          threads)

(display "All threads joined")
(newline)
(display "Final counter value: ")
(display counter)
(newline)

(display "Test complete")
(newline)




