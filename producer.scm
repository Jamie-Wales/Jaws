(import list)

(define buffer '())
(define buffer-max-size 3)        
(define total-items-to-produce 8) 
(define items-produced 0)
(define items-consumed 0)

(define buffer-mutex (mutex-create))
(define not-empty-cond (condition-variable-create))
(define not-full-cond (condition-variable-create))
(define done-mutex (mutex-create))
(define all-done-cond (condition-variable-create))

(define (buffer-empty?)
  (null? buffer))

(define (buffer-full?)
  (= (length buffer) buffer-max-size))

(define (producer)
  (display "Producer started")
  (newline)
  
  (let loop ()
    (mutex-lock! buffer-mutex)
    (if (>= items-produced total-items-to-produce)
        (begin
          (display "Producer completed work")
          (newline)
          (mutex-unlock! buffer-mutex))
        (begin
          (if (buffer-full?)
              (begin
                (display "Buffer full, producer waiting...")
                (newline)
                (condition-variable-wait not-full-cond buffer-mutex)
                (display "Producer woke up - buffer has space")
                (newline)
                ;; Retry after waking up
                (mutex-unlock! buffer-mutex)
                (loop))
              (begin
                ;; Produce an item
                (let ((item items-produced))
                  (set! buffer (append buffer (list item)))
                  (set! items-produced (+ items-produced 1))
                  (display "Produced: item-")
                  (display item)
                  (display " (buffer size: ")
                  (display (length buffer))
                  (display ")")
                  (newline))
                
                ;; Use broadcast instead of signal to wake all consumers
                (condition-variable-broadcast! not-empty-cond)
                (mutex-unlock! buffer-mutex)
                
                ;; Small delay to simulate work
                (thread-sleep! 10)
                (loop))))))
  
  (display "Producer thread exiting")
  (newline))

(define (consumer id)
  (display "Consumer ")
  (display id)
  (display " started")
  (newline)
  
  (let loop ()
    (mutex-lock! buffer-mutex)
    (if (and (>= items-consumed total-items-to-produce) (buffer-empty?))
        (begin
          (display "Consumer ")
          (display id)
          (display " completed work")
          (newline)
          (mutex-unlock! buffer-mutex))
        (begin
          (if (buffer-empty?)
              (begin
                (display "Buffer empty, consumer ")
                (display id)
                (display " waiting...")
                (newline)
                (condition-variable-wait not-empty-cond buffer-mutex)
                (display "Consumer ")
                (display id)
                (display " woke up - items available")
                (newline)
                (mutex-unlock! buffer-mutex)
                (loop))
              (begin
                (let ((item (car buffer)))
                  (set! buffer (cdr buffer))
                  (set! items-consumed (+ items-consumed 1))
                  (display "Consumer ")
                  (display id)
                  (display " consumed: item-")
                  (display item)
                  (display " (buffer size: ")
                  (display (length buffer))
                  (display ")")
                  (newline))
                (condition-variable-broadcast! not-full-cond)
                (mutex-unlock! buffer-mutex)
                (when (= items-consumed total-items-to-produce)
                  (mutex-lock! done-mutex)
                  (condition-variable-broadcast! all-done-cond)
                  (mutex-unlock! done-mutex))
                (thread-sleep! 20)
                (loop))))))
  
  (display "Consumer ")
  (display id)
  (display " thread exiting")
  (newline))

(define (run-producer-consumer)
  (display "Starting producer-consumer example") (newline)
  (let ((producer-thread (thread-spawn producer)))
    (let ((consumer-thread (thread-spawn (lambda () (consumer 1))))) 
      (display "Main thread locking done-mutex...")(newline)
      (mutex-lock! done-mutex)
      (display "Main thread waiting for completion...") (newline)
      (condition-variable-wait all-done-cond done-mutex)
      (display "Main thread woke up - all done signaled.")(newline)
      (mutex-unlock! done-mutex)
      (display "Joining producer thread") (newline)
      (thread-join producer-thread)
      (display "Joining consumer thread") (newline)
      (thread-join consumer-thread) 

      (display "All threads joined. Example finished.") (newline))))

(run-producer-consumer)
