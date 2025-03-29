(import list)
;; Revised Producer-Consumer example using broadcast instead of signal
;; This version uses broadcast to ensure all waiting threads are notified

;; Shared buffer with limited capacity
(define buffer '())
(define buffer-max-size 3)        ;; Smaller buffer
(define total-items-to-produce 8) ;; Fewer items
(define items-produced 0)
(define items-consumed 0)

;; Synchronization objects
(define buffer-mutex (mutex-create))
(define not-empty-cond (condition-variable-create))
(define not-full-cond (condition-variable-create))
(define done-mutex (mutex-create))
(define all-done-cond (condition-variable-create))

;; Helper function to check if buffer is empty
(define (buffer-empty?)
  (null? buffer))

;; Helper function to check if buffer is full
(define (buffer-full?)
  (= (length buffer) buffer-max-size))

;; Producer function - produces items and adds them to the buffer
(define (producer)
  (display "Producer started")
  (newline)
  
  (let loop ()
    (mutex-lock! buffer-mutex)
    
    ;; Check if we've produced enough items
    (if (>= items-produced total-items-to-produce)
        (begin
          (display "Producer completed work")
          (newline)
          (mutex-unlock! buffer-mutex))
        (begin
          ;; Wait until buffer is not full
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

;; Consumer function - consumes items from the buffer
(define (consumer id)
  (display "Consumer ")
  (display id)
  (display " started")
  (newline)
  
  (let loop ()
    (mutex-lock! buffer-mutex)
    
    ;; Check if we've consumed all items
    (if (and (>= items-consumed total-items-to-produce) (buffer-empty?))
        (begin
          (display "Consumer ")
          (display id)
          (display " completed work")
          (newline)
          (mutex-unlock! buffer-mutex))
        (begin
          ;; Wait until buffer is not empty
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
                ;; Retry after waking up
                (mutex-unlock! buffer-mutex)
                (loop))
              (begin
                ;; Take item from buffer
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
                
                ;; Use broadcast instead of signal
                (condition-variable-broadcast! not-full-cond)
                (mutex-unlock! buffer-mutex)
                
                ;; If this was the last item, signal completion
                (when (= items-consumed total-items-to-produce)
                  (mutex-lock! done-mutex)
                  (condition-variable-broadcast! all-done-cond)
                  (mutex-unlock! done-mutex))
                
                ;; Small delay to simulate processing
                (thread-sleep! 20)
                (loop))))))
  
  (display "Consumer ")
  (display id)
  (display " thread exiting")
  (newline))

;; Main function to start the producer-consumer system
(define (run-producer-consumer)
  (display "Starting producer-consumer example")
  (newline)
  
  ;; Create producer and consumer threads
  (define producer-thread (thread-spawn producer))
  (define consumer1-thread (thread-spawn (lambda () (consumer 1))))
  
  ;; Wait for all items to be consumed
  (mutex-lock! done-mutex)
  (display "Main thread waiting for completion...")
  (newline)
  (condition-variable-wait all-done-cond done-mutex)
  (mutex-unlock! done-mutex)
  
  ;; Join threads
  (display "Joining producer thread")
  (newline)
  (thread-join producer-thread)
  (display "Joining consumer 1 thread")
  (newline)
  (thread-join consumer1-thread)
  
  ;; Summary
  (display "Producer-consumer example completed")
  (newline)
  (display "Items produced: ")
  (display items-produced)
  (newline)
  (display "Items consumed: ")
  (display items-consumed)
  (newline)
  (display "Final buffer size: ")
  (display (length buffer))
  (newline))

;; Run the example
(run-producer-consumer)
