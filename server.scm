(import (base))

(define (run-server port)
  (display "Starting server on port ")
  (display port)
  (newline)

  (let ((server-sock (socket-server port)))
    (if server-sock
        (begin
          (display "Server listening. Waiting for connection...")
          (newline)
          ;; --- Accept Loop (Simplified: accepts one client) ---
          (let ((client-sock (socket-accept server-sock))) ; Blocks here
            (if client-sock
                (begin
                  (display "Client connected!")
                  (newline)

                  ;; Read message from client
                  (let ((received-data (socket-read client-sock 1024)))
                    (display "Server received: ")
                    (display received-data)
                    (newline)

                    ;; Write response to client
                    (socket-write client-sock "Hello from server!")
                    (display "Server sent response.")
                    (newline)

                    ;; Close client connection
                    (socket-close client-sock)
                    (display "Client socket closed.")
                    (newline)))
                (begin ;; Else block for failed accept
                  (display "Failed to accept client connection.")
                  (newline))))
            ;; --- End Accept Loop ---

          ;; Close server socket (after handling client)
          (socket-close server-sock)
          (display "Server socket closed.")
          (newline))
        (begin ;; Else block for failed server socket creation
          (display "Failed to create server socket.")
          (newline)))))

;; --- Main execution for server ---
(define server-port 5555)
(run-server server-port)

(display "Server finished.")
(newline)
