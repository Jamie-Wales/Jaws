(import base)

(define (run-client host port)
  (display "Attempting to connect to ")
  (display host)
  (display ":")
  (display port)
  (newline)

  (let ((client-sock (socket-connect host port)))
    (if client-sock
        (begin
          (display "Connected to server.")
          (newline)

          ;; Write message to server
          (socket-write client-sock "Hello from client!")
          (display "Client sent message.")
          (newline)

          ;; Read response from server
          (let ((response-data (socket-read client-sock 1024)))
            (display "Client received: ")
            (display response-data)
            (newline))

          ;; Close client socket
          (socket-close client-sock)
          (display "Client connection closed.")
          (newline))
        (begin ;; Else block for failed connection
          (display "Failed to connect to server.")
          (newline)))))

;; --- Main execution for client ---
(define server-port 5555)
(define server-host "127.0.0.1") ;; Use "localhost" or the server's IP

(run-client server-host server-port)

(display "Client finished.")
(newline)
