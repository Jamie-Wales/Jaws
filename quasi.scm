(define (run-server port)
  (display "Starting server on port ")
  (display port)
  (newline)

  (let ((server-sock (socket-server port)))
    (if server-sock
        (begin
          (display "Server listening. Waiting for connection...")
          (newline)
          (let ((client-sock (socket-accept server-sock))) ; Blocks here until client connects
            (if client-sock
                (begin
                  (display "Client connected!")
                  (newline)

                  ; Read message from client
                  (let ((received-data (socket-read client-sock 1024)))
                    (display "Server received: ")
                    (display received-data)
                    (newline)

                    ; Write response to client
                    (socket-write client-sock "Hello from server!")
                    (display "Server sent response.")
                    (newline)

                    ; Close client connection
                    (socket-close client-sock)
                    (display "Client socket closed.")
                    (newline)))
                (display "Failed to accept client connection.")
                (newline)))

          ; Close server socket
          (socket-close server-sock)
          (display "Server socket closed.")
          (newline))
        (display "Failed to create server socket.")
        (newline))))

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

          ; Write message to server
          (socket-write client-sock "Hello from client!")
          (display "Client sent message.")
          (newline)

          ; Read response from server
          (let ((response-data (socket-read client-sock 1024)))
            (display "Client received: ")
            (display response-data)
            (newline))

          ; Close client socket
          (socket-close client-sock)
          (display "Client connection closed.")
          (newline))
        (display "Failed to connect to server.")
        (newline))))


(define server-port 5555)

(display "Defining server and client functions...")
(newline)
(display "Starting server (will wait for connection)...")
(newline)
(run-server server-port) ; If this blocks until accept, the client won't run yet.

(display "Starting client...") ; This line might not be reached if run-server blocks indefinitely
(newline)
(run-client "127.0.0.1" server-port)

(display "Test finished.")
(newline)
