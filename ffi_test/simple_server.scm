(import base)
(load-library "http" "../ffi_test/libsimple_http.so")
(register-function "http" "http_init_server" "http-init-server" "int" "int")
(register-function "http" "http_accept_client" "http-accept-client" "int" "int")
(register-function "http" "http_read_request" "http-read-request" "string" "int")
(register-function "http" "http_get_path" "http-get-path" "string" "string")
(register-function "http" "http_send_response" "http-send-response" "void" "int" "string")
(register-function "http" "http_close_client" "http-close-client" "void" "int")
(register-function "http" "http_shutdown" "http-shutdown" "void" "int")

(define (home-page)
  "<html>
   <head><title>Welcome</title></head>
   <body>
     <h1>Welcome to the Scheme FFI Web Server!</h1>
     <p>This is a simple web server written in Scheme using FFI to C.</p>
     <ul>
       <li><a href='/'>Home</a></li>
       <li><a href='/about'>About</a></li>
       <li><a href='/contact'>Contact</a></li>
     </ul>
   </body>
   </html>")

(define (about-page)
  "<html>
   <head><title>About</title></head>
   <body>
     <h1>About this Server</h1>
     <p>This server demonstrates how to use Foreign Function Interface (FFI) 
        to call C functions from Scheme code.</p>
     <p>It uses a simple C library to handle the low-level socket operations 
        while the server logic is written in Scheme.</p>
     <p><a href='/'>Back to Home</a></p>
   </body>
   </html>")

(define (contact-page)
  "<html>
   <head><title>Contact</title></head>
   <body>
     <h1>Contact Us</h1>
     <p>This is a demo contact page.</p>
     <p>In a real application, you might have a form here.</p>
     <p><a href='/'>Back to Home</a></p>
   </body>
   </html>")

(define (not-found-page)
  "<html>
   <head><title>404 Not Found</title></head>
   <body>
     <h1>404 - Page Not Found</h1>
     <p>The page you requested could not be found.</p>
     <p><a href='/'>Back to Home</a></p>
   </body>
   </html>")

(define (route-request path)
  (cond
    ((string=? path "/") (home-page))
    ((string=? path "/about") (about-page))
    ((string=? path "/contact") (contact-page))
    (else (not-found-page))))

(define (handle-client client)
  (let* ((request (http-read-request client))
         (path (http-get-path request)))
    (display "Received request for: ")
    (display path)
    (newline)
    (let ((response (route-request path)))
      (http-send-response client response))))

(define (server-loop server)
  (newline)
  (display "Waiting for connections...")
  (newline)
  (let ((client (http-accept-client server)))
    (if (< client 0)
        (begin
          (display "Failed to accept client")
          (newline))
        (begin
          (display "Client connected")
          (newline)
          (handle-client client)
          (http-close-client client)
          (display "Client disconnected")
          (newline))))
  (server-loop server))

(define (start-server port)
  (display "Starting server on port ")
  (display port)
  (newline)
  (let ((server (http-init-server port)))
    (if (< server 0)
        (begin
          (display "Failed to start server")
          (newline))
        (begin
          (display "Server started successfully")
          (newline)
          (server-loop server)))))

(start-server 8080)
