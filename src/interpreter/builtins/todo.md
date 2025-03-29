
(socket-server port-number [backlog]):

# To Test

Purpose: To create a listening TCP socket that waits for incoming connections.
Arguments: Takes a port number to listen on and an optional backlog argument (how many pending connections the OS should queue).
Returns: A new 'server socket' port object.
Implementation: Would likely wrap Port::createServerSocket.

(socket-connect host port-number):
Purpose: To establish a TCP connection to a remote server.
Arguments: Takes the hostname (or IP address) and the port number of the server to connect to.
Returns: A new 'client socket' port object representing the established connection.
Implementation: Would likely wrap Port::connectToServer.

(socket-accept server-socket-port):
Purpose: To accept an incoming connection on a listening server socket.
Arguments: Takes a 'server socket' port (created by socket-server).
Returns: A new 'client socket' port object representing the connection to the specific client that just connected. This is the port you'd use for socket-read and socket-write with that client.
Implementation: Would likely wrap Port::acceptConnection.

(socket-read socket-port [max-bytes]):
Purpose: To read data sent from the other end of a connection.
Arguments: Takes a 'client socket' port (from socket-connect or socket-accept) and an optional maximum number of bytes to read.
Returns: The data read, likely as a string or bytevector. Might return an end-of-file object if the connection is closed.
Implementation: Would likely wrap Port::socketRead.

(socket-write socket-port data):
Purpose: To send data to the other end of a connection.
Arguments: Takes a 'client socket' port and the data (string or bytevector) to send.
Returns: Possibly the number of bytes written, or an unspecified value.
Implementation: Would likely wrap Port::socketWrite.

(socket-close socket-port):
Purpose: To close a socket connection (either client or server).
Arguments: Takes any socket port object.
Returns: An unspecified value.
Implementation: Would likely wrap Port::close.

(socket-set-nonblocking! socket-port boolean):
Purpose: To change the blocking behavior of socket operations (like socket-read or socket-accept). If non-blocking is true, these operations return immediately, even if no data/connection is available.
Arguments: Takes a socket port and a boolean (#t for non-blocking, #f for blocking).
Returns: An unspecified value.
Implementation: Would likely wrap Port::setNonBlocking.
