#pragma once
#include <memory>
#include <string>
#include <vector>

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

class SocketStream {
public:
    int handle;
    bool isOpen;
    explicit SocketStream(int h);
    ~SocketStream();
    void closeSocket();
    bool is_open() const;
    std::string read(size_t maxBytes = 1024);
    int write(const std::string& data);
    std::shared_ptr<SocketStream> accept();
    bool setNonBlocking(bool nonBlocking);
};
