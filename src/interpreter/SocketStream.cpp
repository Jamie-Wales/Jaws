#include "SocketStream.h"
#include <stdexcept>

SocketStream::SocketStream(int h)
    : handle(h)
    , isOpen(true)
{
}

SocketStream::~SocketStream()
{
    if (isOpen) {
        close(handle);
        isOpen = false;
    }
}

void SocketStream::closeSocket()
{
    if (isOpen) {
        close(handle);
        isOpen = false;
    }
}

bool SocketStream::is_open() const
{
    return isOpen && handle >= 0;
}

std::string SocketStream::read(size_t maxBytes)
{
    std::vector<char> buffer(maxBytes);
    int bytesRead = recv(handle, buffer.data(), buffer.size(), 0);

    if (bytesRead <= 0) {
        if (bytesRead < 0 && errno != EWOULDBLOCK && errno != EAGAIN) {
            throw std::runtime_error("Socket read error: " + std::string(strerror(errno)));
        }
        return "";
    }

    return std::string(buffer.data(), bytesRead);
}

int SocketStream::write(const std::string& data)
{
    int bytesWritten = send(handle, data.c_str(), data.size(), 0);
    if (bytesWritten < 0) {
        throw std::runtime_error("Socket write error: " + std::string(strerror(errno)));
    }
    return bytesWritten;
}

std::shared_ptr<SocketStream> SocketStream::accept()
{
    struct sockaddr_in clientAddr;
    socklen_t addrLen = sizeof(clientAddr);

    int clientSock = ::accept(handle, (struct sockaddr*)&clientAddr, &addrLen);

    if (clientSock < 0) {
        if (errno != EWOULDBLOCK && errno != EAGAIN) {
            throw std::runtime_error("Accept error: " + std::string(strerror(errno)));
        }
        return nullptr;
    }

    return std::make_shared<SocketStream>(clientSock);
}

bool SocketStream::setNonBlocking(bool nonBlocking)
{
    int flags = fcntl(handle, F_GETFL, 0);
    if (flags == -1) {
        throw std::runtime_error("Failed to get socket flags: " + std::string(strerror(errno)));
    }

    if (nonBlocking)
        flags |= O_NONBLOCK;
    else
        flags &= ~O_NONBLOCK;

    if (fcntl(handle, F_SETFL, flags) == -1) {
        throw std::runtime_error("Failed to set socket flags: " + std::string(strerror(errno)));
    }

    return true;
}
