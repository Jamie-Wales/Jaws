#include "Port.h"
#include "SocketStream.h"
#include <stdexcept>

Port::Port(std::shared_ptr<std::fstream> f, PortType t)
    : stream(std::move(f))
    , type(t)
{
}

Port::Port(const std::string& s)
    : stream(std::make_shared<std::stringstream>(s))
    , type(PortType::Input)
{
}

Port::Port(std::stringstream&& ss)
    : stream(std::make_shared<std::stringstream>(std::move(ss)))
    , type(PortType::Input)
{
}

Port::Port(std::shared_ptr<SocketStream> sock, PortType t)
    : stream(std::move(sock))
    , type(t)
{
}

Port Port::createServerSocket(int port, int backlog)
{
    int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock < 0) {
        throw std::runtime_error("Failed to create socket: " + std::string(strerror(errno)));
    }

    int yes = 1;
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes)) < 0) {
        ::close(sock);
        throw std::runtime_error("Failed to set socket options: " + std::string(strerror(errno)));
    }

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = INADDR_ANY;

    if (bind(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        ::close(sock);
        throw std::runtime_error("Failed to bind to port " + std::to_string(port) + ": " + std::string(strerror(errno)));
    }

    if (listen(sock, backlog) < 0) {
        ::close(sock);
        throw std::runtime_error("Failed to listen on socket: " + std::string(strerror(errno)));
    }

    return Port(std::make_shared<SocketStream>(sock), PortType::ServerSocket);
}

Port Port::connectToServer(const std::string& host, int port)
{
    int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (sock < 0) {
        throw std::runtime_error("Failed to create socket: " + std::string(strerror(errno)));
    }

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);

    if (inet_pton(AF_INET, host.c_str(), &addr.sin_addr) <= 0) {
        ::close(sock);
        throw std::runtime_error("Invalid address or address not supported: " + host);
    }

    if (connect(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        ::close(sock);
        throw std::runtime_error("Connection failed to " + host + ":" + std::to_string(port) + ": " + std::string(strerror(errno)));
    }

    return Port(std::make_shared<SocketStream>(sock), PortType::ClientSocket);
}

Port Port::acceptConnection() const
{
    if (type != PortType::ServerSocket) {
        throw std::runtime_error("Cannot accept on non-server socket");
    }

    auto serverSock = std::get<std::shared_ptr<SocketStream>>(stream);
    auto clientSock = serverSock->accept();

    if (!clientSock) {
        throw std::runtime_error("Failed to accept connection");
    }

    return Port(clientSock, PortType::ClientSocket);
}

bool Port::isOpen() const
{
    return std::visit(overloaded {
                          [](const std::shared_ptr<std::fstream>& f) { return f && f->is_open(); },
                          [](const std::shared_ptr<std::stringstream>&) { return true; },
                          [](const std::shared_ptr<SocketStream>& s) { return s && s->is_open(); } },
        stream);
}

void Port::close() const
{
    std::visit(overloaded {
                   [](const std::shared_ptr<std::fstream>& f) {
                       if (f && f->is_open())
                           f->close();
                   },
                   [](const std::shared_ptr<std::stringstream>&) { /* Nothing to close */ },
                   [](const std::shared_ptr<SocketStream>& s) {
                       if (s)
                           s->closeSocket();
                   } },
        stream);
}

std::istream* Port::get() const
{
    return std::visit(overloaded {
                          [](const std::shared_ptr<std::fstream>& f) -> std::istream* { return f.get(); },
                          [](const std::shared_ptr<std::stringstream>& s) -> std::istream* { return s.get(); },
                          [](const std::shared_ptr<SocketStream>&) -> std::istream* {
                              throw std::runtime_error("Cannot get std::istream from socket");
                          } },
        stream);
}

std::ostream* Port::getOutput() const
{
    return std::visit(overloaded {
                          [](const std::shared_ptr<std::fstream>& f) -> std::ostream* { return f.get(); },
                          [](const std::shared_ptr<std::stringstream>& s) -> std::ostream* { return s.get(); },
                          [](const std::shared_ptr<SocketStream>&) -> std::ostream* {
                              throw std::runtime_error("Cannot get std::ostream from socket");
                          } },
        stream);
}

std::string Port::socketRead(size_t maxBytes) const
{
    if (type != PortType::ClientSocket) {
        throw std::runtime_error("Can only read from client socket");
    }

    auto sock = std::get<std::shared_ptr<SocketStream>>(stream);
    return sock->read(maxBytes);
}

int Port::socketWrite(const std::string& data) const
{
    if (type != PortType::ClientSocket) {
        throw std::runtime_error("Can only write to client socket");
    }

    auto sock = std::get<std::shared_ptr<SocketStream>>(stream);
    return sock->write(data);
}

bool Port::setNonBlocking(bool nonBlocking) const
{
    if (type != PortType::ServerSocket && type != PortType::ClientSocket) {
        throw std::runtime_error("Can only set non-blocking mode on sockets");
    }

    auto sock = std::get<std::shared_ptr<SocketStream>>(stream);
    return sock->setNonBlocking(nonBlocking);
}

PortType Port::getType() const
{
    return type;
}
