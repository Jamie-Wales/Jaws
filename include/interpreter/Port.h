#pragma once
#include "Visit.h"
#include <fstream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <variant>
#include <vector>

class SocketStream;

enum class PortType {
    Input,
    Output,
    ServerSocket,
    ClientSocket
};

class Port {
public:
    std::variant<
        std::shared_ptr<std::fstream>,
        std::shared_ptr<std::stringstream>,
        std::shared_ptr<SocketStream>>
        stream;
    PortType type;

    Port(std::shared_ptr<std::fstream> f, PortType t);
    explicit Port(const std::string& s);
    explicit Port(std::stringstream&& ss);
    Port(std::shared_ptr<SocketStream> sock, PortType t);

    static Port createServerSocket(int port, int backlog = 5);
    static Port connectToServer(const std::string& host, int port);

    Port acceptConnection() const;
    std::string socketRead(size_t maxBytes = 1024) const;
    int socketWrite(const std::string& data) const;
    bool setNonBlocking(bool nonBlocking) const;

    bool isOpen() const;
    void close() const;
    std::istream* get() const;
    std::ostream* getOutput() const;
    PortType getType() const;
};
