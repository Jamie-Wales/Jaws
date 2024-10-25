#pragma once
#include <fstream>
#include <memory>

enum class PortType {
    Input,
    Output
};

class Port {
public:
    mutable std::shared_ptr<std::fstream> file;  
    PortType type;
    Port(std::shared_ptr<std::fstream> f, PortType t) 
        : file(std::move(f)), type(t) {}

    bool isOpen() const { 
        return file && file->is_open(); 
    }

    void close() const {  
        if (isOpen()) {
            file->close();
        }
    }

    std::fstream* get() const { return file.get(); }
    PortType getType() const { return type; }
};
