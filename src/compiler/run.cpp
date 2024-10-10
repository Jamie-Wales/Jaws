#include "run.h"
#include "Parser.h"
#include "Scanner.h"
#include "Token.h"
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>

std::string readFile(const std::string& path)
{
    std::ifstream inputFileStream(path);
    if (!inputFileStream) {
        throw std::runtime_error("Unable to open file: " + path);
    }
    std::stringstream buffer;
    buffer << inputFileStream.rdbuf();
    return buffer.str();
}

void runFile(const std::string& path)
{
    try {
        Scanner scanner;
        std::string sourceCode = readFile(path);
        std::vector<Token> tokens = scanner.tokenize(sourceCode);

        Parser parser(tokens);
        auto expr = parser.parse();

        if (!expr) {
            throw std::runtime_error("Parsing failed");
        }

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
}
