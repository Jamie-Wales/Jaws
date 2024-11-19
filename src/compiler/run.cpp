#include "run.h"
#include "Error.h"
#include "Interpreter.h"
#include "Parser.h"
#include "Scanner.h"
#include "Token.h"
#include "Value.h"
#include "icons.h"
#include "run.h"
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>

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
        std::string sourceCode = readFile(path);
        auto scanner = std::make_shared<Scanner>();
        auto parser = std::make_shared<Parser>();
        parser->initialize(scanner);
        std::vector<Token> tokens = scanner->tokenize(sourceCode);
        parser->load(tokens);
        Interpreter i = { scanner, parser };
        i.init();
        std::cout << i.outputStream.str();
    } catch (const InterpreterError& e) {
        e.printFormattedError();
    }
}

void runPrompt()
{
    printJawsLogo();
    std::cout << "Welcome to the Jaws REPL!\n";
    std::cout << "Type 'exit' to quit, '(help)' for commands.\n\n";

    auto scanner = std::make_shared<Scanner>();
    auto parser = std::make_shared<Parser>();
    parser->initialize(scanner);
    Interpreter i = { scanner, parser };

    std::string input;
    while (true) {
        std::cout << "jaws: |> ";
        if (!std::getline(std::cin, input)) {
            break;
        }

        if (input == "exit") {
            std::cout << "Fin-ishing up. Goodbye!\n";
            break;
        }

        if (input == "jaws") {
            std::cout << jaws2 << std::endl;
            continue;
        }

        if (input.empty())
            continue;

        try {

            std::vector<Token> tokens = scanner->tokenize(input);
            parser->load(tokens);
            i.init();
            std::cout << i.outputStream.str();
            i.outputStream.clear();
        } catch (const ParseError& e) {
            e.printFormattedError();
        } catch (const InterpreterError& e) {
            e.printFormattedError();
        }
    }
}
