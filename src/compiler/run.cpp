#include "run.h"
#include "Error.h"
#include "Interpreter.h"
#include "Parser.h"
#include "Scanner.h"
#include "Token.h"
#include "Value.h"
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
        std::vector<Token> tokens = scanner->tokenize(sourceCode);
        Parser parser;
        parser.initialize(scanner);
        parser.load(tokens);

        auto expr = parser.parse();
        if (!expr) {
            throw std::runtime_error("Parsing failed");
        }

        Interpreter interpreter;
        if (expr) {
            std::optional<SchemeValue> result = interpreter.interpret(*expr);
            if (result) {
                std::cout << result->toString() << std::endl;
            }
        }
    } catch (const InterpreterError& e) {
        e.printFormattedError();
    }
}

void printJawsLogo()
{
    std::cout << R"(
     ██╗ █████╗ ██╗    ██╗███████╗
     ██║██╔══██╗██║    ██║██╔════╝
     ██║███████║██║ █╗ ██║███████╗
██   ██║██╔══██║██║███╗██║╚════██║
╚█████╔╝██║  ██║╚███╔███╔╝███████║
 ╚════╝ ╚═╝  ╚═╝ ╚══╝╚══╝ ╚══════╝
                                  
        ⠀ ⢯⠙⠲⢤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠸⡆⠀⠀⠈⠳⣄⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⡇⠀⠀⠀⠀⠈⢳⡀⠀⠀⠀⠀⠀
        ⠀⠀⢰⠇⠀⠀⠀⠀⠀⠈⢷⠀⠀⠀⠀⠀
        ⣀⡴⠒⢾⡀⣀⡴⠒⢦⣀⢀⡼⠓⢦⣀
        ⣩⠴⠒⢶⣉⣉⡴⠒⢦⣉⣉⡴⠒⠶⣌
        ⠁⠀⠀⠀⠈⠁⠀⠀⠀⠈⠁⠀⠀⠀⠈
    )" << '\n';
    std::cout << "Jaws Awesomely Wrangles Scheme v0.1\n\n";
}

void printHelp()
{

    std::cout << "Available commands:\n"
              << "  exit       - Exit the Jaws REPL\n"
              << "  help       - Display this help message\n"
              << "\nBasic Jaws syntax:\n"
              << "  Numbers    - Integers (e.g., 42) or floating-point (e.g., 3.14)\n"
              << "  Strings    - Enclosed in double quotes (e.g., \"Hello, Jaws!\")\n"
              << "  Lists      - Enclosed in parentheses (e.g., (+ 1 2))\n"
              << "  Symbols    - Identifiers for variables and functions\n"
              << "\nBuilt-in functions:\n"
              << "  +          - Addition (e.g., (+ 1 2 3))\n"
              << "  define     - Define variables (e.g., (define x 10))\n"
              << "  if         - Conditional execution (e.g., (if (> x 0) \"positive\" \"non-positive\"))\n"
              << "\nEnter Scheme expressions to evaluate them" << std::endl;
}
void runPrompt()
{
    printJawsLogo();
    std::cout << "Welcome to the Jaws REPL!\n";
    std::cout << "Type 'exit' to quit, 'help' for commands.\n\n";

    auto scanner = std::make_shared<Scanner>();
    Parser parser;
    parser.initialize(scanner);
    Interpreter interpreter;

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

        if (input == "help") {
            printHelp();
            continue;
        }

        if (input == "jaws") {
            std::cout <<
                R"(
We're gonna need a bigger boat
------------------------------

⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⠦⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⢀⡠⢤⣤⣀⣀⣤⣄⣠⣼⣧⣶⠆⠀⠀⠀⡀⠀⠀⣀⠀⠀
⠀⠀⠀⠀⠀⠉⠉⠉⠛⠛⠛⠉⠭⠙⠛⠋⠛⠒⠒⠒⠋⠀⠂⠒⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⡶⠛⠻⢷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢠⣾⠋⠀⠀⠀⠀⠹⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⣰⣿⡇⠀⠀⠀⠀⠀⠰⠿⣿⣧⡀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣼⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠘⣿⣷⡀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⣠⣼⡿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠻⣿⣦⠀⠀⠀⠀⠀
⠀⠀⠀⠀⣿⠋⠀⢀⣠⣤⣴⣶⣾⣶⢶⣶⣦⣤⣤⡀⠈⠻⡀⠀⠀⠀⠀
⠀⠀⠀⣸⠃⢠⣾⡟⠋⡁⢸⡀⢠⡆⠀⣦⠀⡍⢙⠻⣦⠀⢱⡀⠀⠀⠀
⠀⠀⢰⡏⠀⣾⠁⢷⣄⣷⣾⣷⣿⣿⣾⣿⣼⣧⣾⢀⠿⡆⠀⣧⠀⠀⠀
⠀⢀⣿⡇⢰⣇⣷⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡔⡃⠀⣿⣧⠀⠀
⠀⣾⣿⠁⠀⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇⠀⣿⣿⡆⠀
⣸⣿⡏⠀⠀⢹⣿⣿⣿⠛⣿⠏⢹⣿⠃⢻⡟⢹⣿⣿⣿⠀⠀⢿⣿⣷⠀
⣿⡿⠀⠀⠀⠸⡿⠻⠙⣀⣨⠤⠤⠧⠤⠬⣀⡘⠹⠻⡻⠀⠀⠈⢿⣿⡇
⣿⠇⠀⠀⠀⠀⠀⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⣿⡇
⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡗
⠈⠳⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠀
                )" << std::endl;
            continue;
        }

        if (input.empty())
            continue;

        try {
            std::vector<Token> tokens = scanner->tokenize(input);
            parser.load(tokens);
            auto expr = parser.parse();
            if (expr) {
                std::optional<SchemeValue> result = interpreter.interpret(*expr);
                if (result) {
                    std::cout << result->toString() << std::endl;
                }
            }
        } catch (const ParseError& e) {
            e.printFormattedError();
        } catch (const InterpreterError& e) {
            e.printFormattedError();
        }
    }
}
