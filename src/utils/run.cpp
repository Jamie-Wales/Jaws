#include "run.h"
#include "ATransformer.h"
#include "Error.h"
#include "ExpressionUtils.h"
#include "Procedure.h"
#include "icons.h"
#include "interpret.h"
#include "parse.h"
#include "scan.h"
#include <fstream>
#include <iostream>
#include <sstream>

std::string readFile(const std::string& path)
{
    std::ifstream file(path);
    if (!file) {
        throw std::runtime_error("Unable to open file: " + path);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

void evaluate(interpret::InterpreterState& state, const std::string& input)
{
    auto tokens = scanner::tokenize(input);
    auto expressions = parse::parse(std::move(tokens));
    auto anf = ir::ANFtransform(*expressions);

    for (const auto& expr : anf) {
        std::cout << expr->toString() << std::endl;
    }
    auto val = interpret::interpret(state, *expressions);
    if (val) {
        std::cout << val->toString() << std::endl;
    }
}

void runFile(const std::string& path)
{
    try {
        auto state = interpret::createInterpreter();
        evaluate(state, readFile(path));
    } catch (const ParseError& e) {
        e.printFormattedError();
    } catch (const InterpreterError& e) {
        e.printFormattedError();
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
}

void runPrompt()
{
    printJawsLogo();
    std::cout << "Welcome to the Jaws REPL!\n";
    std::cout << "Type 'exit' to quit, '(help)' for commands.\n\n";

    auto state = interpret::createInterpreter();
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

        if (input.empty()) {
            continue;
        }

        try {
            evaluate(state, input);
        } catch (const ParseError& e) {
            e.printFormattedError();
        } catch (const InterpreterError& e) {
            e.printFormattedError();
        }
    }
}
