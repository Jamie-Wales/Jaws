#include "run.h"
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
    auto file = readFile("../example.scm");
    auto tokens = scanner::tokenize(file);
    auto expressions = parse::parse(std::move(tokens));
    for (const auto& expr : *expressions) {
        interpret::interpret(state, expr);
    }

    tokens = scanner::tokenize(input);
    expressions = parse::parse(std::move(tokens));
    for (auto& expr : *expressions)
        interpret::interpret(state, expr);
}

void runFile(const std::string& path)
{
    try {
        auto state = interpret::createInterpreter();
        evaluate(state, readFile(path));
        std::cout << state.output.str();
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

        if (input == "jaws") {
            std::cout << jaws2 << std::endl;
            continue;
        }

        if (input.empty()) {
            continue;
        }

        try {
            evaluate(state, input);
            std::cout << state.output.str();
            state.output.str("");
            state.output.clear();
        } catch (const ParseError& e) {
            e.printFormattedError();
        } catch (const InterpreterError& e) {
            e.printFormattedError();
        }
    }
}
