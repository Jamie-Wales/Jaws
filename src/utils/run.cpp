#include "run.h"
#include "ANFTransformer.h"
#include "Error.h"
#include "Import.h"
#include "MacroTraits.h"
#include "icons.h"
#include "interpret.h"
#include "optimise.h"
#include "parse.h"
#include "scan.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>

Options parse_args(std::vector<std::string> args)
{
    Options opts = {};
    for (size_t i = 0; i < args.size(); i++) {
        const std::string& arg = args[i];
        if ((arg == "--print" || arg == "-p")) {
            opts.printCode = true;
            opts.printMacro = true;
            opts.printANF = true;
        } else if ((arg == "--no-opt" || arg == "-no")) {
            opts.optimise = false;
        } else if ((arg == "--script")) {
            if (i + 1 >= args.size()) {
                throw std::runtime_error("--script requires a file path");
            }
            opts.input = readFile(args[++i]);
            opts.file = true;
        } else if ((arg == "--printast" || arg == "-printast")) {
            opts.printAST = true;
        } else {
            throw std::runtime_error("Unknown argument: " + arg);
        }
    }
    return opts;
}

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

void evaluate(interpret::InterpreterState& state, Options& opts)
{
    auto tokens = scanner::tokenize(opts.input);
    auto expressions = parse::parse(std::move(tokens));
    if (!expressions) {
        std::cerr << "Parsing failed\n";
        return;
    }
    if (opts.printCode) {
        std::cout << "<| Original File |>\n";
        for (const auto& expression : *expressions) {
            std::cout << expression->toString() << "\n";
        }
        std::cout << "\n"
                  << std::endl;
    }
    if (opts.printAST) {
        std::cout << "\n<| Initial AST |>\n";
        for (const auto& expression : *expressions) {
            std::cout << expression->ASTToString() << "\n";
        }

        std::cout << "\n"
                  << std::endl;
    }

    auto withImports = import::processImports(*expressions);
    if (opts.printCode) {
        std::cout << "\n<| File After Import |>\n";
        for (const auto& expression : withImports) {
            std::cout << expression->toString() << "\n";
        }
        std::cout << "\n"
                  << std::endl;
    }

    if (opts.printAST) {
        std::cout << "\n<| AST After Import |>\n";
        for (const auto& expression : withImports) {
            std::cout << expression->ASTToString() << "\n";
        }
    }

    const auto expanded = macroexp::expandMacros(withImports);

    if (opts.printMacro) {
        std::cout << "\n<| Expanded Macro |>\n";
        for (const auto& expression : expanded) {
            std::cout << expression->toString() << "\n";
        }

        std::cout << "\n"
                  << std::endl;
        if (opts.printAST) {
            std::cout << "\n<| AST After Macro Expansion |>\n";
            for (const auto& expression : expanded) {
                std::cout << expression->ASTToString() << "\n";
            }
            std::cout << "\n"
                      << std::endl;
        }
    }
    if (opts.optimise) {
        auto anf = ir::ANFtransform(expanded);
        if (!anf.empty()) {
            anf = optimise::optimise(anf, opts.printANF);
        }
    }
    auto val = interpret::interpret(state, expanded);
    if (val) {
        std::cout << val->toString() << std::endl;
    }
}

void runFile(Options& opts)
{
    try {
        auto state = interpret::createInterpreter();
        evaluate(state, opts);
    } catch (const ParseError& e) {
        e.printFormattedError();
    } catch (const InterpreterError& e) {
        e.printFormattedError();
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
}

void runPrompt(Options& opts)
{
    printJawsLogo();
    std::cout << "<| Welcome to the Jaws REPL |>\n";
    std::cout << "<| Type 'exit' to quit, '(help)' for commands |>\n";

    auto state = interpret::createInterpreter();

    while (true) {
        std::cout << "jaws: |> ";
        std::string input;

        if (!std::getline(std::cin, input))
            break;
        if (input == "exit") {
            std::cout << "Fin-ishing up. Goodbye!\n";
            break;
        }
        if (input.empty())
            continue;

        try {
            opts.input = input;
            evaluate(state, opts);
        } catch (const ParseError& e) {
            e.printFormattedError();
        } catch (const InterpreterError& e) {
            e.printFormattedError();
        } catch (const std::exception& e) {
            std::cerr << "Error: " << e.what() << std::endl;
        }
    }
}
