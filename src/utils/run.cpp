#include "run.h"
#include "ANFTransformer.h"
#include "Error.h"
#include "MacroExpander.h"
#include "Procedure.h"
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
            opts.printAST = true;
            opts.printANF = true;
        } else if ((arg == "--no-opt" || arg == "-no")) {
            opts.optimise = false;
        } else if (arg == "--script") {
            if (i + 1 >= args.size()) {
                throw std::runtime_error("--script requires a file path");
            }
            opts.input = readFile(args[++i]);
            opts.file = true;
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

    if (opts.printAST) {
        std::cout << "Initial Abstract Syntax Tree:\n";
        for (const auto& expression : *expressions) {
            std::cout << expression->toString() << "\n";
        }
    }

    std::cout << "DEBUG: First pass - Processing macro definitions\n";
    for (const auto& expr : *expressions) {
        if (auto* define_syntax = std::get_if<DefineSyntaxExpression>(&expr->as)) {
            std::cout << "DEBUG: Found define-syntax for " << define_syntax->name.lexeme << "\n";
            auto val = interpret::interpret(state, expr);
            if (!val) {
                std::cerr << "Failed to process macro definition\n";
            }
        }
    }

    // Second pass: Expand macros
    std::cout << "DEBUG: Second pass - Expanding macros\n";
    std::vector<std::shared_ptr<Expression>> expanded;
    for (const auto& expr : *expressions) {
        // Skip define-syntax forms in expansion phase
        if (std::get_if<DefineSyntaxExpression>(&expr->as)) {
            std::cout << "DEBUG: Skipping define-syntax in expansion phase\n";
            expanded.push_back(expr);
            continue;
        }

        if (auto expandedExpr = pattern::MacroExpander::expandMacrosIn(state, expr)) {
            expanded.push_back(*expandedExpr);
            if (opts.printAST) {
                std::cout << "\nExpanded Expression:\n"
                          << (*expandedExpr)->toString() << "\n";
            }
        } else {
            expanded.push_back(expr);
        }
    }

    auto anf = ir::ANFtransform(expanded);
    if (opts.optimise) {
        anf = optimise::optimise(anf);
    }
    if (opts.printANF) {
        std::cout << "ANF representation:\n";
        for (const auto& expr : anf) {
            std::cout << expr->toString() << "\n";
        }
        std::cout << "\n";
    }

    // Final pass: Interpret the expanded code
    std::cout << "DEBUG: Final pass - Interpreting expanded code\n";
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
    std::cout << "Welcome to the Jaws REPL!\n";
    std::cout << "Type 'exit' to quit, '(help)' for commands.\n\n";
    auto state = interpret::createInterpreter();
    while (true) {
        std::cout << "jaws: |> ";
        std::string input;
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
void testMacros()
{
    Options opts;
    opts.printAST = true;
    opts.optimise = false;

    std::vector<std::string> tests = {
        // Simple begin macro
        R"(
            (define-syntax my-begin
              (syntax-rules ()
                ((my-begin expr ...)
                 ((lambda () expr ...)))))

            (my-begin 
              (display "hello")
              (display "world"))
        )",

        R"(
            (define-syntax constant-macro
              (syntax-rules ()
                ((constant-macro)
                 42)))
            
            (constant-macro)
        )",

        R"(
            (define-syntax display-twice
              (syntax-rules ()
                ((display-twice expr)
                 (begin 
                   (display expr)
                   (display expr)))))
            
            (display-twice "hello")
        )"
    };

    auto state = interpret::createInterpreter();

    for (const auto& test : tests) {
        try {
            std::cout << "\n=== Testing Macro ===\n";
            std::cout << "Input:\n"
                      << test << "\n";
            opts.input = test;

            auto tokens = scanner::tokenize(opts.input);
            std::cout << "\nTokens:\n";
            for (const auto& token : tokens) {
                std::cout << "  " << token.lexeme << " (type: " << static_cast<int>(token.type) << ")\n";
            }

            evaluate(state, opts);
            std::cout << "=== End Test ===\n";
        } catch (const std::exception& e) {
            std::cerr << "Test failed: " << e.what() << "\n";
        }
    }
}
