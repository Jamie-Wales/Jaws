#include "ftxui/component/component.hpp"
#include "ftxui/component/component_base.hpp"
#include "ftxui/component/component_options.hpp"
#include "ftxui/component/screen_interactive.hpp"
#include "ftxui/dom/direction.hpp"
#include "ftxui/dom/elements.hpp"
#include "ftxui/screen/color.hpp"

#include "Error.h"
#include "Interpreter.h"
#include "Parser.h"
#include "Scanner.h"
#include "Token.h"
#include "Value.h"

#include "run.h"
#include <algorithm>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

using namespace ftxui;

void printUsage(const char* programName) {
    std::cerr << "Usage:\n"
              << "  " << programName << "                  Start interactive REPL\n"
              << "  " << programName << " --script <file>  Run Scheme script file\n"
              << "  " << programName << " -t               Start TUI mode\n";
}

void startTUI() {
    std::string input;
    std::string output;

    auto scanner = std::make_shared<Scanner>();
    Parser parser;
    parser.initialize(scanner);
    Interpreter interpreter;

    // Input box options
    InputOption input_option;
    input_option.on_enter = [&] {
        if (std::count(input.begin(), input.end(), '(') != std::count(input.begin(), input.end(), ')')) {
            return;
        }
        try {
            std::vector<Token> tokens = scanner->tokenize(input);
            parser.load(tokens);
            auto expr = parser.parse();

            if (expr) {
                std::stringstream result_stream;
                for (auto& ex : *expr) {
                    std::optional<SchemeValue> result = interpreter.interpret(ex);
                    if (result) {
                        result_stream << result->toString() << "\n";
                    }
                }
                output = result_stream.str();
            }
        } catch (const ParseError& e) {
            output = "Parse Error: " + std::string(e.what());
        } catch (const InterpreterError& e) {
            output = "Interpreter Error: " + std::string(e.what());
        }

        input.clear();
    };

    auto input_box = Input(&input, "Type your Scheme code here...", input_option);
    
    // Syntax highlighting function
    auto SyntaxHighlight = [](const std::string& code) -> Element {
        Scanner scanner = {};
        Elements elements = {};

        auto tokens = scanner.tokenize(code);
        for (const auto& token : tokens) {
            switch (token.type) {
            case Tokentype::INTEGER:
            case Tokentype::FLOAT:
            case Tokentype::STRING:
            case Tokentype::QUOTE_SYMBOL:
            case Tokentype::TRUE:
            case Tokentype::FALSE:
            case Tokentype::RATIONAL:
            case Tokentype::COMPLEX:
                elements.push_back(text(token.lexeme) | color(Color::HotPink));
                break;
            case Tokentype::SYMBOL:
            case Tokentype::PLUS:
            case Tokentype::MINUS:
            case Tokentype::MULTIPLY:
            case Tokentype::DIVIDE:
            case Tokentype::EQUAL:
            case Tokentype::LESS_THAN:
            case Tokentype::GREATER_THAN:
                elements.push_back(text(token.lexeme) | color(Color::Yellow));
                break;
            case Tokentype::COMMENT:
            case Tokentype::EOF_TOKEN:
            case Tokentype::ERROR:
                break;
            default:
                elements.push_back(text(token.lexeme) | color(Color::White));
                break;
            }
            elements.push_back(text(" "));
        }
        return hbox(std::move(elements));
    };

    auto container = Container::Vertical({
        input_box,
    });

    auto renderer = Renderer(container, [&] {
        return vbox({
            hbox({
                text("jaws: |> ") | color(Color::GreenLight),
                SyntaxHighlight(input),
            }),
            separator(),
            text("Output:") | color(Color::Cyan),
            text(output)
        });
    });

    auto screen = ScreenInteractive::FullscreenPrimaryScreen();
    screen.Loop(renderer);
}

int main(int argc, const char* argv[]) {
    if (argc == 1) {
        // No arguments - start REPL
        runPrompt();
        return EXIT_SUCCESS;
    }

    std::string arg1 = argv[1];
    
    if (arg1 == "--help" || arg1 == "-h") {
        printUsage(argv[0]);
        return EXIT_SUCCESS;
    }
    
    if (arg1 == "--script") {
        if (argc != 3) {
            std::cerr << "Error: --script requires a file path\n";
            printUsage(argv[0]);
            return EXIT_FAILURE;
        }
        runFile(argv[2]);
        return EXIT_SUCCESS;
    }
    
    if (arg1 == "-t") {
        startTUI();
        return EXIT_SUCCESS;
    }

    std::cerr << "Error: Unknown argument '" << arg1 << "'\n";
    printUsage(argv[0]);
    return EXIT_FAILURE;
}
