#include "run.h"
#include "ANFTransformer.h"
#include "AssemblyGeneration.h"
#include "Error.h"
#include "Import.h"
#include "MacroTraits.h"
#include "QBEGenerator.h"
#include "ThreeAC.h"
#include "icons.h"
#include "interpret.h"
#include "optimise.h"
#include "parse.h"
#include "scan.h"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <unistd.h>

std::string formatScheme(const std::string& code)
{
    char filename[] = "/tmp/schemefmtXXXXXX.scm";
    int fd = mkstemps(filename, 4);
    if (fd == -1) {
        throw std::runtime_error("Could not create temp file");
    }
    {
        std::ofstream tmpOut(filename);
        if (!tmpOut) {
            close(fd);
            throw std::runtime_error("Could not write to temp file");
        }
        tmpOut << code;
    }
    close(fd);
    std::string cmd = "scheme-format -i \"";
    cmd += filename;
    cmd += "\"";

    int ret = std::system(cmd.c_str());
    if (ret != 0) {
        std::remove(filename);
        throw std::runtime_error("scheme-format failed (non-zero exit).");
    }
    std::ifstream tmpIn(filename);
    if (!tmpIn) {
        std::remove(filename);
        throw std::runtime_error("Failed to re-open temp file for reading");
    }
    std::string line, result;
    while (std::getline(tmpIn, line)) {
        result += line + "\n";
    }
    std::remove(filename);
    return result;
}

Options parse_args(std::vector<std::string> args)
{
    Options opts = {};
    for (size_t i = 0; i < args.size(); i++) {
        const std::string& arg = args[i];
        if (arg == "--prettyprint") {
            opts.prettyPrint = true;
            opts.printCode = true;
            opts.printMacro = true;
            opts.printANF = true;
            opts.print3AC = true;
        } else if ((arg == "--print" || arg == "-p")) {
            opts.printCode = true;
            opts.printMacro = true;
            opts.printANF = true;
            opts.print3AC = true;
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
        } else if ((arg == "--print3ac" || arg == "-3ac")) {
            opts.print3AC = true;
        } else if ((arg == "--compile" || arg == "-c")) {
            opts.compile = true;
            if (i + 1 < args.size() && args[i + 1][0] != '-') {
                opts.outputPath = args[++i];
            } else {
                opts.outputPath = "build";
            }
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
    std::stringstream ss;
    if (opts.printCode) {
        for (const auto& expression : *expressions) {
            ss << expression->toString() << "\n";
        }
        const auto output = opts.prettyPrint ? formatScheme(ss.str()) : ss.str();
        std::cout
            << "<| Original File |>\n"
            << output
            << std::endl;
        ss.str("");
        ss.clear();
    }

    if (opts.printAST) {
        std::cout << "\n<| Initial AST |>\n";
        for (const auto& expression : *expressions) {
            std::cout << expression->ASTToString() << "\n";
        }
        std::cout << "\n"
                  << std::endl;
        ss.str("");
        ss.clear();
    }

    auto withImports = import::processImports(*expressions);
    if (opts.printCode) {
        for (const auto& expression : withImports) {
            ss << expression->toString() << "\n";
        }
        const auto output = opts.prettyPrint ? formatScheme(ss.str()) : ss.str();
        std::cout << "\n<| File After Import |>\n"
                  << output
                  << std::endl;
        ss.str("");
        ss.clear();
    }

    if (opts.printAST) {
        std::cout << "\n<| AST After Import |>\n";
        for (const auto& expression : withImports) {
            std::cout << expression->ASTToString() << "\n";
        }
    }

    const auto expanded = macroexp::expandMacros(withImports);

    if (opts.printMacro) {
        ss.str("");
        ss.clear();
        for (const auto& expression : expanded) {
            ss << expression->toString() << "\n";
        }
        const auto& output = opts.prettyPrint ? formatScheme(ss.str()) : ss.str();

        std::cout << "\n<| Expanded Macro |>\n"
                  << output
                  << std::endl;

        ss.str("");
        ss.clear();

        if (opts.printAST) {
            std::cout << "\n<| AST After Macro Expansion |>\n";
            for (const auto& expression : expanded) {
                std::cout << expression->ASTToString() << "\n";
            }
            std::cout << "\n"
                      << std::endl;
        }
    }

    if (opts.compile) {
        auto anf = ir::ANFtransform(expanded);
        if (!anf.empty()) {
            if (opts.printANF) {
                std::cout << "\n<| ANF Before Optimization |>\n";
                for (const auto& tl : anf) {
                    std::cout << tl->toString() << "\n";
                }
                std::cout << std::endl;
            }

            if (opts.optimise) {
                auto [optimizedAnf, preGraph, postGraph] = optimise::optimise(anf);

                if (opts.printANF) {
                    std::cout << "\n<| ANF After Optimization |>\n";
                    for (const auto& tl : optimizedAnf) {
                        std::cout << tl->toString() << "\n";
                    }
                    std::cout << std::endl;
                }

                const auto _3ac = tac::anfToTac(optimizedAnf);
            } else {
                const auto _3ac = tac::anfToTac(anf);

                if (opts.print3AC) {
                    std::cout << "<| Three Address Code |>" << std::endl
                              << _3ac.toString() << std::endl;
                }

                try {
                    std::filesystem::create_directories(opts.outputPath);
                    auto outPath = std::filesystem::path(opts.outputPath);
                    std::string qbeFile = (outPath / "output.qbe").string();
                    std::string asmFile = (outPath / "output.asm").string();
                    std::string exeFile = (outPath / "scheme_program").string();

                    // Generate QBE IR
                    std::cout << "Generating QBE IR to: " << qbeFile << std::endl;
                    generateQBEIr(_3ac, qbeFile);

                    // Compile QBE IR to assembly (redirect output to file)
                    std::cout << "Compiling QBE IR to assembly: " << asmFile << std::endl;
                    std::string qbeCmd = "qbe " + qbeFile + " > " + asmFile;
                    if (system(qbeCmd.c_str()) != 0) {
                        throw std::runtime_error("QBE compilation failed: " + qbeCmd);
                    }

                    // Compile assembly to executable, linking with runtime
                    std::cout << "Compiling assembly to executable: " << exeFile << std::endl;
                    std::string linkCmd = "clang -o " + exeFile + " " + asmFile + " ../runtime/build/libruntime.a";
                    if (system(linkCmd.c_str()) != 0) {
                        throw std::runtime_error("Linking failed: " + linkCmd);
                    }

                    std::cout << "Successfully generated executable: " << exeFile << std::endl;
                    return;
                } catch (const std::filesystem::filesystem_error& e) {
                    throw std::runtime_error("Filesystem error: " + std::string(e.what()));
                }
            }
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
