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
void prepareInterpreterEnvironment(const import::ProcessedCode& code, interpret::InterpreterState& state)
{
    for (const auto& libData : code.importedLibrariesData) {
        for (const auto& [name, binding] : libData.exportedBindings) {
            if (binding.type == import::ExportedBinding::Type::VALUE) {
                interpret::interpret(state, binding.definition);
            }
        }
    }
}

std::vector<std::shared_ptr<Expression>> prepareCompilerInput(const import::ProcessedCode& code)
{
    std::vector<std::shared_ptr<Expression>> compilerInputList;
    for (const auto& libData : code.importedLibrariesData) {
        for (const auto& [name, binding] : libData.exportedBindings) {
            compilerInputList.push_back(binding.definition);
        }
    }
    return compilerInputList;
}
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
    auto expressionsOpt = parse::parse(std::move(tokens));
    if (!expressionsOpt) {
        std::cerr << "Parsing failed\n";
        return;
    }
    auto parsedExpressions = *expressionsOpt;

    std::stringstream ss_print_buffer;
    if (opts.printCode) {
        ss_print_buffer.str("");
        ss_print_buffer.clear();
        for (const auto& expression : parsedExpressions) {
            ss_print_buffer << expression->toString() << "\n";
        }
        const auto output = opts.prettyPrint ? formatScheme(ss_print_buffer.str()) : ss_print_buffer.str();
        std::cout << "<| Original Code |>\n"
                  << output << std::endl;
    }
    if (opts.printAST) {
        std::cout << "\n<| Initial AST |>\n";
        for (const auto& expression : parsedExpressions) {
            std::cout << expression->ASTToString() << "\n";
        }
        std::cout << "\n"
                  << std::endl;
    }

    // 2. Process Imports (Decoupled)
    import::ProcessedCode processedCode;
    try {
        processedCode = import::processImports(parsedExpressions);
    } catch (const std::exception& e) {
        std::cerr << "[Import Error] " << e.what() << std::endl;
        return;
    }

    if (opts.printCode || opts.printAST) {
        std::vector<std::shared_ptr<Expression>> afterImportExpressions = processedCode.remainingExpressions;
        for (const auto& libData : processedCode.importedLibrariesData) {
            for (const auto& [name, binding] : libData.exportedBindings) {
                afterImportExpressions.push_back(binding.definition);
            }
        }

        if (opts.printCode) {
            ss_print_buffer.str("");
            ss_print_buffer.clear();
            for (const auto& expression : afterImportExpressions) {
                ss_print_buffer << expression->toString() << "\n";
            }
            const auto output = opts.prettyPrint ? formatScheme(ss_print_buffer.str()) : ss_print_buffer.str();
            std::cout << "\n<| Code After Import Processing |>\n"
                      << output << std::endl;
        }
        if (opts.printAST) {
            std::cout << "\n<| AST After Import Processing |>\n";
            for (const auto& expression : afterImportExpressions) {
                std::cout << expression->ASTToString() << "\n";
            }
            std::cout << "\n"
                      << std::endl;
        }
    }
    std::vector<std::shared_ptr<Expression>> finalExpressions;
    try {
        auto macroEnv = std::make_shared<pattern::MacroEnvironment>();

        // Populate with imported macros
        for (const auto& libData : processedCode.importedLibrariesData) {
            for (const auto& [name, binding] : libData.exportedBindings) {
                if (binding.type == import::ExportedBinding::Type::SYNTAX) {
                    if (auto* ds = std::get_if<DefineSyntaxExpression>(&binding.definition->as)) {
                        macroEnv->defineMacro(name, ds->rule);
                    }
                }
            }
        }

        // Add top-level macros from the current file/input & prepare list for expansion
        std::vector<std::shared_ptr<Expression>> expressionsToExpand;
        for (const auto& expr : processedCode.remainingExpressions) {
            if (const auto* de = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                if (std::holds_alternative<SyntaxRulesExpression>(de->rule->as)) {
                    macroEnv->defineMacro(de->name.token.lexeme, de->rule);
                }
            } else {
                expressionsToExpand.push_back(expr);
            }
        }

        finalExpressions = macroexp::expandMacros(expressionsToExpand, macroEnv);

    } catch (const std::exception& e) {
        std::cerr << "[Macro Expansion Error] " << e.what() << std::endl;
        return;
    }

    if (opts.printMacro) {
        ss_print_buffer.str("");
        ss_print_buffer.clear();
        for (const auto& expression : finalExpressions) {
            ss_print_buffer << expression->toString() << "\n";
        }
        const auto output = opts.prettyPrint ? formatScheme(ss_print_buffer.str()) : ss_print_buffer.str();
        std::cout << "\n<| Expanded Macro |>\n"
                  << output << std::endl;
    }
    if (opts.printAST && (opts.printMacro || opts.prettyPrint)) { // Optionally print AST again after expansion
        std::cout << "\n<| AST After Macro Expansion |>\n";
        for (const auto& expression : finalExpressions) {
            std::cout << expression->ASTToString() << "\n";
        }
        std::cout << "\n"
                  << std::endl;
    }

    try {
        if (opts.compile) {
            auto importedDefinitions = prepareCompilerInput(processedCode);
            importedDefinitions.insert(importedDefinitions.end(), finalExpressions.begin(), finalExpressions.end());
            auto anf = ir::ANFtransform(importedDefinitions);
            if (!anf.empty()) {
                const auto* optimizedAnfPtr = &anf; // Pointer to use correct ANF list
                std::vector<std::shared_ptr<ir::ANF>> optimizedAnfStorage;

                if (opts.printANF) { // Print ANF before optimization
                    std::cout << "\n<| ANF Before Optimization |>\n";
                    for (const auto& tl : anf) {
                        std::cout << tl->toString() << "\n";
                    }
                    std::cout << std::endl;
                }

                if (opts.optimise) {
                    auto [optResult, preGraph, postGraph] = optimise::optimise(anf);
                    if (opts.printANF) { // Print ANF after optimization
                        std::cout << "\n<| ANF After Optimization |>\n";
                        for (const auto& tl : *optimizedAnfPtr) {
                            std::cout << tl->toString() << "\n";
                        }
                        std::cout << std::endl;
                    }
                }

                const auto _3ac = tac::anfToTac(*optimizedAnfPtr);

                if (opts.print3AC) { // Print 3AC
                    std::cout << "\n<| Three Address Code |>\n"
                              << _3ac.toString() << std::endl;
                }

                // Compilation output steps
                std::filesystem::create_directories(opts.outputPath);
                auto outPath = std::filesystem::path(opts.outputPath);
                std::string qbeFile = (outPath / "output.qbe").string();
                std::string asmFile = (outPath / "output.asm").string();
                std::string exeFile = (outPath / "scheme_program").string();

                std::cout << "Generating QBE IR to: " << qbeFile << std::endl;
                generateQBEIr(_3ac, qbeFile);

                std::cout << "Compiling QBE IR to assembly: " << asmFile << std::endl;
                std::string qbeCmd = "qbe " + qbeFile + " > " + asmFile;
                if (system(qbeCmd.c_str()) != 0) {
                    throw std::runtime_error("QBE compilation failed");
                }

                std::cout << "Compiling assembly to executable: " << exeFile << std::endl;
                std::string linkCmd = "clang -o " + exeFile + " " + asmFile + " ../runtime/build/libruntime.a";
                if (system(linkCmd.c_str()) != 0) {
                    throw std::runtime_error("Linking failed");
                }

                std::cout << "Successfully generated executable: " << exeFile << std::endl;
                return;

            } else {
                std::cerr << "ANF transformation resulted in empty output." << std::endl;
            }
        } else {
            prepareInterpreterEnvironment(processedCode, state);
            auto val = interpret::interpret(state, finalExpressions);
            if (val) {
                std::cout << val->toString() << std::endl;
            }
        }
    } catch (const InterpreterError& e) {
        e.printFormattedError();
    } catch (const ParseError& e) {
        e.printFormattedError();
    } catch (const std::exception& e) {
        std::cerr << "[Runtime Error] " << e.what() << std::endl;
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
