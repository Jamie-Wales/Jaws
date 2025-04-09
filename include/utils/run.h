#pragma once
#include "Import.h"
#include "interpret.h"
#include <string>
#include <vector>

struct Options {
    bool prettyPrint = false;
    bool printAST = false;
    bool printMacro = false;
    bool printANF = false;
    bool print3AC = false;
    bool compile = false;
    bool optimise = true;
    bool file = false;
    bool printCode = false;
    std::string input;
    std::string outputPath;
};

Options parse_args(std::vector<std::string> args);
std::string readFile(const std::string& path);
void runFile(Options& opts);
void runPrompt(Options& opts);
void evaluate(
    interpret::InterpreterState& state,
    import::LibraryRegistry& registry,
    Options& opts,
    std::shared_ptr<pattern::MacroEnvironment> macroEnv);
void testMacros();
void prepareInterpreterEnvironment(const import::ProcessedCode& code, interpret::InterpreterState& state);
std::vector<std::shared_ptr<Expression>> prepareCompilerInput(const import::ProcessedCode& code);
