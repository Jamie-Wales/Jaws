#pragma once
#include "interpret.h"
#include <string>
#include <vector>

struct Options {
    bool prettyPrint = false;
    bool printAST = false;
    bool printMacro = false;
    bool printANF = false;
    bool print3AC = false; // New flag for printing 3AC
    bool compile = false; // New flag for compilation
    bool optimise = true;
    bool file = false;
    bool printCode = false;
    std::string input;
    std::string outputPath; // Add output path for compilation
};

Options parse_args(std::vector<std::string> args);
std::string readFile(const std::string& path);
void runFile(Options& opts);
void runPrompt(Options& opts);
void evaluate(interpret::InterpreterState& state, Options& opts);
void testMacros();
