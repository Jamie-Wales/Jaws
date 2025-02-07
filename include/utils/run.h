#pragma once

#include "interpret.h"
#include <string>
#include <vector>

struct Options {
    std::string input;
    bool file = false;
    bool debug = false;
    bool printAST = false;
    bool optimise = true;
    bool printANF = false;
    bool printMacro = false;
};

Options parse_args(std::vector<std::string> args);
std::string readFile(const std::string& path);
void runFile(Options& opts);
void runPrompt(Options& opts);
void evaluate(interpret::InterpreterState& state, Options& opts);
void testMacros();
