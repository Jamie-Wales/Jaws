#include "run.h"
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

void printUsage(const char* programName)
{
    std::cerr << "Usage:\n"
              << "  " << programName << "                  Start interactive REPL\n"
              << "  " << programName << " --script <file>  Run Scheme script file\n"
              << "  " << programName << " --print, -p      Print AST and ANF\n"
              << "  " << programName << " --no-opt, -no    Disable optimizations\n"
              << "  " << programName << " -h, --help       Show this help message\n";
}

int main(int argc, const char* argv[])
{
    testMacros();
    std::vector<std::string> args;
    for (int i = 1; i < argc; i++) {
        args.emplace_back(argv[i]);
    }

    try {
        Options opts;
        if (args.empty()) {
            opts.file = false;
            runPrompt(opts);
            return EXIT_SUCCESS;
        }

        if (args[0] == "--help" || args[0] == "-h") {
            printUsage(argv[0]);
            return EXIT_SUCCESS;
        }
        try {
            opts = parse_args(args);
        } catch (const std::runtime_error& e) {
            std::cerr << "Error: " << e.what() << "\n";
            printUsage(argv[0]);
            return EXIT_FAILURE;
        }

        opts.file ? runFile(opts) : runPrompt(opts);
        return EXIT_SUCCESS;

    } catch (const std::exception& e) {
        std::cerr << "Fatal error: " << e.what() << "\n";
        return EXIT_FAILURE;
    }
}
