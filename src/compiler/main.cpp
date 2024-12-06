#include "Error.h"
#include "run.h"
#include <cstdlib>
#include <iostream>
#include <string>

void printUsage(const char* programName)
{
    std::cerr << "Usage:\n"
              << "  " << programName << "                  Start interactive REPL\n"
              << "  " << programName << " --script <file>  Run Scheme script file\n"
              << "  " << programName << " -t               Start TUI mode\n";
}

int main(int argc, const char* argv[])
{
    if (argc == 1) {
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

    std::cerr << "Error: Unknown argument '" << arg1 << "'\n";
    printUsage(argv[0]);
    return EXIT_FAILURE;
}
