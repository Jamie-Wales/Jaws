#include "run.h"
#include <cstdlib>
#include <iostream>
#include <string>

int main(const int argc, const char* argv[])
{
    if (argc > 1) {
        if (std::string(argv[1]) == "--script" && argc == 3) {
            runFile(argv[2]);
        } else {
            std::cout << "Usage: " << argv[0] << " [--script <filename>]" << std::endl;
            return EXIT_FAILURE;
        }
    } else {
        runPrompt();
    }
    return EXIT_SUCCESS;
}
