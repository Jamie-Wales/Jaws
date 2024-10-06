#include "run.h"
#include <cstdlib>
#include <iostream>

int main(const int argc, const char* argv[])
{
    if (argc == 2) {
        runFile(argv[1]);
    } else {
        std::cout << "Usage: " << argv[0] << " [script]" << std::endl;
        return EXIT_FAILURE;
    }
}
