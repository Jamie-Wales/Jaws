#pragma once
#include <string>
#include <sys/resource.h>
#include <vector>

struct Options {
    std::string script;
    bool DEBUG;
    bool ANF;
    bool SSA;
    bool AST;
    bool _3AC;
};

struct Optimisations {
    bool DCE;
};

Options argParse(int argc, const char* argv[])
{
    std::vector<std::string> args;
    for (int i = 1; i < argc; i++) {
        args.emplace_back(argv[argc]);
    }
    Options opts = {};
    for (const auto& arg : args) {
        if (arg == "-d" || arg == "-debug") {
            opts.DEBUG = true;
        } else if (arg == "-anf") {
            opts.ANF = true;
        } else if (arg == "-ast") {
            opts.AST = true;
        }
    }
    return opts;
}
