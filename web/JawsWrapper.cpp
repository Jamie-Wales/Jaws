#include "ANFTransformer.h"
#include "Error.h"
#include "Import.h"
#include "MacroTraits.h"
#include "ThreeAC.h"
#include "interpret.h"
#include "optimise.h"
#include "parse.h"
#include "run.h"
#include "scan.h"
#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <iostream>
#include <sstream>
#include <string>

using namespace emscripten;

class OutputCapture {
private:
    std::stringstream buffer;
    std::streambuf* oldCoutBuffer;
    bool capturing;

public:
    OutputCapture()
        : oldCoutBuffer(nullptr)
        , capturing(false)
    {
    }

    void start()
    {
        if (!capturing) {
            oldCoutBuffer = std::cout.rdbuf();
            std::cout.rdbuf(buffer.rdbuf());
            capturing = true;
        }
    }

    void stop()
    {
        if (capturing) {
            std::cout.rdbuf(oldCoutBuffer);
            capturing = false;
        }
    }

    std::string getOutput()
    {
        return buffer.str();
    }

    void clear()
    {
        buffer.str("");
        buffer.clear();
    }

    ~OutputCapture()
    {
        stop();
    }
};

class JawsWrapper {
private:
    interpret::InterpreterState interpreterState;
    OutputCapture outputCapture;
    Options opts;

public:
    JawsWrapper()
        : interpreterState(interpret::createInterpreter())
    {
        // Default options
        opts.printCode = false;
        opts.printMacro = false;
        opts.printANF = false;
        opts.print3AC = false;
        opts.printAST = false;
        opts.optimise = true;
        opts.compile = false;
        opts.file = false;
    }

    std::string evaluate(const std::string& input)
    {
        try {
            outputCapture.clear();
            outputCapture.start();
            opts.input = input;
            auto tokens = scanner::tokenize(input);
            auto expressions = parse::parse(std::move(tokens));
            if (!expressions) {
                outputCapture.stop();
                return "Parsing failed";
            }
            auto withImports = import::processImports(*expressions);
            const auto expanded = macroexp::expandMacros(withImports);
            if (opts.optimise) {
                auto anf = ir::ANFtransform(expanded);
                if (!anf.empty()) {
                    anf = optimise::optimise(anf, opts.printANF);

                    if (opts.print3AC) {
                        const auto _3ac = tac::anfToTac(anf);
                        std::cout << "\n<| Three Address Code |>\n"
                                  << _3ac.toString() << std::endl;
                    }
                }
            }
            auto val = interpret::interpret(interpreterState, expanded);
            if (val) {
                std::cout << val->toString() << std::endl;
            }
            outputCapture.stop();
            return outputCapture.getOutput();
        } catch (const ParseError& e) {
            outputCapture.stop();
            return std::string("Parse Error: ") + e.what();
        } catch (const InterpreterError& e) {
            outputCapture.stop();
            return std::string("Interpreter Error: ") + e.what();
        } catch (const std::exception& e) {
            outputCapture.stop();
            return std::string("Error: ") + e.what();
        }
    }

    void setOption(const std::string& option, bool value)
    {
        if (option == "printCode")
            opts.printCode = value;
        else if (option == "printMacro")
            opts.printMacro = value;
        else if (option == "printANF")
            opts.printANF = value;
        else if (option == "print3AC")
            opts.print3AC = value;
        else if (option == "printAST")
            opts.printAST = value;
        else if (option == "optimise")
            opts.optimise = value;
    }

    std::string getEnvironment() const
    {
        return "Jaws Scheme WebAssembly Environment";
    }
};

// Emscripten bindings
EMSCRIPTEN_BINDINGS(jaws_module)
{
    class_<JawsWrapper>("JawsWrapper")
        .constructor<>()
        .function("evaluate", &JawsWrapper::evaluate)
        .function("setOption", &JawsWrapper::setOption)
        .function("getEnvironment", &JawsWrapper::getEnvironment);
}
