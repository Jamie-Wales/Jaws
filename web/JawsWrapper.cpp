#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>

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

using namespace emscripten;

class JawsWrapper {
private:
    interpret::InterpreterState state;
    std::stringstream ss;
    std::stringstream outputCapture;

    void resetStream()
    {
        ss.str("");
        ss.clear();
    }

    void resetOutputCapture()
    {
        outputCapture.str("");
        outputCapture.clear();
    }

    std::vector<std::shared_ptr<Expression>> parseInput(const std::string& input)
    {
        auto tokens = scanner::tokenize(input);
        auto expressions = parse::parse(std::move(tokens));
        if (!expressions) {
            throw std::runtime_error("Parsing failed");
        }
        return *expressions;
    }

    // New method to capture stdout during evaluation
    std::streambuf* redirectStdout()
    {
        resetOutputCapture();
        return std::cout.rdbuf(outputCapture.rdbuf());
    }

    void restoreStdout(std::streambuf* oldBuf)
    {
        std::cout.rdbuf(oldBuf);
    }

public:
    JawsWrapper()
        : state(interpret::createInterpreter())
    {
    }

    // Updated to return a JavaScript object with both result and stdout
    emscripten::val evaluate(const std::string& input)
    {
        try {
            // Redirect stdout to capture display output
            auto oldBuf = redirectStdout();
            
            auto expressions = parseInput(input);
            const auto withImports = import::processImports(expressions);
            const auto expanded = macroexp::expandMacros(withImports);
            std::string result;
            for (const auto& expr : expanded) {
                std::vector<std::shared_ptr<Expression>> single_expr = { expr };
                auto val = interpret::interpret(state, single_expr);
                if (val) {
                    result = val->toString();
                }
            }
            
            // Get the captured stdout and restore cout
            std::string stdoutCapture = outputCapture.str();
            restoreStdout(oldBuf);
            
            // Create and return a JavaScript object with both result and stdout
            emscripten::val retVal = emscripten::val::object();
            retVal.set("result", result);
            retVal.set("stdout", stdoutCapture);
            return retVal;
        } catch (const std::exception& e) {
            // For errors, return an object with error message
            emscripten::val retVal = emscripten::val::object();
            retVal.set("error", std::string("Error: ") + e.what());
            return retVal;
        }
    }

    std::string getMacroExpanded(const std::string& input)
    {
        try {
            auto expressions = parseInput(input);
            const auto withImports = import::processImports(expressions);
            const auto expanded = macroexp::expandMacros(withImports);

            resetStream();
            for (const auto& expr : expanded) {
                ss << expr->toString() << "\n";
            }
            return ss.str();
        } catch (const std::exception& e) {
            return std::string("Error: ") + e.what();
        }
    }

    std::string getANF(const std::string& input)
    {
        try {
            auto expressions = parseInput(input);
            const auto withImports = import::processImports(expressions);
            const auto expanded = macroexp::expandMacros(withImports);
            auto anf = ir::ANFtransform(expanded);

            resetStream();
            for (const auto& tl : anf) {
                ss << tl->toString() << "\n";
            }
            return ss.str();
        } catch (const std::exception& e) {
            return std::string("Error: ") + e.what();
        }
    }

    std::string getOptimizedANF(const std::string& input)
    {
        try {
            auto expressions = parseInput(input);
            const auto withImports = import::processImports(expressions);
            const auto expanded = macroexp::expandMacros(withImports);
            auto anf = ir::ANFtransform(expanded);
            anf = optimise::optimise(anf, true);

            resetStream();
            for (const auto& tl : anf) {
                ss << tl->toString() << "\n";
            }
            return ss.str();
        } catch (const std::exception& e) {
            return std::string("Error: ") + e.what();
        }
    }

    std::string getThreeAC(const std::string& input)
    {
        try {
            auto expressions = parseInput(input);
            const auto withImports = import::processImports(expressions);
            const auto expanded = macroexp::expandMacros(withImports);
            auto anf = ir::ANFtransform(expanded);
            anf = optimise::optimise(anf, false);
            const auto _3ac = tac::anfToTac(anf);
            return _3ac.toString();
        } catch (const std::exception& e) {
            return std::string("Error: ") + e.what();
        }
    }

    std::string getAST(const std::string& input)
    {
        try {
            auto expressions = parseInput(input);
            resetStream();
            for (const auto& expr : expressions) {
                ss << expr->ASTToString() << "\n";
            }
            return ss.str();
        } catch (const std::exception& e) {
            return std::string("Error: ") + e.what();
        }
    }
};

EMSCRIPTEN_BINDINGS(jaws_module)
{
    class_<JawsWrapper>("JawsWrapper")
        .constructor<>()
        .function("evaluate", &JawsWrapper::evaluate)
        .function("getMacroExpanded", &JawsWrapper::getMacroExpanded)
        .function("getANF", &JawsWrapper::getANF)
        .function("getOptimizedANF", &JawsWrapper::getOptimizedANF)
        .function("getThreeAC", &JawsWrapper::getThreeAC)
        .function("getAST", &JawsWrapper::getAST);
}
