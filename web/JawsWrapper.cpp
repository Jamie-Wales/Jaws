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

    struct CompilationStages {
        std::vector<std::shared_ptr<Expression>> parsed;
        std::vector<std::shared_ptr<Expression>> withImports;
        std::vector<std::shared_ptr<Expression>> expanded;
        std::vector<std::shared_ptr<ir::TopLevel>> anf;
        std::vector<std::shared_ptr<ir::TopLevel>> optimizedAnf;
        std::string preDependencyGraph;
        std::string postDependencyGraph;
    };

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

    CompilationStages compileStages(const std::string& input)
    {
        CompilationStages stages;

        // Initial parsing
        auto tokens = scanner::tokenize(input);
        auto expressions = parse::parse(std::move(tokens));
        if (!expressions) {
            throw std::runtime_error("Parsing failed");
        }
        stages.parsed = *expressions;

        // Process imports
        stages.withImports = import::processImports(stages.parsed);

        // Expand macros
        stages.expanded = macroexp::expandMacros(stages.withImports);

        // Generate ANF - store the result directly
        stages.anf = ir::ANFtransform(stages.expanded);

        // Optimize and get dependency graphs
        auto [optimizedAnf, preGraph, postGraph] = optimise::optimise(stages.anf);
        stages.optimizedAnf = optimizedAnf;
        stages.preDependencyGraph = preGraph;
        stages.postDependencyGraph = postGraph;

        return stages;
    }

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

    emscripten::val evaluate(const std::string& input)
    {
        try {
            auto oldBuf = redirectStdout();

            auto stages = compileStages(input);
            std::string result;

            // Evaluate using expanded form
            for (const auto& expr : stages.expanded) {
                std::vector<std::shared_ptr<Expression>> single_expr = { expr };
                auto val = interpret::interpret(state, single_expr);
                if (val) {
                    result = val->toString();
                }
            }

            std::string stdoutCapture = outputCapture.str();
            restoreStdout(oldBuf);

            emscripten::val retVal = emscripten::val::object();
            retVal.set("result", result);
            retVal.set("stdout", stdoutCapture);
            return retVal;
        } catch (const std::exception& e) {
            emscripten::val retVal = emscripten::val::object();
            retVal.set("error", std::string("Error: ") + e.what());
            return retVal;
        }
    }

    emscripten::val getAllStages(const std::string& input)
    {
        try {
            auto stages = compileStages(input);

            // Convert each stage to string representation
            resetStream();
            std::string astStr;
            for (const auto& expr : stages.parsed) {
                ss << expr->ASTToString() << "\n";
            }
            astStr = ss.str();

            resetStream();
            std::string macroStr;
            for (const auto& expr : stages.expanded) {
                ss << expr->toString() << "\n";
            }
            macroStr = ss.str();

            resetStream();
            std::string anfStr;
            for (const auto& expr : stages.anf) {
                ss << expr->toString() << "\n";
            }
            anfStr = ss.str();

            resetStream();
            std::string optAnfStr;
            for (const auto& expr : stages.optimizedAnf) {
                ss << expr->toString() << "\n";
            }
            optAnfStr = ss.str();

            // Generate Three-AC from optimized ANF
            auto threeAC = tac::anfToTac(stages.optimizedAnf);
            std::string threeACStr = threeAC.toString();

            // Return all stages at once
            emscripten::val retVal = emscripten::val::object();
            retVal.set("ast", astStr);
            retVal.set("macroExpanded", macroStr);
            retVal.set("anf", anfStr);
            retVal.set("optimizedANF", optAnfStr);
            retVal.set("threeAC", threeACStr);
            retVal.set("preDependencyGraph", stages.preDependencyGraph);
            retVal.set("postDependencyGraph", stages.postDependencyGraph);
            return retVal;
        } catch (const std::exception& e) {
            emscripten::val retVal = emscripten::val::object();
            retVal.set("error", std::string("Error: ") + e.what());
            return retVal;
        }
    }
};

EMSCRIPTEN_BINDINGS(jaws_module)
{
    emscripten::class_<JawsWrapper>("JawsWrapper")
        .constructor<>()
        .function("evaluate", &JawsWrapper::evaluate)
        .function("getAllStages", &JawsWrapper::getAllStages);
}
