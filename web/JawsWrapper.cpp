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
    std::shared_ptr<pattern::MacroEnvironment> mainMacroEnv;
    import::LibraryRegistry registry;
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

        // Process imports - Now using registry
        import::ProcessedCode processedCode = import::processImports(stages.parsed, registry);
        stages.withImports = processedCode.remainingExpressions;

        // Extract macros before macro expansion
        std::vector<std::shared_ptr<Expression>> expressionsToExpand;
        for (const auto& expr : stages.withImports) {
            if (const auto* de = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                if (de->rule && std::holds_alternative<SyntaxRulesExpression>(de->rule->as)) {
                    mainMacroEnv->defineMacro(de->name.token.lexeme, de->rule);
                }
            } else {
                expressionsToExpand.push_back(expr);
            }
        }

        // Expand macros with macro environment
        stages.expanded = macroexp::expandMacros(expressionsToExpand, mainMacroEnv);

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
        , mainMacroEnv(std::make_shared<pattern::MacroEnvironment>())
    {
        // Initialize library registry similar to run.cpp
        try {
            import::preloadLibraries("/lib", registry);
            import::populateInterpreterStateFromRegistry(registry, state);
            import::populateMacroEnvironmentFromRegistry(registry, *mainMacroEnv);
            std::cout << "[Info] Preloaded libraries populated into WASM environment." << std::endl;
        } catch (const std::exception& e) {
            std::cerr << "[Warning] Error setting up WASM environment: " << e.what() << std::endl;
        }
    }

    emscripten::val evaluate(const std::string& input)
    {
        try {
            auto oldBuf = redirectStdout();

            // Parse code
            auto tokens = scanner::tokenize(input);
            auto expressionsOpt = parse::parse(std::move(tokens));
            if (!expressionsOpt) {
                throw std::runtime_error("Parsing failed");
            }
            auto parsedExpressions = *expressionsOpt;

            // Process imports
            import::ProcessedCode processedCode;
            try {
                processedCode = import::processImports(parsedExpressions, registry);
            } catch (const std::exception& e) {
                throw std::runtime_error(std::string("[Import Error] ") + e.what());
            }

            // Handle macro definitions
            std::vector<std::shared_ptr<Expression>> expressionsToExpand;
            for (const auto& expr : processedCode.remainingExpressions) {
                if (const auto* de = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                    if (de->rule && std::holds_alternative<SyntaxRulesExpression>(de->rule->as)) {
                        mainMacroEnv->defineMacro(de->name.token.lexeme, de->rule);
                    }
                } else {
                    expressionsToExpand.push_back(expr);
                }
            }

            // Prepare interpreter environment with imported values
            prepareInterpreterEnvironment(processedCode, state);

            // Expand macros
            auto finalExpressions = macroexp::expandMacros(expressionsToExpand, mainMacroEnv);

            // Evaluate
            std::string result;
            auto val = interpret::interpret(state, finalExpressions);
            if (val) {
                result = val->toString();
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
