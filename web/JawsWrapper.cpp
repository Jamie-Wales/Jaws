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
    std::shared_ptr<pattern::MacroEnvironment> macroEnv;
    import::LibraryRegistry registry;
    std::stringstream ss;
    std::stringstream outputCapture;
    bool librariesPreloaded = false; // Track whether libraries have been preloaded

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

        // Process imports using registry
        import::ProcessedCode processedCode = import::processImports(stages.parsed, registry);
        stages.withImports = processedCode.remainingExpressions;

        // Register syntax definitions in macro environment
        for (const auto& expr : stages.withImports) {
            if (const auto* de = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                if (de->rule && std::holds_alternative<SyntaxRulesExpression>(de->rule->as)) {
                    macroEnv->defineMacro(de->name.token.lexeme, de->rule);
                }
            }
        }

        // Collect non-syntax expressions for expansion
        std::vector<std::shared_ptr<Expression>> expressionsToExpand;
        for (const auto& expr : stages.withImports) {
            if (expr && !std::holds_alternative<DefineSyntaxExpression>(expr->as)) {
                expressionsToExpand.push_back(expr);
            }
        }

        // Expand macros with macro environment
        stages.expanded = macroexp::expandMacros(expressionsToExpand, macroEnv);

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

    void ensureLibrariesPreloaded()
    {
        if (!librariesPreloaded) {
            try {
                std::cout << "[Info] Preloading libraries into WASM environment..." << std::endl;
                import::preloadLibraries("/lib", registry);
                import::populateMacroEnvironmentFromRegistry(registry, *macroEnv);
                import::populateInterpreterStateFromRegistry(registry, state, macroEnv);
                std::cout << "[Info] Preloaded libraries populated into WASM environment." << std::endl;
                librariesPreloaded = true;
            } catch (const std::exception& e) {
                std::cerr << "[Warning] Error setting up WASM environment: " << e.what() << std::endl;
            }
        }
    }

    // Helper to prepare interpreter environment with expanded imported values
    void prepareInterpreterEnvironment(const import::ProcessedCode& code)
    {
        for (const auto& libData : code.importedLibrariesData) {
            for (const auto& [name, binding] : libData.exportedBindings) {
                if (binding.type == import::ExportedBinding::Type::VALUE && binding.definition) {
                    try {
                        // Expand macros in the binding definition
                        std::vector<std::shared_ptr<Expression>> toExpand = { binding.definition };
                        auto expanded = macroexp::expandMacros(toExpand, macroEnv);

                        // Interpret the expanded definition
                        if (!expanded.empty() && expanded[0]) {
                            interpret::interpret(state, expanded[0]);
                        }
                    } catch (const std::exception& e) {
                        std::cerr << "[Warning] Error processing expanded binding '" << name << "': " << e.what() << std::endl;
                    }
                }
            }
        }
    }

public:
    JawsWrapper()
        : state(interpret::createInterpreter())
        , macroEnv(std::make_shared<pattern::MacroEnvironment>())
    {
        ensureLibrariesPreloaded();
    }

    emscripten::val evaluate(const std::string& input)
    {
        try {
            // Ensure libraries are loaded (only happens once)
            ensureLibrariesPreloaded();

            auto oldBuf = redirectStdout();

            // Parse code
            auto tokens = scanner::tokenize(input);
            auto expressionsOpt = parse::parse(std::move(tokens));
            if (!expressionsOpt) {
                restoreStdout(oldBuf);
                emscripten::val retVal = emscripten::val::object();
                retVal.set("error", "Parsing failed");
                return retVal;
            }
            auto parsedExpressions = *expressionsOpt;

            // Process imports
            import::ProcessedCode processedCode;
            try {
                processedCode = import::processImports(parsedExpressions, registry);
            } catch (const std::exception& e) {
                restoreStdout(oldBuf);
                emscripten::val retVal = emscripten::val::object();
                retVal.set("error", std::string("[Import Error] ") + e.what());
                return retVal;
            }

            // First register all syntax definitions from user code in the macro environment
            for (const auto& expr : processedCode.remainingExpressions) {
                if (const auto* de = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                    if (de->rule && std::holds_alternative<SyntaxRulesExpression>(de->rule->as)) {
                        macroEnv->defineMacro(de->name.token.lexeme, de->rule);
                    }
                }
            }

            // Prepare interpreter environment with expanded imported values
            prepareInterpreterEnvironment(processedCode);

            // Collect non-syntax expressions for expansion
            std::vector<std::shared_ptr<Expression>> expressionsToExpand;
            for (const auto& expr : processedCode.remainingExpressions) {
                if (expr && !std::holds_alternative<DefineSyntaxExpression>(expr->as)) {
                    expressionsToExpand.push_back(expr);
                }
            }

            // Expand macros in user code
            auto finalExpressions = macroexp::expandMacros(expressionsToExpand, macroEnv);

            // Evaluate using expanded form
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
            // Ensure libraries are loaded (only happens once)
            ensureLibrariesPreloaded();

            auto tokens = scanner::tokenize(input);
            auto expressionsOpt = parse::parse(std::move(tokens));
            if (!expressionsOpt) {
                emscripten::val retVal = emscripten::val::object();
                retVal.set("error", "Parsing failed");
                return retVal;
            }
            auto parsedExpressions = *expressionsOpt;

            // Convert AST to string
            resetStream();
            std::string astStr;
            for (const auto& expr : parsedExpressions) {
                ss << expr->ASTToString() << "\n";
            }
            astStr = ss.str();

            // Process imports
            import::ProcessedCode processedCode;
            try {
                processedCode = import::processImports(parsedExpressions, registry);
            } catch (const std::exception& e) {
                emscripten::val retVal = emscripten::val::object();
                retVal.set("error", std::string("[Import Error] ") + e.what());
                return retVal;
            }

            // First register all syntax definitions in the macro environment
            for (const auto& expr : processedCode.remainingExpressions) {
                if (const auto* de = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                    if (de->rule && std::holds_alternative<SyntaxRulesExpression>(de->rule->as)) {
                        macroEnv->defineMacro(de->name.token.lexeme, de->rule);
                    }
                }
            }

            // Collect non-syntax expressions for expansion
            std::vector<std::shared_ptr<Expression>> expressionsToExpand;
            for (const auto& expr : processedCode.remainingExpressions) {
                if (expr && !std::holds_alternative<DefineSyntaxExpression>(expr->as)) {
                    expressionsToExpand.push_back(expr);
                }
            }

            // Expand macros in user code
            auto expandedExpressions = macroexp::expandMacros(expressionsToExpand, macroEnv);

            // Convert expanded code to string
            resetStream();
            std::string macroStr;
            for (const auto& expr : expandedExpressions) {
                ss << expr->toString() << "\n";
            }
            macroStr = ss.str();

            // Generate ANF
            auto anf = ir::ANFtransform(expandedExpressions);

            // Convert ANF to string
            resetStream();
            std::string anfStr;
            for (const auto& expr : anf) {
                ss << expr->toString() << "\n";
            }
            anfStr = ss.str();

            // Optimize and get dependency graphs
            auto [optimizedAnf, preGraph, postGraph] = optimise::optimise(anf);

            // Convert optimized ANF to string
            resetStream();
            std::string optAnfStr;
            for (const auto& expr : optimizedAnf) {
                ss << expr->toString() << "\n";
            }
            optAnfStr = ss.str();

            // Generate Three-AC from optimized ANF
            auto threeAC = tac::anfToTac(optimizedAnf);
            std::string threeACStr = threeAC.toString();

            // Return all stages at once
            emscripten::val retVal = emscripten::val::object();
            retVal.set("ast", astStr);
            retVal.set("macroExpanded", macroStr);
            retVal.set("anf", anfStr);
            retVal.set("optimizedANF", optAnfStr);
            retVal.set("threeAC", threeACStr);
            retVal.set("preDependencyGraph", preGraph);
            retVal.set("postDependencyGraph", postGraph);
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
