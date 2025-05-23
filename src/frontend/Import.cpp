#include "Import.h"
#include "Expression.h"
#include "Syntax.h"
#include "parse.h"
#include "scan.h"

#include <MacroTraits.h>
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#ifdef WASM_BUILD
#include <emscripten/fetch.h>
#include <emscripten/val.h>
#endif

namespace import {

void populateInterpreterStateFromRegistry(
    const LibraryRegistry& registry,
    interpret::InterpreterState& state,
    std::shared_ptr<pattern::MacroEnvironment> macroEnv)
{
    for (const auto& [libNameStr, libData] : registry) {
        for (const auto& definitionExpr : libData.bodyExpressions) {
            if (!definitionExpr)
                continue;

            if (isDefinition(definitionExpr) && !std::holds_alternative<DefineSyntaxExpression>(definitionExpr->as)) {
                try {
                    std::vector<std::shared_ptr<Expression>> toExpand = { definitionExpr };
                    // Expand macros within the definition itself if needed
                    auto expanded = macroexp::expandMacros(toExpand, macroEnv);

                    if (!expanded.empty() && expanded[0]) {
                        interpret::interpret(state, expanded[0]);
                    }
                } catch (const std::exception& e) {
                    std::string name = "[unknown]";
                    if (auto* d = std::get_if<DefineExpression>(&definitionExpr->as))
                        name = d->name.token.lexeme;
                    else if (auto* dp = std::get_if<DefineProcedure>(&definitionExpr->as))
                        name = dp->name.token.lexeme;
                    std::cerr << "[Warning] Error processing definition for '" << name
                              << "' from library '" << libNameStr << "': " << e.what() << std::endl;
                }
            }
        }
    }
}

void populateMacroEnvironmentFromRegistry(const LibraryRegistry& registry, pattern::MacroEnvironment& macroEnv)
{
    for (const auto& [libNameStr, libData] : registry) {
        for (const auto& bodyExpr : libData.bodyExpressions) { // Iterate body in order
            if (!bodyExpr)
                continue;

            // Find syntax definitions
            if (auto* ds = std::get_if<DefineSyntaxExpression>(&bodyExpr->as)) {
                // Check if this syntax is exported by the library
                if (libData.exportNamesSet.count(ds->name.token.lexeme)) {
                    if (ds->rule && std::holds_alternative<SyntaxRulesExpression>(ds->rule->as)) {
                        // Define macro using the rule expression pointer
                        macroEnv.defineMacro(ds->name.token.lexeme, ds->rule);
                    } else {
                        std::cerr << "[Warning] Exported syntax '" << ds->name.token.lexeme << "' from library '" << libNameStr << "' does not contain valid SyntaxRulesExpression." << std::endl;
                    }
                }
            }
        }
    }
}
#ifdef WASM_BUILD
bool fileExists(const std::string& path)
{
    emscripten::val xhr = emscripten::val::global("XMLHttpRequest").new_();
    xhr.call<void>("open", std::string("HEAD"), path, false);
    xhr.call<void>("send");
    return xhr["status"].as<int>() == 200;
}
std::string readFile(const std::string& path)
{
    emscripten::val xhr = emscripten::val::global("XMLHttpRequest").new_();
    xhr.call<void>("open", std::string("GET"), path, false);
    xhr.call<void>("send");
    if (xhr["status"].as<int>() != 200) {
        throw std::runtime_error("Failed to load file (status " + xhr["status"].as<std::string>() + "): " + path);
    }
    return xhr["responseText"].as<std::string>();
}
#else
bool fileExists(const std::string& path)
{
    std::error_code ec;
    return std::filesystem::exists(path, ec);
}
std::string readFile(const std::string& path)
{
    std::ifstream file(path);
    if (!file) {
        throw std::runtime_error("Unable to open file: " + path);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}
#endif

bool isDefinition(const std::shared_ptr<Expression>& expr)
{
    return std::holds_alternative<DefineExpression>(expr->as) || std::holds_alternative<DefineSyntaxExpression>(expr->as) || std::holds_alternative<DefineProcedure>(expr->as);
}

std::string libraryNameToStringPath(const std::vector<std::shared_ptr<Expression>>& nameParts)
{
    std::filesystem::path libPath;
    for (const auto& partExpr : nameParts) {
        if (auto* atom = std::get_if<AtomExpression>(&partExpr->as)) {
            if (atom->value.token.type == Tokentype::IDENTIFIER) {
                libPath /= atom->value.token.lexeme;
            } else if (atom->value.token.type == Tokentype::INTEGER) {
                try {
                    if (std::stoi(atom->value.token.lexeme) < 0) {
                        throw std::runtime_error("Negative version");
                    }
                    libPath /= atom->value.token.lexeme;
                } catch (...) {
                    throw std::runtime_error("Invalid integer in library name: " + atom->value.token.lexeme);
                }
            } else {
                throw std::runtime_error("Invalid token type");
            }
        } else {
            throw std::runtime_error("Library name component must be an atom");
        }
    }
    return libPath.string();
}

std::string resolveLibraryPath(const std::vector<std::shared_ptr<Expression>>& nameParts)
{
    std::string namePathStr = libraryNameToStringPath(nameParts);
#ifdef WASM_BUILD
    std::string webLibDir = "/lib/";
    std::string resolvedPath = webLibDir + namePathStr + ".scm";
    return resolvedPath;
#else
    std::filesystem::path baseLibPath = "../lib";
    std::filesystem::path resolvedPath = baseLibPath / (namePathStr + ".scm");
    if (fileExists(resolvedPath.string()))
        return resolvedPath.string();
    std::filesystem::path relativePath = namePathStr + ".scm";
    if (fileExists(relativePath.string()))
        return relativePath.string();
    throw std::runtime_error("Cannot resolve library path (Native): " + namePathStr);
#endif
}

std::vector<std::shared_ptr<Expression>> deriveLibraryNameFromPath(
    const std::filesystem::path& filePath,
    const std::filesystem::path& basePath)
{
    std::vector<std::shared_ptr<Expression>> nameParts;
    try {
        auto relativePath = std::filesystem::relative(filePath, basePath);
        relativePath.replace_extension();
        for (const auto& part : relativePath) {
            std::string partStr = part.string();
            if (partStr.empty() || partStr == ".")
                continue;
            bool isInteger = !partStr.empty() && std::all_of(partStr.begin(), partStr.end(), ::isdigit);
            Token token;
            token.type = isInteger ? Tokentype::INTEGER : Tokentype::IDENTIFIER;
            token.lexeme = partStr;
            token.line = 1;
            token.column = 1;
            // Assuming makeAtom exists or using constructor directly
            // nameParts.push_back(makeAtom(partStr, token.type));
            nameParts.push_back(std::make_shared<Expression>(AtomExpression { HygienicSyntax { token, {} } }, 1));
        }
    } catch (const std::exception& e) {
        throw std::runtime_error("Failed to derive library name from path '" + filePath.string() + "': " + e.what());
    }
    if (nameParts.empty()) {
        throw std::runtime_error("Derived library name is empty for path: " + filePath.string());
    }
    return nameParts;
}

LibraryData importLibrary(
    const std::string& path,
    LibraryRegistry& registry,
    std::set<std::string>& visitedPaths)
{
    std::string registryKey;
    try {
#ifdef WASM_BUILD
        registryKey = path;
#else
        registryKey = std::filesystem::absolute(path).string();
#endif
    } catch (const std::exception& e) {
        std::cerr << "[Warning] Could not normalize path for registry key: " << path << std::endl;
        registryKey = path;
    }

    if (registry.count(registryKey)) {
        return registry[registryKey];
    }
    if (visitedPaths.count(path)) {
        throw std::runtime_error("Circular library dependency detected: " + path);
    }
    visitedPaths.insert(path);

    LibraryData libraryData;
    try {
        if (!fileExists(path)) {
            throw std::runtime_error("Library file not found: " + path);
        }
        auto source = readFile(path);
        auto tokens = scanner::tokenize(source);
        auto parsedExprsOpt = parse::parse(std::move(tokens));

        if (!parsedExprsOpt || parsedExprsOpt->empty()) {
            throw std::runtime_error("Library empty or parse failed: " + path);
        }

        auto parsedExprs = *parsedExprsOpt;

        if (auto* libDef = std::get_if<DefineLibraryExpression>(&parsedExprs[0]->as)) {
            libraryData.canonicalName = libDef->libraryName;
            libraryData.bodyExpressions = libDef->body;

            for (const auto& expSym : libDef->exports) {
                libraryData.exportNamesSet.insert(expSym.token.lexeme);
            }

            for (const auto& bodyExpr : libraryData.bodyExpressions) {
                if (!bodyExpr || !isDefinition(bodyExpr))
                    continue;

                std::string defName;
                HygienicSyntax defSyntax;
                ExportedBinding::Type defType = ExportedBinding::Type::UNKNOWN;

                if (auto* d = std::get_if<DefineExpression>(&bodyExpr->as)) {
                    defName = d->name.token.lexeme;
                    defSyntax = d->name;
                    defType = ExportedBinding::Type::VALUE;
                } else if (auto* dp = std::get_if<DefineProcedure>(&bodyExpr->as)) {
                    defName = dp->name.token.lexeme;
                    defSyntax = dp->name;
                    defType = ExportedBinding::Type::VALUE;
                } else if (auto* ds = std::get_if<DefineSyntaxExpression>(&bodyExpr->as)) {
                    defName = ds->name.token.lexeme;
                    defSyntax = ds->name;
                    defType = ExportedBinding::Type::SYNTAX;
                }

                if (!defName.empty() && libraryData.exportNamesSet.count(defName)) {
                    if (libraryData.exportedBindings.count(defName)) {
                        std::cerr << "[Warning] Duplicate export '" << defName << "' detected in library '" << path << "'. Overwriting." << std::endl;
                    }
                    ExportedBinding binding;
                    binding.syntax = defSyntax;
                    binding.definition = bodyExpr; // Point to the expression in bodyExpressions
                    binding.type = defType;
                    libraryData.exportedBindings[defName] = std::move(binding);
                }
            }
        } else {
            throw std::runtime_error("File does not start with (define-library): " + path);
        }
    } catch (...) {
        visitedPaths.erase(path);
        throw;
    }

    visitedPaths.erase(path);
    registry[registryKey] = std::move(libraryData);
    return registry[registryKey];
}

void preloadLibraries(const std::string& basePathStr, LibraryRegistry& registry)
{
#ifdef WASM_BUILD
    std::cout << "[Info] Preloading libraries for WASM from hardcoded list..." << std::endl;

    // Ensure basePathStr starts with a slash and ends with a slash
    std::string normalizedBasePath = basePathStr;
    if (!normalizedBasePath.empty() && normalizedBasePath.front() != '/') {
        normalizedBasePath = "/" + normalizedBasePath;
    }
    if (!normalizedBasePath.empty() && normalizedBasePath.back() != '/') {
        normalizedBasePath += "/";
    }

    std::vector<std::string> wasm_library_paths = {
        "base.scm",
        "list-utils.scm",
        "loops.scm",
        "vector-utils.scm",
        "utilities.scm",
        "math.scm",
    };

    std::set<std::string> visitedPathsForPreload;
    for (const std::string& path : wasm_library_paths) {
        std::string fullPath = normalizedBasePath + path;
        std::string registryKey = fullPath;

        if (registry.count(registryKey)) {
            continue;
        }

        std::cout << "[Info] WASM Preloading library: " << fullPath << std::endl;
        try {
            importLibrary(fullPath, registry, visitedPathsForPreload);
        } catch (const std::exception& e) {
            std::cerr << "[Warning] Failed to preload library '" << fullPath << "': " << e.what() << std::endl;

            // Try alternate path in case the file exists in a different location
            std::string altPath = "/lib/" + path;
            if (altPath != fullPath) {
                std::cout << "[Info] Trying alternate path: " << altPath << std::endl;
                try {
                    importLibrary(altPath, registry, visitedPathsForPreload);
                } catch (const std::exception& e) {
                    std::cerr << "[Warning] Also failed with alternate path '" << altPath << "': " << e.what() << std::endl;
                }
            }
        }
        visitedPathsForPreload.clear();
    }
#else
    std::filesystem::path basePath(basePathStr);
    if (!std::filesystem::exists(basePath) || !std::filesystem::is_directory(basePath)) {
        std::cerr << "[Warning] Library base path not found: " << basePathStr << std::endl;
        return;
    }
    std::set<std::string> visitedPathsForPreload;
    std::error_code ec;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(basePath, ec)) {
        if (ec) {
            std::cerr << "[Warning] Filesystem error iterating " << basePathStr << ": " << ec.message() << std::endl;
            break;
        }
        if (entry.is_regular_file(ec) && entry.path().extension() == ".scm") {
            if (ec) {
                std::cerr << "[Warning] Filesystem error checking file type for " << entry.path().string() << ": " << ec.message() << std::endl;
                continue;
            }
            std::string currentPath = entry.path().string();
            std::string registryKey;
            try {
                registryKey = std::filesystem::absolute(entry.path()).string();
            } catch (const std::exception& e) {
                std::cerr << "[Warning] Could not get absolute path for '" << currentPath << "': " << e.what() << std::endl;
                continue;
            }
            if (registry.count(registryKey)) {
                continue;
            }
            std::cout << "[Info] Preloading library: " << currentPath << std::endl;
            try {
                importLibrary(currentPath, registry, visitedPathsForPreload);
            } catch (const std::exception& e) {
                std::cerr << "[Warning] Failed to preload library '" << currentPath << "': " << e.what() << std::endl;
            }
            visitedPathsForPreload.clear();
        }
    }
#endif
}

ProcessedCode processImports(
    const std::vector<std::shared_ptr<Expression>>& expressions,
    LibraryRegistry& registry)
{
    ProcessedCode result;
    std::set<std::string> visitedPaths;

    for (const auto& expr : expressions) {
        if (auto* import = std::get_if<ImportExpression>(&expr->as)) {
            for (const auto& spec : import->imports) {
                if (spec.type == ImportExpression::ImportSet::Type::DIRECT) {
                    try {
                        std::string path = resolveLibraryPath(spec.library);
                        LibraryData data = importLibrary(path, registry, visitedPaths);
                        if (!data.exportedBindings.empty() || !data.canonicalName.empty()) {
                            result.importedLibrariesData.push_back(std::move(data));
                        }
                    } catch (const std::exception& e) {
                        throw std::runtime_error("Failed to import library '" + libraryNameToStringPath(spec.library) + "': " + e.what());
                    }
                } else {
                    throw std::runtime_error("Unsupported R6RS-style import set found.");
                }
            }
        } else if (std::holds_alternative<DefineLibraryExpression>(expr->as)) {
            throw std::runtime_error("define-library found outside library file context.");
        } else {
            result.remainingExpressions.push_back(expr);
        }
    }
    return result;
}

} // namespace import
