#include "Import.h"
#include "parse.h"
#include "scan.h"

#include <filesystem>
#include <fstream>
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
        throw ParseError("Failed to load file: " + path, Token(), "");
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

// --- Helper Functions Implementation ---

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
    if (fileExists(resolvedPath))
        return resolvedPath;
    resolvedPath = namePathStr + ".scm";
    if (fileExists(resolvedPath))
        return resolvedPath;
    throw std::runtime_error("Cannot resolve library path (WASM): " + namePathStr);
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

// --- Main Function Implementations ---

LibraryData importLibrary(
    const std::string& path,
    std::set<std::string>& visitedPaths)
{
    if (visitedPaths.count(path)) {
        throw std::runtime_error("Circular library dependency detected: " + path);
    }
    visitedPaths.insert(path);

    LibraryData libraryData;

    if (!fileExists(path)) {
        visitedPaths.erase(path);
        throw std::runtime_error("Library file not found: " + path);
    }
    auto source = readFile(path);
    auto tokens = scanner::tokenize(source);
    auto parsedExprsOpt = parse::parse(std::move(tokens));

    if (!parsedExprsOpt || parsedExprsOpt->empty()) {
        visitedPaths.erase(path);
        return libraryData;
    }
    auto parsedExprs = *parsedExprsOpt;

    if (auto* libDef = std::get_if<DefineLibraryExpression>(&parsedExprs[0]->as)) {
        libraryData.canonicalName = libDef->libraryName;

        std::set<std::string> exportNames;
        for (const auto& expSym : libDef->exports) {
            exportNames.insert(expSym.token.lexeme);
        }

        for (const auto& bodyExpr : libDef->body) {
            if (!isDefinition(bodyExpr))
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

            if (!defName.empty() && exportNames.count(defName)) {
                if (libraryData.exportedBindings.count(defName)) {
                    std::cerr << "[Warning] Duplicate export '" << defName << "' in library '" << path << "'." << std::endl;
                }
                ExportedBinding binding;
                binding.syntax = defSyntax;
                binding.definition = bodyExpr;
                binding.type = defType;
                libraryData.exportedBindings[defName] = std::move(binding);
            }
        }

    } else {
        visitedPaths.erase(path);
        throw std::runtime_error("File '" + path + "' is not a valid R7RS library (missing define-library).");
    }

    visitedPaths.erase(path);
    return libraryData;
}

ProcessedCode processImports(
    const std::vector<std::shared_ptr<Expression>>& expressions)
{
    ProcessedCode result;
    std::set<std::string> visitedPaths;

    for (const auto& expr : expressions) {
        if (auto* import = std::get_if<ImportExpression>(&expr->as)) {
            for (const auto& spec : import->imports) {
                if (spec.type == ImportExpression::ImportSet::Type::DIRECT) {
                    try {
                        std::string path = resolveLibraryPath(spec.library);
                        LibraryData data = importLibrary(path, visitedPaths);
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
