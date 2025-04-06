#include "Import.h"
#include "Error.h"
#include "parse.h"
#include "scan.h"
#include <fstream>
#include <set>

#ifdef WASM_BUILD
#include <emscripten/fetch.h>
#include <emscripten/val.h>
#endif

namespace import {

std::shared_ptr<Expression> renameDefinition(
    const std::shared_ptr<Expression>& expr,
    const std::string& oldName,
    const std::string& newName)
{
    Token newToken(Tokentype::IDENTIFIER, newName, expr->line, expr->line);
    // Create a HygienicSyntax with the new token and empty context
    HygienicSyntax newSyntax { newToken, SyntaxContext {} };

    if (auto* define = std::get_if<DefineExpression>(&expr->as)) {
        return std::make_shared<Expression>(
            Expression { DefineExpression { newSyntax, define->value }, expr->line });
    } else if (auto* defineProc = std::get_if<DefineProcedure>(&expr->as)) {
        return std::make_shared<Expression>(
            Expression { DefineProcedure { newSyntax, defineProc->parameters, defineProc->body, defineProc->isVariadic },
                expr->line });
    } else if (auto* defineSyntax = std::get_if<DefineSyntaxExpression>(&expr->as)) {
        return std::make_shared<Expression>(
            Expression { DefineSyntaxExpression { newSyntax, defineSyntax->rule }, expr->line });
    }

    return expr;
}

#ifdef WASM_BUILD
// For WebAssembly, use emscripten's synchronous XHR to check if a file exists
bool fileExists(const std::string& path)
{
    emscripten::val xhr = emscripten::val::global("XMLHttpRequest").new_();
    xhr.call<void>("open", std::string("HEAD"), path, false);
    xhr.call<void>("send");

    return xhr["status"].as<int>() == 200;
}

// Read file content for WebAssembly using synchronous XHR
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
// Native filesystem operations for non-WASM builds
bool fileExists(const std::string& path)
{
    std::ifstream f(path.c_str());
    return f.good();
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

void importLibrary(
    const std::string& path,
    const ImportExpression::ImportSpec& spec,
    std::vector<std::shared_ptr<Expression>>& expressions)
{
    if (!fileExists(path)) {
        throw ParseError("Cannot find library file: " + path, Token(), "");
    }

    auto source = readFile(path);
    auto tokens = scanner::tokenize(source);
    auto libExprs = parse::parse(std::move(tokens));

    if (!libExprs) {
        throw ParseError("Failed to parse library: " + path, Token(), "");
    }

    auto libraryExprs = processImports(*libExprs);

    for (const auto& expr : libraryExprs) {
        auto transformed = transformExpression(expr, spec);
        if (transformed) {
            expressions.push_back(transformed);
        }
    }
}

#ifdef WASM_BUILD
std::string resolveLibraryPath(const ImportExpression::ImportSpec& spec)
{
    if (spec.library.size() == 1) {
        if (auto* atom = std::get_if<AtomExpression>(&spec.library[0]->as)) {
            std::string fileName = atom->value.token.lexeme + ".scm";

            // First try in /lib directory (preferred location for web)
            std::string webPath = "/lib/" + fileName;
            if (fileExists(webPath)) {
                return webPath;
            }

            // Then try in current directory
            if (fileExists(fileName)) {
                return fileName;
            }

            // Fallback to the original paths for backward compatibility
            std::string localPath = atom->value.token.lexeme + ".scm";
            std::string libPath = "../lib/" + localPath;

            if (fileExists(libPath)) {
                return libPath;
            }

            // For web, also try the public/lib path
            return "/lib/" + fileName;
        }
    }

    std::string libPath;
    for (const auto& part : spec.library) {
        if (auto* atom = std::get_if<AtomExpression>(&part->as)) {
            libPath += atom->value.token.lexeme + "/";
        }
    }

    // First try the web-specific path
    std::string webPath = "/lib/" + libPath.substr(0, libPath.length() - 1) + ".scm";
    if (fileExists(webPath)) {
        return webPath;
    }

    // Fallback to original path format
    return "../lib/" + libPath.substr(0, libPath.length() - 1) + ".scm";
}
#else
std::string resolveLibraryPath(const ImportExpression::ImportSpec& spec)
{
    if (spec.library.size() == 1) {
        if (auto* atom = std::get_if<AtomExpression>(&spec.library[0]->as)) {
            std::string localPath = atom->value.token.lexeme + ".scm";
            if (fileExists(localPath)) {
                return localPath;
            }
            return std::format("../lib/{}", localPath);
        }
    }

    std::string libPath;
    for (const auto& part : spec.library) {
        if (auto* atom = std::get_if<AtomExpression>(&part->as)) {
            libPath += atom->value.token.lexeme + "/";
        }
    }
    return std::format("../lib/{}.scm", libPath.substr(0, libPath.length() - 1));
}
#endif

std::shared_ptr<Expression> transformExpression(
    const std::shared_ptr<Expression>& expr,
    const ImportExpression::ImportSpec& spec)
{
    std::optional<std::string> defName;
    if (auto* define = std::get_if<DefineExpression>(&expr->as)) {
        defName = define->name.token.lexeme;
    } else if (auto* defineProc = std::get_if<DefineProcedure>(&expr->as)) {
        defName = defineProc->name.token.lexeme;
    } else if (auto* defineSyntax = std::get_if<DefineSyntaxExpression>(&expr->as)) {
        defName = defineSyntax->name.token.lexeme;
    }

    if (!defName) {
        return expr;
    }
    switch (spec.type) {
    case ImportExpression::ImportSet::Type::DIRECT:
        return expr;

    case ImportExpression::ImportSet::Type::ONLY:
        for (const auto& id : spec.identifiers) {
            if (id.token.lexeme == *defName) {
                return expr;
            }
        }
        return nullptr;

    case ImportExpression::ImportSet::Type::EXCEPT:
        for (const auto& id : spec.identifiers) {
            if (id.token.lexeme == *defName) {
                return nullptr;
            }
        }
        return expr;

    case ImportExpression::ImportSet::Type::PREFIX:
        return renameDefinition(expr, *defName, spec.prefix.token.lexeme + *defName);

    case ImportExpression::ImportSet::Type::RENAME:
        for (const auto& [oldName, newName] : spec.renames) {
            if (oldName.token.lexeme == *defName) {
                return renameDefinition(expr, *defName, newName.token.lexeme);
            }
        }
        return expr;
    }

    return expr;
}

std::vector<std::shared_ptr<Expression>> processImport(const ImportExpression& import)
{
    std::vector<std::shared_ptr<Expression>> importedExprs;

    for (const auto& spec : import.imports) {
        auto path = resolveLibraryPath(spec);
        importLibrary(path, spec, importedExprs);
    }

    return importedExprs;
}

std::vector<std::shared_ptr<Expression>> processImports(const std::vector<std::shared_ptr<Expression>>& expressions)
{
    std::vector<std::shared_ptr<Expression>> defines;
    std::vector<std::shared_ptr<Expression>> others;

    for (const auto& expr : expressions) {
        if (auto* import = std::get_if<ImportExpression>(&expr->as)) {
            auto imported = processImport(*import);
            for (const auto& impExpr : imported) {
                if (isDefinition(impExpr)) {
                    defines.push_back(impExpr);
                } else {
                    others.push_back(impExpr);
                }
            }
        }
    }

    for (const auto& expr : expressions) {
        if (!std::holds_alternative<ImportExpression>(expr->as)) {
            if (isDefinition(expr)) {
                defines.push_back(expr);
            } else {
                others.push_back(expr);
            }
        }
    }

    std::vector<std::shared_ptr<Expression>> result;
    result.reserve(defines.size() + others.size());
    result.insert(result.end(), defines.begin(), defines.end());
    result.insert(result.end(), others.begin(), others.end());

    return result;
}
}
