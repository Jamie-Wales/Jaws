#include "Import.h"
#include "Error.h"
#include "parse.h"
#include "run.h"
#include "scan.h"
#include <fstream>
#include <set>

namespace import {

std::shared_ptr<Expression> renameDefinition(
    const std::shared_ptr<Expression>& expr,
    const std::string& oldName,
    const std::string& newName)
{
    Token newToken(Tokentype::IDENTIFIER, newName, expr->line, expr->line);

    if (auto* define = std::get_if<DefineExpression>(&expr->as)) {
        return std::make_shared<Expression>(
            Expression { DefineExpression { newToken, define->value }, expr->line });
    } else if (auto* defineProc = std::get_if<DefineProcedure>(&expr->as)) {
        return std::make_shared<Expression>(
            Expression { DefineProcedure { newToken, defineProc->parameters, defineProc->body },
                expr->line });
    } else if (auto* defineSyntax = std::get_if<DefineSyntaxExpression>(&expr->as)) {
        return std::make_shared<Expression>(
            Expression { DefineSyntaxExpression { newToken, defineSyntax->rule }, expr->line });
    }

    return expr;
}

bool fileExists(const std::string& path)
{
    std::ifstream f(path.c_str());
    return f.good();
}

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

std::string resolveLibraryPath(const ImportExpression::ImportSpec& spec)
{
    if (spec.library.size() == 1) {
        if (auto* atom = std::get_if<AtomExpression>(&spec.library[0]->as)) {
            std::string localPath = atom->value.lexeme + ".scm";
            if (fileExists(localPath)) {
                return localPath;
            }
            return std::format("../lib/{}", localPath);
        }
    }

    std::string libPath;
    for (const auto& part : spec.library) {
        if (auto* atom = std::get_if<AtomExpression>(&part->as)) {
            libPath += atom->value.lexeme + "/";
        }
    }
    return std::format("../lib/{}.scm", libPath.substr(0, libPath.length() - 1));
}

std::shared_ptr<Expression> transformExpression(
    const std::shared_ptr<Expression>& expr,
    const ImportExpression::ImportSpec& spec)
{
    std::optional<std::string> defName;
    if (auto* define = std::get_if<DefineExpression>(&expr->as)) {
        defName = define->name.lexeme;
    } else if (auto* defineProc = std::get_if<DefineProcedure>(&expr->as)) {
        defName = defineProc->name.lexeme;
    } else if (auto* defineSyntax = std::get_if<DefineSyntaxExpression>(&expr->as)) {
        defName = defineSyntax->name.lexeme;
    }

    if (!defName) {
        return expr;
    }
    switch (spec.type) {
    case ImportExpression::ImportSet::Type::DIRECT:
        return expr;

    case ImportExpression::ImportSet::Type::ONLY:
        for (const auto& id : spec.identifiers) {
            if (id.lexeme == *defName) {
                return expr;
            }
        }
        return nullptr;

    case ImportExpression::ImportSet::Type::EXCEPT:
        for (const auto& id : spec.identifiers) {
            if (id.lexeme == *defName) {
                return nullptr;
            }
        }
        return expr;

    case ImportExpression::ImportSet::Type::PREFIX:
        return renameDefinition(expr, *defName, spec.prefix.lexeme + *defName);

    case ImportExpression::ImportSet::Type::RENAME:
        for (const auto& [oldName, newName] : spec.renames) {
            if (oldName.lexeme == *defName) {
                return renameDefinition(expr, *defName, newName.lexeme);
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
