#pragma once
#include "Expression.h"
#include "Syntax.h"
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace import {

// --- Data Structures ---

struct ExportedBinding {
    HygienicSyntax syntax;
    std::shared_ptr<Expression> definition;
    enum class Type { VALUE,
        SYNTAX,
        UNKNOWN } type
        = Type::UNKNOWN;
};

struct LibraryData {
    std::vector<std::shared_ptr<Expression>> canonicalName;
    std::map<std::string, ExportedBinding> exportedBindings;
};

struct ProcessedCode {
    std::vector<std::shared_ptr<Expression>> remainingExpressions;
    std::vector<LibraryData> importedLibrariesData;
};

bool fileExists(const std::string& path);
std::string readFile(const std::string& path);
bool isDefinition(const std::shared_ptr<Expression>& expr);
std::string libraryNameToStringPath(const std::vector<std::shared_ptr<Expression>>& nameParts);
std::string resolveLibraryPath(const std::vector<std::shared_ptr<Expression>>& nameParts);

LibraryData importLibrary(
    const std::string& path,
    std::set<std::string>& visitedPaths);

ProcessedCode processImports(
    const std::vector<std::shared_ptr<Expression>>& expressions);

}
