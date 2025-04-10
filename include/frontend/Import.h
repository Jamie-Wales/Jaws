#ifndef IMPORT_H
#define IMPORT_H

#include "Expression.h"
#include "MacroEnvironment.h"
#include "Syntax.h"
#include "interpret.h"
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace import {

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

using LibraryRegistry = std::map<std::string, LibraryData>;

bool fileExists(const std::string& path);
std::string readFile(const std::string& path);
bool isDefinition(const std::shared_ptr<Expression>& expr);
std::string libraryNameToStringPath(const std::vector<std::shared_ptr<Expression>>& nameParts);
std::string resolveLibraryPath(const std::vector<std::shared_ptr<Expression>>& nameParts);
std::vector<std::shared_ptr<Expression>> deriveLibraryNameFromPath(
    const std::filesystem::path& filePath,
    const std::filesystem::path& basePath);

LibraryData importLibrary(
    const std::string& path,
    LibraryRegistry& registry,
    std::set<std::string>& visitedPaths);

void preloadLibraries(const std::string& basePath, LibraryRegistry& registry);

void populateInterpreterStateFromRegistry(
    const LibraryRegistry& registry,
    interpret::InterpreterState& state,
    std::shared_ptr<pattern::MacroEnvironment> macroEnv);
void populateMacroEnvironmentFromRegistry(const LibraryRegistry& registry, pattern::MacroEnvironment& macroEnv);
ProcessedCode processImports(
    const std::vector<std::shared_ptr<Expression>>& expressions,
    LibraryRegistry& registry);

}

#endif // IMPORT_H
