#pragma once
#include "Expression.h"
#include <memory>
#include <string>
#include <vector>

namespace import {

std::vector<std::shared_ptr<Expression>> processImports(const std::vector<std::shared_ptr<Expression>>& expressions);

bool isDefinition(const std::shared_ptr<Expression>& expr);
bool fileExists(const std::string& path);
std::string resolveLibraryPath(const ImportExpression::ImportSpec& spec);

std::vector<std::shared_ptr<Expression>> processImport(const ImportExpression& import);

void importLibrary(
    const std::string& path,
    const ImportExpression::ImportSpec& spec,
    std::vector<std::shared_ptr<Expression>>& expressions);

std::shared_ptr<Expression> transformExpression(
    const std::shared_ptr<Expression>& expr,
    const ImportExpression::ImportSpec& spec);

std::shared_ptr<Expression> renameDefinition(
    const std::shared_ptr<Expression>& expr,
    const std::string& oldName,
    const std::string& newName);

} 
