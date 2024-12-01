#pragma once
#include "Expression.h"
#include <memory>
#include <string>

void debugPrintExpression(std::shared_ptr<Expression> expr, int indent = 0);
void debugPrintPatternStructure(std::shared_ptr<Expression> expr, int indent = 0);
void printTestResult(const std::string& testName, bool success);
