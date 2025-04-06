#include "DebugUtils.h"

#include "Visit.h"
#include <iostream>
#include <string>

void debugPrintExpression(std::shared_ptr<Expression> expr, int indent)
{
    std::string spaces(indent * 2, ' ');

    if (!expr) {
        std::cout << spaces << "null\n";
        return;
    }

    std::visit(overloaded {
                   [&](const AtomExpression& e) {
                       std::cout << spaces << "Atom: " << e.value.token.lexeme << "\n";
                   },
                   [&](const ListExpression& l) {
                       std::cout << spaces << "List (variadic=" << l.isVariadic << ") with "
                                 << l.elements.size() << " elements:\n";
                       for (const auto& elem : l.elements) {
                           debugPrintExpression(elem, indent + 1);
                       }
                   },
                   [&](const auto&) {
                       std::cout << spaces << "Other expression type\n";
                   } },
        expr->as);
}

void debugPrintPatternStructure(std::shared_ptr<Expression> expr, int indent)
{
    std::string spaces(indent * 2, ' ');

    std::visit(overloaded {
                   [&](const AtomExpression& atom) {
                       std::cout << spaces << "Atom: " << atom.value.token.lexeme << "\n";
                   },
                   [&](const ListExpression& list) {
                       std::cout << spaces << "List (variadic=" << list.isVariadic << ") with "
                                 << list.elements.size() << " elements:\n";
                       for (const auto& elem : list.elements) {
                           debugPrintPatternStructure(elem, indent + 1);
                       }
                   },
                   [&](const auto&) {
                       std::cout << spaces << "Other type\n";
                   } },
        expr->as);
}

void printTestResult(const std::string& testName, bool success)
{
    std::cout << (success ? "[PASS] " : "[FAIL] ") << testName << "\n";
}
