#include "Error.h"
#include "Expression.h"
#include "Interpreter.h"
#include <unordered_map>

std::optional<std::shared_ptr<Expression>> MacroExpander::expand(
    const std::string& name,
    const std::vector<std::shared_ptr<Expression>>& args)
{
    std::cout << "\n=== Expanding macro: " << name << " ===" << std::endl;

    auto it = macros.find(name);
    if (it == macros.end()) {
        std::cout << "No macro found with name: " << name << std::endl;
        return std::nullopt;
    }

    if (auto rule = std::get_if<SyntaxRuleExpression>(&it->second->as)) {
        std::cout << "Found syntax rule. Pattern: " << std::endl;
        rule->pattern->print();

        // Build call expression
        std::vector<std::shared_ptr<Expression>> elements;
        elements.push_back(std::make_shared<Expression>(
            AtomExpression { Token { Tokentype::IDENTIFIER, name, 0, 0 } }, 0));
        elements.insert(elements.end(), args.begin(), args.end());
        auto callExpr = std::make_shared<Expression>(sExpression { std::move(elements) }, 0);

        std::cout << "Call expression: " << std::endl;
        callExpr->print();

        SchemeValue callValue = expressionToValue(*callExpr);
        SchemeValue patternValue = expressionToValue(*rule->pattern);

        std::cout << "As SchemeValues:" << std::endl;
        std::cout << "Pattern: " << patternValue.toString() << std::endl;
        std::cout << "Call: " << callValue.toString() << std::endl;

        std::unordered_map<std::string, SchemeValue> bindings;
        if (matchPattern(patternValue, callValue, bindings)) {
            std::cout << "Pattern matched! Bindings:" << std::endl;
            for (const auto& [key, value] : bindings) {
                std::cout << "  " << key << " => ";
                if (value.isValue<std::shared_ptr<Expression>>()) {
                    value.asExpr()->print();
                } else {
                    std::cout << value.toString() << std::endl;
                }
            }

            std::cout << "Template before expansion: " << std::endl;
            rule->template_expr->print();

            SchemeValue templateValue = expressionToValue(*rule->template_expr);
            SchemeValue expanded = expandTemplate(templateValue, bindings);

            std::cout << "Final expansion: " << expanded.toString() << std::endl;
            return std::make_shared<Expression>(*valueToExpression(expanded, interpreter));
        }
        std::cout << "Pattern did not match" << std::endl;
    }
    return std::nullopt;
}

bool MacroExpander::matchPattern(
    const SchemeValue& pattern,
    const SchemeValue& expr,
    std::unordered_map<std::string, SchemeValue>& bindings)
{
    std::cout << "\n=== Matching pattern ===" << std::endl;
    if (pattern.isValue<std::shared_ptr<Expression>>()) {
        auto patExpr = pattern.asExpr();
        if (auto tail = std::get_if<TailExpression>(&patExpr->as)) {
            std::cout << "Pattern is tail expression, unwrapping" << std::endl;
            return matchPattern(expressionToValue(*tail->expression), expr, bindings);
        }
        patExpr->print();
    } else {
        std::cout << "Pattern: " << pattern.toString() << std::endl;
    }

    if (expr.isValue<std::shared_ptr<Expression>>()) {
        auto exprVal = expr.asExpr();
        if (auto tail = std::get_if<TailExpression>(&exprVal->as)) {
            std::cout << "Expression is tail expression, unwrapping" << std::endl;
            return matchPattern(pattern, expressionToValue(*tail->expression), bindings);
        }
        exprVal->print();
    } else {
        std::cout << "Expression: " << expr.toString() << std::endl;
    }

    if (pattern.isValue<Symbol>()) {
        std::cout << "Pattern is symbol: " << pattern.asSymbol() << std::endl;
        bindings[pattern.asSymbol()] = expr;
        if (expr.isValue<std::shared_ptr<Expression>>()) {
            std::cout << "Bound " << pattern.asSymbol() << " to expression:" << std::endl;
            expr.asExpr()->print();
        } else {
            std::cout << "Bound " << pattern.asSymbol() << " to: " << expr.toString() << std::endl;
        }
        return true;
    }

    if (pattern.isValue<std::list<SchemeValue>>() && expr.isValue<std::list<SchemeValue>>()) {
        const auto& patList = pattern.asList();
        const auto& exprList = expr.asList();

        if (patList.size() != exprList.size()) {
            std::cout << "Size mismatch: pattern=" << patList.size()
                      << " expr=" << exprList.size() << std::endl;
            return false;
        }

        std::cout << "Matching list elements" << std::endl;
        auto patIt = patList.begin();
        auto exprIt = exprList.begin();
        int i = 0;
        while (patIt != patList.end()) {
            std::cout << "Element " << i++ << ":" << std::endl;
            if (!matchPattern(*patIt++, *exprIt++, bindings)) {
                std::cout << "Failed to match element" << std::endl;
                return false;
            }
        }
        std::cout << "Successfully matched all elements" << std::endl;
        return true;
    }

    std::cout << "No matching rule for pattern type" << std::endl;
    return false;
}

SchemeValue MacroExpander::expandTemplate(
    const SchemeValue& templ,
    const std::unordered_map<std::string, SchemeValue>& bindings)
{
    std::cout << "\n=== Expanding template ===" << std::endl;
    std::cout << "Template: " << templ.toString() << std::endl;

    if (templ.isValue<Number>() || templ.isValue<bool>() || templ.isValue<std::string>() || templ.isValue<std::shared_ptr<Expression>>()) {
        std::cout << "Template is literal value, keeping as is" << std::endl;
        return templ;
    }

    if (templ.isValue<Symbol>()) {
        auto it = bindings.find(templ.asSymbol());
        if (it != bindings.end()) {
            std::cout << "Expanding symbol " << templ.asSymbol()
                      << " to " << it->second.toString() << std::endl;
            return it->second;
        }
        std::cout << "Symbol " << templ.asSymbol() << " not in bindings, keeping as is" << std::endl;
        return templ;
    }

    if (templ.isValue<std::list<SchemeValue>>()) {
        std::cout << "Expanding list elements" << std::endl;
        std::list<SchemeValue> expanded;
        int i = 0;
        for (const auto& elem : templ.asList()) {
            std::cout << "Element " << i++ << ":" << std::endl;
            expanded.push_back(expandTemplate(elem, bindings));
        }
        return SchemeValue(std::move(expanded));
    }

    std::cout << "Template is unknown type, returning as is" << std::endl;
    return templ;
}
