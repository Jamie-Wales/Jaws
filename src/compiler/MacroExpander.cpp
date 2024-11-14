#include "Error.h"
#include "Expression.h"
#include "Interpreter.h"
#include <unordered_map>

std::optional<std::shared_ptr<Expression>> MacroExpander::expand(
    const std::string& name,
    const std::vector<std::shared_ptr<Expression>>& args)
{
    std::cout << "\nExpanding macro: " << name << std::endl;
    auto it = macros.find(name);
    if (it == macros.end()) {
        std::cout << "No macro found for: " << name << std::endl;
        return std::nullopt;
    }

    if (auto rules = std::get_if<SyntaxRulesExpression>(&it->second->as)) {
        std::cout << "Found rules for macro: " << name << std::endl;
        std::cout << "Number of patterns: " << rules->patterns.size() << std::endl;

        std::vector<std::shared_ptr<Expression>> elements;
        elements.push_back(std::make_shared<Expression>(
            AtomExpression { Token { Tokentype::IDENTIFIER, name, 0, 0 } }, 0));
        elements.insert(elements.end(), args.begin(), args.end());

        auto callExpr = std::make_shared<Expression>(sExpression { std::move(elements) }, 0);
        std::cout << "Macro call expression:" << std::endl;
        callExpr->print();

        for (size_t i = 0; i < rules->patterns.size(); i++) {
            const auto& pattern = rules->patterns[i];
            std::cout << "\nTrying pattern " << i + 1 << ":" << std::endl;
            pattern.pattern->print();

            std::unordered_map<std::string, std::shared_ptr<Expression>> bindings;
            interpreter.scope->pushFrame();

            try {
                if (matchPattern(pattern, callExpr, bindings)) {
                    std::cout << "Pattern matched! Current bindings:" << std::endl;
                    for (const auto& [key, value] : bindings) {
                        std::cout << "  " << key << " = ";
                        value->print();
                    }

                    std::cout << "Expanding template:" << std::endl;
                    pattern.template_expr->print();

                    auto result = expandTemplate(pattern.template_expr, bindings);
                    std::cout << "Final result:" << std::endl;
                    result->print();

                    interpreter.scope->popFrame();
                    return result;
                }
            } catch (const std::exception& e) {
                std::cout << "Error during expansion: " << e.what() << std::endl;
                interpreter.scope->popFrame();
                continue;
            }
            std::cout << "Pattern did not match" << std::endl;
            interpreter.scope->popFrame();
        }
        std::cout << "No matching pattern found" << std::endl;
    }
    return std::nullopt;
}
bool MacroExpander::matchPattern(
    const SyntaxPattern& pattern,
    std::shared_ptr<Expression> expr,
    std::unordered_map<std::string, std::shared_ptr<Expression>>& bindings)
{
    std::cout << "\nMatching pattern:" << std::endl;
    pattern.pattern->print();
    std::cout << "Against expression:" << std::endl;
    expr->print();

    if (auto tail = std::get_if<TailExpression>(&pattern.pattern->as)) {
        std::cout << "Pattern is tail expression, matching inner pattern:" << std::endl;
        tail->expression->print();
        return matchPattern(SyntaxPattern(pattern.literals, tail->expression, nullptr), expr, bindings);
    }

    if (auto atom = std::get_if<AtomExpression>(&pattern.pattern->as)) {
        bool isLiteral = std::find_if(pattern.literals.begin(), pattern.literals.end(),
                             [&](const Token& lit) { return lit.lexeme == atom->value.lexeme; })
            != pattern.literals.end();

        if (isLiteral) {
            std::cout << "Checking literal: " << atom->value.lexeme << std::endl;
            auto exprAtom = std::get_if<AtomExpression>(&expr->as);
            bool matches = exprAtom && exprAtom->value.lexeme == atom->value.lexeme;
            std::cout << (matches ? "Literal matched" : "Literal did not match") << std::endl;
            return matches;
        } else {
            std::cout << "Binding variable " << atom->value.lexeme << " to:" << std::endl;
            expr->print();
            bindings[atom->value.lexeme] = expr;
            return true;
        }
    }

    auto patternExpr = std::get_if<sExpression>(&pattern.pattern->as);
    if (!patternExpr) {
        std::cout << "Pattern is not an s-expression, atom, or tail expression" << std::endl;
        return false;
    }

    auto actualExpr = std::get_if<sExpression>(&expr->as);
    if (!actualExpr || patternExpr->elements.size() != actualExpr->elements.size()) {
        std::cout << "Size mismatch: pattern has " << (patternExpr ? patternExpr->elements.size() : 0) << " elements, expression has " << (actualExpr ? actualExpr->elements.size() : 0) << std::endl;
        return false;
    }

    for (size_t i = 0; i < patternExpr->elements.size(); i++) {
        std::cout << "\nMatching element " << i << ":" << std::endl;
        SyntaxPattern elementPattern(pattern.literals, patternExpr->elements[i], nullptr);
        if (!matchPattern(elementPattern, actualExpr->elements[i], bindings)) {
            std::cout << "Failed to match element " << i << std::endl;
            return false;
        }
    }

    std::cout << "Successfully matched all elements" << std::endl;
    return true;
}
std::shared_ptr<Expression> MacroExpander::expandTemplate(
    std::shared_ptr<Expression> templ,
    const std::unordered_map<std::string, std::shared_ptr<Expression>>& bindings)
{
    std::cout << "\nExpanding template part:" << std::endl;
    templ->print();

    auto result = std::visit(overloaded {
                                 [&](const AtomExpression& atom) -> std::shared_ptr<Expression> {
                                     std::cout << "Expression type is: AtomExpression" << std::endl;

                                     auto it = bindings.find(atom.value.lexeme);
                                     if (it != bindings.end()) {
                                         std::cout << "Found binding for " << atom.value.lexeme << ", replacing with:" << std::endl;
                                         it->second->print();
                                         return it->second->clone();
                                     }
                                     std::cout << "No binding found for " << atom.value.lexeme << ", keeping as is" << std::endl;
                                     return std::make_shared<Expression>(AtomExpression { atom }, templ->line);
                                 },
                                 [&](const sExpression& sExpr) -> std::shared_ptr<Expression> {
                                     std::cout << "Expression type is: sExpression with " << sExpr.elements.size() << " elements" << std::endl;

                                     std::vector<std::shared_ptr<Expression>> expanded;
                                     expanded.reserve(sExpr.elements.size());
                                     for (size_t i = 0; i < sExpr.elements.size(); i++) {
                                         std::cout << "Expanding element " << i << ":" << std::endl;
                                         sExpr.elements[i]->print();
                                         auto expandedElem = expandTemplate(sExpr.elements[i], bindings);
                                         std::cout << "Expanded to:" << std::endl;
                                         expandedElem->print();
                                         expanded.push_back(expandedElem);
                                     }
                                     auto result = std::make_shared<Expression>(sExpression { std::move(expanded) }, templ->line);
                                     std::cout << "Complete s-expression expansion:" << std::endl;
                                     result->print();
                                     return result;
                                 },
                                 [&](const TailExpression& tailExpr) -> std::shared_ptr<Expression> {
                                     std::cout << "Expression type is: TailExpression" << std::endl;

                                     std::cout << "Expanding tail expression:" << std::endl;
                                     tailExpr.expression->print();
                                     auto expanded = expandTemplate(tailExpr.expression, bindings);
                                     std::cout << "Expanded tail to:" << std::endl;
                                     expanded->print();
                                     return expanded;
                                 },
                                 [&](const IfExpression& ifExpr) -> std::shared_ptr<Expression> {
                                     std::cout << "Expression type is: IfExpression" << std::endl;

                                     std::cout << "Expanding if expression..." << std::endl;
                                     auto condition = expandTemplate(ifExpr.condition, bindings);
                                     std::cout << "Expanded condition to:" << std::endl;
                                     condition->print();

                                     auto thenBranch = expandTemplate(ifExpr.then, bindings);
                                     std::cout << "Expanded then branch to:" << std::endl;
                                     thenBranch->print();

                                     std::optional<std::shared_ptr<Expression>> elseBranch;
                                     if (ifExpr.el) {
                                         elseBranch = expandTemplate(*ifExpr.el, bindings);
                                         std::cout << "Expanded else branch to:" << std::endl;
                                         (*elseBranch)->print();
                                     }

                                     auto result = std::make_shared<Expression>(
                                         IfExpression { condition, thenBranch, elseBranch },
                                         templ->line);
                                     std::cout << "Complete if expression expansion:" << std::endl;
                                     result->print();
                                     return result;
                                 },
                                 [&](const auto&) -> std::shared_ptr<Expression> {
                                     std::cout << "Unknown expression type, cloning as is" << std::endl;
                                     return templ->clone();
                                 } },
        templ->as);

    return result;
}
