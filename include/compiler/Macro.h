#pragma once
#include "Expression.h"
#include <iostream>
#include <unordered_map>
#include <variant>

class MacroExpander {
private:
    struct MacroDefinition {
        Token name;
        std::vector<std::shared_ptr<Expression>> patterns;
        std::vector<std::shared_ptr<Expression>> templates;
    };

    using MatchMap = std::unordered_map<std::string, std::vector<AtomExpression>>;
    std::unordered_map<std::string, MacroDefinition> macros;

    std::vector<std::shared_ptr<Expression>> expandToAtoms(std::shared_ptr<Expression> expr) {
        std::vector<std::shared_ptr<Expression>> output;

        std::visit(overloaded {
            [&](const sExpression& se) {
                for (const auto& ele : se.elements) {
                    auto expanded = expandToAtoms(ele);
                    output.insert(output.end(), expanded.begin(), expanded.end());
                }
            },
            [&](const TailExpression& te) {
                auto expanded = expandToAtoms(te.expression);
                output.insert(output.end(), expanded.begin(), expanded.end());
            },
            [&](const AtomExpression& ae) {
                output.push_back(std::make_shared<Expression>(ae, expr->line));
            },
            [&](const auto&) {
                output.push_back(expr->clone());
            }
        }, expr->as);

        return output;
    }

    std::pair<bool, std::pair<std::shared_ptr<Expression>, MatchMap>> match(std::shared_ptr<Expression> expr, MacroDefinition& md) {
        MatchMap matches;

        if (auto sexp = std::get_if<sExpression>(&expr->as)) {
            for (const auto& pattern : md.patterns) {
                auto patternAtoms = expandToAtoms(pattern);
                auto exprAtoms = expandToAtoms(expr);
                matches.clear();
                size_t patternPos = 1;
                size_t exprPos = 1;
                bool matched = true;

                while (patternPos < patternAtoms.size() && matched) {
                    auto patAtom = std::get_if<AtomExpression>(&patternAtoms[patternPos]->as);
                    if (!patAtom) {
                        matched = false;
                        break;
                    }

                    if (patAtom->value.lexeme == "...") {
                        if (patternPos > 0) {
                            auto prevAtom = std::get_if<AtomExpression>(&patternAtoms[patternPos-1]->as);
                            if (prevAtom) {
                                while (exprPos < exprAtoms.size()) {
                                    if (auto exprAtom = std::get_if<AtomExpression>(&exprAtoms[exprPos]->as)) {
                                        matches["..."].push_back(*exprAtom);
                                    }
                                    exprPos++;
                                }
                            }
                        }
                        patternPos++;
                    } else {
                        if (exprPos >= exprAtoms.size()) {
                            matched = false;
                            break;
                        }

                        if (auto exprAtom = std::get_if<AtomExpression>(&exprAtoms[exprPos]->as)) {
                            matches[patAtom->value.lexeme].push_back(*exprAtom);
                            // Print regular pattern matches too
                            std::cout << patAtom->value.lexeme << " = " << exprAtom->value.lexeme << "\n";
                            exprPos++;
                            patternPos++;
                        } else {
                            matched = false;
                        }
                    }
                }

                if (matched && exprPos >= exprAtoms.size()) {
                    return {true, {pattern, matches}};
                }
            }
        }
        return {false, {}};
    }

    void collectMacros(const std::vector<std::shared_ptr<Expression>>& ast) {
        for (const auto& expr : ast) {
            if (auto define = std::get_if<DefineSyntaxExpression>(&expr->as)) {
                if (auto rules = std::get_if<SyntaxRulesExpression>(&define->rule->as)) {
                    std::cout << "Found macro: " << define->name.lexeme << "\n";
                    macros[define->name.lexeme] = MacroDefinition{
                        define->name,
                        rules->pattern,
                        rules->template_expr
                    };
                }
            }
        }
    }

public:
    std::optional<std::vector<std::shared_ptr<Expression>>> expand(
        const std::optional<std::vector<std::shared_ptr<Expression>>>& ast)
    {
        if (!ast) return std::nullopt;

        collectMacros(*ast);

        for (const auto& expr : *ast) {
            if (auto sexp = std::get_if<sExpression>(&expr->as)) {
                if (!sexp->elements.empty()) {
                    if (auto first = std::get_if<AtomExpression>(&sexp->elements.at(0)->as)) {
                        auto it = macros.find(first->value.lexeme);
                        if (it != macros.end()) {
                            const auto pair = match(expr, it->second);
                            pair.second.first->print();
                            for (const auto [k, v] : pair.second.second) {
                               std::cout << k << "\n";
                                for (const auto& val : v) {
                                    std::cout << val.value.lexeme;
                                }
                            }

                        }
                    }
                }
            }
        }


        return ast;
    }
};