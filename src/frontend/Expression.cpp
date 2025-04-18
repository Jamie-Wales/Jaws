#include "Expression.h"
#include "Token.h"
#include "Visit.h"
#include <cstddef>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <variant>

Expression::Expression(Expression::ExpressionVariant as, int line)
    : as { as }
    , line { line }
{
}

BeginExpression::BeginExpression(std::vector<std::shared_ptr<Expression>> values)
    : values { values }
{
}

sExpression::sExpression(std::vector<std::shared_ptr<Expression>> elems, bool variadic)
    : elements(std::move(elems))
    , isVariadic(variadic)
{
}

QuasiQuoteExpression::QuasiQuoteExpression(std::shared_ptr<Expression> value)
    : value(std::move(value))
{
}

UnquoteExpression::UnquoteExpression(std::shared_ptr<Expression> value)
    : value(std::move(value))
{
}

SpliceExpression::SpliceExpression(std::shared_ptr<Expression> value)
    : value(std::move(value))
{
}

DefineLibraryExpression::DefineLibraryExpression(
    std::vector<std::shared_ptr<Expression>> name,
    std::vector<HygienicSyntax> exp,
    std::vector<ImportExpression::ImportSpec> imp,
    std::vector<std::shared_ptr<Expression>> b)
    : libraryName(std::move(name))
    , exports(std::move(exp))
    , imports(std::move(imp))
    , body(std::move(b))
{
}

AtomExpression::AtomExpression(Token token)
    : value { token, SyntaxContext {} }
{
}

AtomExpression::AtomExpression(HygienicSyntax syntax)
    : value(std::move(syntax))
{
}

// SetExpression implementation
SetExpression::SetExpression(const Token& identifier, std::shared_ptr<Expression> value)
    : identifier { identifier, SyntaxContext {} }
    , value(std::move(value))
{
}

SetExpression::SetExpression(const HygienicSyntax& syntax, std::shared_ptr<Expression> value)
    : identifier(syntax)
    , value(std::move(value))
{
}

// DefineSyntaxExpression implementation
DefineSyntaxExpression::DefineSyntaxExpression(Token name, std::shared_ptr<Expression> rule)
    : name { name, SyntaxContext {} }
    , rule(std::move(rule))
{
}

DefineSyntaxExpression::DefineSyntaxExpression(HygienicSyntax name, std::shared_ptr<Expression> rule)
    : name(std::move(name))
    , rule(std::move(rule))
{
}

// DefineProcedure implementation
DefineProcedure::DefineProcedure(Token name, std::vector<Token> parameters,
    std::vector<std::shared_ptr<Expression>> body, bool isVariadic)
    : name { name, SyntaxContext {} }
    , parameters {}
    , body(std::move(body))
    , isVariadic(isVariadic)
{
    this->parameters.reserve(parameters.size());
    for (const auto& param : parameters) {
        this->parameters.push_back(HygienicSyntax { param, SyntaxContext {} });
    }
}

DefineProcedure::DefineProcedure(HygienicSyntax name, std::vector<HygienicSyntax> parameters,
    std::vector<std::shared_ptr<Expression>> body, bool isVariadic)
    : name(std::move(name))
    , parameters(std::move(parameters))
    , body(std::move(body))
    , isVariadic(isVariadic)
{
}

// LetExpression implementation
LetExpression::LetExpression(std::optional<Token> name,
    std::vector<std::pair<Token, std::shared_ptr<Expression>>> arguments,
    std::vector<std::shared_ptr<Expression>> body)
    : name {}
    , arguments {}
    , body(std::move(body))
{
    if (name) {
        this->name = HygienicSyntax { *name, SyntaxContext {} };
    }

    this->arguments.reserve(arguments.size());
    for (const auto& [token, expr] : arguments) {
        this->arguments.push_back({ HygienicSyntax { token, SyntaxContext {} }, expr });
    }
}

LetExpression::LetExpression(std::optional<HygienicSyntax> name, Args arguments,
    std::vector<std::shared_ptr<Expression>> body)
    : name(std::move(name))
    , arguments(std::move(arguments))
    , body(std::move(body))
{
}

std::vector<Token> LetExpression::getParameterTokens() const
{
    std::vector<Token> params;
    params.reserve(arguments.size());
    for (const auto& pair : arguments) {
        params.push_back(pair.first.token);
    }
    return params;
}

std::vector<HygienicSyntax> LetExpression::getParameterSyntax() const
{
    std::vector<HygienicSyntax> params;
    params.reserve(arguments.size());
    for (const auto& pair : arguments) {
        params.push_back(pair.first);
    }
    return params;
}

// SyntaxRule implementation
SyntaxRule::SyntaxRule(std::shared_ptr<Expression> pattern, std::shared_ptr<Expression> template_expr)
    : pattern(std::move(pattern))
    , template_expr(std::move(template_expr))
{
}
void SyntaxRule::analyzePattern(const std::vector<Token>& literals)
{
    analyzePatternRecursive(pattern, literals, 0);
}

bool isPatternVariable(const Token& token,
    const std::vector<Token>& literals)
{
    if (token.lexeme == "_") {
        return true;
    }
    if (token.lexeme == ".") {
        return false;
    }

    bool isLiteral = std::find_if(literals.begin(), literals.end(),
                         [&](const Token& lit) { return lit.lexeme == token.lexeme; })
        != literals.end(); // Found in literals

    if (isLiteral) {
        DEBUG_LOG("isPatternVariable: Token '" << token.lexeme << "' found in literals list. Not a variable."); // Optional log
        return false;
    }
    bool result = (token.type == Tokentype::IDENTIFIER);
    DEBUG_LOG("isPatternVariable: Token '" << token.lexeme << "' isIdentifier=" << (token.type == Tokentype::IDENTIFIER) << ", isLiteral=false, isKeyword=false, isDot=false, isWildcard=false. Result: " << result); // Optional log
    return result;
}
bool isEllipsisToken(const std::shared_ptr<Expression>& expr)
{
    if (!expr)
        return false;

    return std::visit(overloaded {
                          [](const AtomExpression& atom) {
                              return atom.value.token.type == Tokentype::ELLIPSIS;
                          },
                          [](const auto&) { return false; } },
        expr->as);
}

void SyntaxRule::analyzePatternRecursive(
    const std::shared_ptr<Expression>& expr,
    const std::vector<Token>& literals,
    int ellipsis_level)
{
    if (!expr)
        return;

    // Use std::visit to handle different expression types
    std::visit(overloaded {
                   [&](const AtomExpression& atom) {
                       if (isPatternVariable(atom.value.token, literals)) {
                           std::string var_name = atom.value.token.lexeme;
                           if (ellipsis_level > 0) {
                               pattern_info.ellipsis_vars.insert(var_name);
                               pattern_info.ellipsis_depth[var_name] = std::max(
                                   pattern_info.ellipsis_depth[var_name], ellipsis_level);
                           } else {
                               pattern_info.regular_vars.insert(var_name);
                           }
                       }
                   },
                   [&](const ListExpression& list) {
                       for (size_t i = 0; i < list.elements.size(); i++) {
                           bool has_ellipsis = (i + 1 < list.elements.size() && isEllipsisToken(list.elements[i + 1]));
                           analyzePatternRecursive(
                               list.elements[i],
                               literals,
                               has_ellipsis ? ellipsis_level + 1 : ellipsis_level);
                           if (has_ellipsis)
                               i++;
                       }
                   },
                   [&](const VectorExpression& vec) {
                       for (size_t i = 0; i < vec.elements.size(); i++) {
                           bool has_ellipsis = (i + 1 < vec.elements.size() && isEllipsisToken(vec.elements[i + 1]));
                           analyzePatternRecursive(
                               vec.elements[i],
                               literals,
                               has_ellipsis ? ellipsis_level + 1 : ellipsis_level);
                           if (has_ellipsis)
                               i++;
                       }
                   },
                   [&](const QuoteExpression& quote) {
                   },
                   [&](const auto&) {
                   } },
        expr->as);
}

// SyntaxRulesExpression implementation
SyntaxRulesExpression::SyntaxRulesExpression(std::vector<Token> literals, std::vector<SyntaxRule> rules)
    : literals(std::move(literals))
    , rules(std::move(rules))
{
}

// QuoteExpression implementation
QuoteExpression::QuoteExpression(std::shared_ptr<Expression> expression)
    : expression(std::move(expression))
{
}

// DefineExpression implementation
DefineExpression::DefineExpression(Token n, std::shared_ptr<Expression> v)
    : name { n, SyntaxContext {} }
    , value(std::move(v))
{
}

DefineExpression::DefineExpression(HygienicSyntax n, std::shared_ptr<Expression> v)
    : name(std::move(n))
    , value(std::move(v))
{
}

// TailExpression implementation
TailExpression::TailExpression(std::shared_ptr<Expression> expression)
    : expression(expression)
{
}

// ImportSpec implementation with constructors for backward compatibility
ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib)
    : type(ImportSet::Type::DIRECT)
    , library(std::move(lib))
{
}

ImportExpression::ImportSpec::ImportSpec(ImportSet::Type t,
    std::vector<std::shared_ptr<Expression>> lib,
    std::vector<HygienicSyntax> ids)
    : type(t)
    , library(std::move(lib))
    , identifiers(std::move(ids))
{
}

ImportExpression::ImportSpec::ImportSpec(ImportSet::Type t,
    std::vector<std::shared_ptr<Expression>> lib,
    std::vector<Token> ids)
    : type(t)
    , library(std::move(lib))
{
    identifiers.reserve(ids.size());
    for (const auto& id : ids) {
        identifiers.push_back(HygienicSyntax { id, SyntaxContext {} });
    }
}

ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib,
    HygienicSyntax pfx)
    : type(ImportSet::Type::PREFIX)
    , library(std::move(lib))
    , prefix(std::move(pfx))
{
}

ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib,
    Token pfx)
    : type(ImportSet::Type::PREFIX)
    , library(std::move(lib))
    , prefix { pfx, SyntaxContext {} }
{
}

ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib,
    std::vector<std::pair<HygienicSyntax, HygienicSyntax>> renames_)
    : type(ImportSet::Type::RENAME)
    , library(std::move(lib))
    , renames(std::move(renames_))
{
}

ImportExpression::ImportSpec::ImportSpec(std::vector<std::shared_ptr<Expression>> lib,
    std::vector<std::pair<Token, Token>> renames_)
    : type(ImportSet::Type::RENAME)
    , library(std::move(lib))
{
    renames.reserve(renames_.size());
    for (const auto& [oldName, newName] : renames_) {
        renames.push_back({ HygienicSyntax { oldName, SyntaxContext {} },
            HygienicSyntax { newName, SyntaxContext {} } });
    }
}

ImportExpression::ImportSpec::ImportSpec(const ImportSpec& other)
    : type(other.type)
    , library(other.library)
    , identifiers(other.identifiers)
    , prefix(other.prefix)
    , renames(other.renames)
{
}

// ImportExpression constructor
ImportExpression::ImportExpression(std::vector<ImportSpec> imports_)
    : imports(std::move(imports_))
{
}

ImportExpression::ImportSpec makeDirectImport(std::vector<std::shared_ptr<Expression>> library)
{
    return ImportExpression::ImportSpec(std::move(library));
}

ImportExpression::ImportSpec makeOnlyImport(
    std::vector<std::shared_ptr<Expression>> library,
    std::vector<Token> identifiers)
{
    return ImportExpression::ImportSpec(
        ImportExpression::ImportSet::Type::ONLY,
        std::move(library),
        std::move(identifiers));
}

ImportExpression::ImportSpec makeExceptImport(
    std::vector<std::shared_ptr<Expression>> library,
    std::vector<Token> identifiers)
{
    return ImportExpression::ImportSpec(
        ImportExpression::ImportSet::Type::EXCEPT,
        std::move(library),
        std::move(identifiers));
}

ImportExpression::ImportSpec makePrefixImport(
    std::vector<std::shared_ptr<Expression>> library,
    Token prefix)
{
    return ImportExpression::ImportSpec(std::move(library), std::move(prefix));
}

ImportExpression::ImportSpec makeRenameImport(
    std::vector<std::shared_ptr<Expression>> library,
    std::vector<std::pair<Token, Token>> renames)
{
    return ImportExpression::ImportSpec(std::move(library), std::move(renames));
}

// ListExpression implementation
ListExpression::ListExpression(std::vector<std::shared_ptr<Expression>> elems, bool variadic)
    : elements(std::move(elems))
    , isVariadic(variadic)
{
}

// LambdaExpression implementation
LambdaExpression::LambdaExpression(std::vector<Token> parameters, std::vector<std::shared_ptr<Expression>> body,
    bool isVariadic)
    : parameters {}
    , body(std::move(body))
    , isVariadic(isVariadic)
{
    this->parameters.reserve(parameters.size());
    for (const auto& param : parameters) {
        this->parameters.push_back(HygienicSyntax { param, SyntaxContext {} });
    }
}

LambdaExpression::LambdaExpression(std::vector<HygienicSyntax> parameters, std::vector<std::shared_ptr<Expression>> body,
    bool isVariadic)
    : parameters(std::move(parameters))
    , body(std::move(body))
    , isVariadic(isVariadic)
{
}

// VectorExpression implementation
VectorExpression::VectorExpression(std::vector<std::shared_ptr<Expression>> elems)
    : elements(std::move(elems))
{
}

// IfExpression implementation
IfExpression::IfExpression(std::shared_ptr<Expression> condition, std::shared_ptr<Expression> then,
    std::optional<std::shared_ptr<Expression>> el)
    : condition(std::move(condition))
    , then(std::move(then))
    , el(std::move(el))
{
}

void indent(std::stringstream& ss, int indentLevel)
{
    for (int i = 0; i < indentLevel; i++) {
        ss << "    ";
    }
}

std::string Expression::toString() const
{
    std::stringstream ss;
    toString(ss);
    return ss.str();
}

void Expression::toString(std::stringstream& ss) const
{
    std::visit(
        overloaded {
            [&](const AtomExpression& e) {
                ss << e.value.token.lexeme;
            },

            [&](const BeginExpression& e) {
                ss << "(begin ";
                for (const auto val : e.values) {
                    val->toString(ss);
                    ss << " ";
                }
                ss << ")";
            },
            [&](const DefineLibraryExpression& e) {
                ss << "(define-library (";
                // Print library name
                for (size_t i = 0; i < e.libraryName.size(); ++i) {
                    e.libraryName[i]->toString(ss);
                    if (i < e.libraryName.size() - 1)
                        ss << " ";
                }
                ss << ")";
                if (!e.exports.empty()) {
                    ss << " (export";
                    for (const auto& exp : e.exports) {
                        ss << " " << exp.token.lexeme;
                    }
                    ss << ")";
                }
                if (!e.imports.empty()) {
                    ss << " (import";
                    for (const auto& spec : e.imports) {
                        ss << " ...import-spec..."; // Placeholder - reuse ImportExpression toString if needed
                    }
                    ss << ")";
                }

                // Print body if any
                if (!e.body.empty()) {
                    ss << " (begin";
                    for (const auto& expr : e.body) {
                        ss << " ";
                        expr->toString(ss);
                    }
                    ss << ")";
                }
                ss << ")";
            },

            [&](const QuasiQuoteExpression& e) {
                ss << "(quasiquote ";
                e.value->toString(ss);
                ss << ")";
            },

            [&](const UnquoteExpression& e) {
                ss << "(unquote ";
                e.value->toString(ss);
                ss << ")";
            },

            [&](const SpliceExpression& e) {
                ss << "(unquote-splice ";
                e.value->toString(ss);
                ss << ")";
            },
            [&](const sExpression& e) {
                ss << "(";
                for (size_t i = 0; i < e.elements.size(); ++i) {
                    e.elements[i]->toString(ss);
                    if (i < e.elements.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const ListExpression& e) {
                ss << "(";
                for (size_t i = 0; i < e.elements.size(); ++i) {
                    e.elements[i]->toString(ss);
                    if (i < e.elements.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const DefineExpression& e) {
                ss << "(define " << e.name.token.lexeme << " ";
                e.value->toString(ss);
                ss << ")";
            },
            [&](const DefineProcedure& e) {
                ss << "(define (" << e.name.token.lexeme;
                for (const auto& param : e.parameters) {
                    ss << " " << param.token.lexeme;
                }
                ss << (e.isVariadic ? " ..." : "");
                ss << ") ";
                for (const auto& expr : e.body) {
                    expr->toString(ss);
                }
                ss << ")";
            },
            [&](const VectorExpression& e) {
                ss << "#(";
                for (size_t i = 0; i < e.elements.size(); ++i) {
                    e.elements[i]->toString(ss);
                    if (i < e.elements.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const LambdaExpression& e) {
                ss << "(lambda (";
                for (size_t i = 0; i < e.parameters.size(); ++i) {
                    ss << e.parameters[i].token.lexeme;
                    if (i < e.parameters.size() - 1)
                        ss << " ";
                }
                ss << ") ";
                for (size_t i = 0; i < e.body.size(); ++i) {
                    e.body[i]->toString(ss);
                    if (i < e.body.size() - 1)
                        ss << " ";
                }
                ss << ")";
            },
            [&](const IfExpression& e) {
                ss << "(if ";
                e.condition->toString(ss);
                ss << " ";
                e.then->toString(ss);
                if (e.el) {
                    ss << " ";
                    (*e.el)->toString(ss);
                }
                ss << ")";
            },
            [&](const QuoteExpression& e) {
                ss << "(quote ";
                e.expression->toString(ss);
                ss << ")";
            },
            [&](const SetExpression& e) {
                ss << "(set! " << e.identifier.token.lexeme << " ";
                e.value->toString(ss);
                ss << ")";
            },
            [&](const TailExpression& e) {
                e.expression->toString(ss);
            },
            [&](const ImportExpression& e) {
                ss << "(import";
                for (const auto& spec : e.imports) {
                    ss << " ";
                    switch (spec.type) {
                    case ImportExpression::ImportSet::Type::DIRECT: {
                        ss << "(";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::ONLY: {
                        ss << "(only (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        for (const auto& id : spec.identifiers) {
                            ss << " " << id.token.lexeme;
                        }
                        ss << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::EXCEPT: {
                        ss << "(except (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        for (const auto& id : spec.identifiers) {
                            ss << " " << id.token.lexeme;
                        }
                        ss << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::PREFIX: {
                        ss << "(prefix (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ") " << spec.prefix.token.lexeme << ")";
                        break;
                    }
                    case ImportExpression::ImportSet::Type::RENAME: {
                        ss << "(rename (";
                        for (const auto& part : spec.library) {
                            ss << " " << part->toString();
                        }
                        ss << ")";
                        for (const auto& [old_name, new_name] : spec.renames) {
                            ss << " (" << old_name.token.lexeme << " " << new_name.token.lexeme << ")";
                        }
                        ss << ")";
                        break;
                    }
                    }
                }
                ss << ")";
            },
            [&](const SyntaxRulesExpression& e) {
                ss << "(syntax-rules (";
                for (size_t i = 0; i < e.literals.size(); ++i) {
                    ss << e.literals[i].lexeme;
                    if (i < e.literals.size() - 1)
                        ss << " ";
                }
                ss << ") ";
                for (const auto& rule : e.rules) {
                    ss << "(";
                    rule.pattern->toString(ss);
                    ss << " ";
                    rule.template_expr->toString(ss);
                    ss << ") ";
                }
                ss << ")";
            },
            [&](const DefineSyntaxExpression& e) {
                ss << "(define-syntax " << e.name.token.lexeme << " ";
                e.rule->toString(ss);
                ss << ")";
            },
            [&](const LetExpression& e) {
                ss << "(let ";
                if (e.name) {
                    ss << e.name->token.lexeme << " ";
                }
                ss << "(";
                for (size_t i = 0; i < e.arguments.size(); ++i) {
                    ss << "(" << e.arguments[i].first.token.lexeme << " ";
                    e.arguments[i].second->toString(ss);
                    ss << ")";
                    if (i < e.arguments.size() - 1)
                        ss << " ";
                }
                ss << ") ";
                for (size_t i = 0; i < e.body.size(); ++i) {
                    e.body[i]->toString(ss);
                    if (i < e.body.size() - 1)
                        ss << " ";
                }
                ss << ")";
            } },
        as);
}

std::shared_ptr<Expression> Expression::clone() const
{
    return std::visit(overloaded {

                          [&](const BeginExpression& e) {
                              std::vector<std::shared_ptr<Expression>> elements = {};
                              for (const auto val : e.values) {
                                  elements.push_back(val->clone());
                              }

                              return std::make_shared<Expression>(Expression {
                                  BeginExpression {
                                      elements },
                                  line });
                          },
                          [&](const QuasiQuoteExpression& p) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements = {};
                              return std::make_shared<Expression>(Expression {
                                  QuasiQuoteExpression {
                                      p.value->clone() },
                                  line });
                          },

                          [&](const UnquoteExpression& p) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements = {};
                              return std::make_shared<Expression>(Expression {
                                  UnquoteExpression {
                                      p.value->clone() },
                                  line });
                          },

                          [&](const SpliceExpression& p) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> elements = {};
                              return std::make_shared<Expression>(Expression {
                                  SpliceExpression {
                                      p.value->clone() },
                                  line });
                          },
                          [&](const DefineLibraryExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedName;
                              clonedName.reserve(e.libraryName.size());
                              for (const auto& part : e.libraryName) {
                                  clonedName.push_back(part->clone());
                              }

                              std::vector<HygienicSyntax> clonedExports = e.exports;

                              std::vector<ImportExpression::ImportSpec> clonedImports;
                              clonedImports.reserve(e.imports.size());
                              for (const auto& spec : e.imports) {
                                  clonedImports.push_back(ImportExpression::ImportSpec(spec));
                              }

                              std::vector<std::shared_ptr<Expression>> clonedBody;
                              clonedBody.reserve(e.body.size());
                              for (const auto& expr : e.body) {
                                  clonedBody.push_back(expr->clone());
                              }

                              return std::make_shared<Expression>(
                                  DefineLibraryExpression {
                                      std::move(clonedName),
                                      std::move(clonedExports),
                                      std::move(clonedImports),
                                      std::move(clonedBody) },
                                  line);
                          },

                          [&](const LetExpression& p) -> std::shared_ptr<Expression> {
                              LetExpression::Args output;
                              for (const auto& [syntax, second] : p.arguments) {
                                  output.push_back({ syntax, second->clone() });
                              }
                              std::vector<std::shared_ptr<Expression>> clonedBody;
                              for (const auto& expr : p.body) {
                                  clonedBody.push_back(expr->clone());
                              }
                              return std::make_shared<Expression>(Expression {
                                  LetExpression {
                                      p.name, output, clonedBody },
                                  line });
                          },
                          [&](const SyntaxRulesExpression& s) -> std::shared_ptr<Expression> {
                              std::vector<Token> clonedLiterals = s.literals; // Tokens can be copied directly
                              std::vector<SyntaxRule> clonedRules;

                              for (const auto& rule : s.rules) {
                                  clonedRules.push_back(SyntaxRule(rule.pattern->clone(), rule.template_expr->clone()));
                              }

                              return std::make_shared<Expression>(
                                  SyntaxRulesExpression {
                                      std::move(clonedLiterals),
                                      std::move(clonedRules) },
                                  line);
                          },
                          [&](const DefineSyntaxExpression& d) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(
                                  DefineSyntaxExpression {
                                      d.name,
                                      d.rule->clone() },
                                  line);
                          },

                          [&](const SetExpression& d) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(
                                  SetExpression {
                                      d.identifier,
                                      d.value->clone() },
                                  line);
                          },
                          [&](const ImportExpression& i) -> std::shared_ptr<Expression> {
                              std::vector<ImportExpression::ImportSpec> importsCopy;
                              for (const auto& spec : i.imports) {
                                  importsCopy.push_back(ImportExpression::ImportSpec(spec)); // Use copy constructor
                              }
                              return std::make_shared<Expression>(
                                  Expression { ImportExpression { std::move(importsCopy) }, line });
                          },
                          [&](const TailExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(TailExpression { e.expression->clone() }, line);
                          },
                          [&](const AtomExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(AtomExpression { e.value }, line);
                          },

                          [&](const ListExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedElements;
                              clonedElements.reserve(e.elements.size());
                              for (const auto& elem : e.elements) {
                                  clonedElements.push_back(elem->clone());
                              }
                              return std::make_shared<Expression>(ListExpression {
                                                                      std::move(clonedElements), e.isVariadic },
                                  line);
                          },
                          [&](const QuoteExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(QuoteExpression { e.expression->clone() }, line);
                          },
                          [&](const VectorExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedElements;
                              clonedElements.reserve(e.elements.size());
                              for (const auto& elem : e.elements) {
                                  clonedElements.push_back(elem->clone());
                              }
                              return std::make_shared<Expression>(VectorExpression { std::move(clonedElements) }, line);
                          },
                          [&](const sExpression& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedElements;
                              clonedElements.reserve(e.elements.size());
                              for (const auto& elem : e.elements) {
                                  clonedElements.push_back(elem->clone());
                              }
                              return std::make_shared<Expression>(sExpression { std::move(clonedElements) }, line);
                          },
                          [&](const DefineExpression& e) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(
                                  DefineExpression { e.name, e.value->clone() },
                                  line);
                          },
                          [&](const DefineProcedure& e) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedBody;
                              for (const auto& expr : e.body) {
                                  clonedBody.push_back(expr->clone());
                              }
                              return std::make_shared<Expression>(
                                  DefineProcedure { e.name, e.parameters, std::move(clonedBody), e.isVariadic },
                                  line);
                          },
                          [&](const LambdaExpression& l) -> std::shared_ptr<Expression> {
                              std::vector<std::shared_ptr<Expression>> clonedBody;
                              for (const auto& expr : l.body) {
                                  clonedBody.push_back(expr->clone());
                              }
                              return std::make_shared<Expression>(
                                  LambdaExpression { l.parameters, std::move(clonedBody), l.isVariadic },
                                  line);
                          },
                          [&](const IfExpression& i) -> std::shared_ptr<Expression> {
                              if (i.el) {
                                  return std::make_shared<Expression>(
                                      IfExpression {
                                          i.condition->clone(),
                                          i.then->clone(),
                                          (*i.el)->clone() },
                                      line);
                              } else {
                                  return std::make_shared<Expression>(
                                      IfExpression {
                                          i.condition->clone(),
                                          i.then->clone(),
                                          std::nullopt },
                                      line);
                              }
                          } },
        as);
}

void Expression::print(int indent) const
{
    std::string indentation(indent * 2, ' ');
    std::cout << indentation << toString() << std::endl;
}

std::string Expression::ASTToString() const
{
    std::stringstream ss;
    ASTToString(ss, 0);
    return ss.str();
}

void Expression::ASTToString(std::stringstream& ss, int indentLevel) const
{
    std::visit(
        overloaded {

            [&](const BeginExpression& e) {
                indent(ss, indentLevel);
                ss << "BeginExpression: ";

                for (const auto& val : e.values) {
                    val->ASTToString(ss, indentLevel + 1);
                }
            },
            [&](const AtomExpression& e) {
                indent(ss, indentLevel);
                ss << "AtomExpression: " << e.value.token.lexeme;
                // Print hygiene information if needed
                if (!e.value.context.marks.empty()) {
                    ss << " [marks: ";
                    for (auto it = e.value.context.marks.begin(); it != e.value.context.marks.end(); ++it) {
                        if (it != e.value.context.marks.begin()) {
                            ss << ", ";
                        }
                        ss << *it;
                    }
                    ss << "]";
                }
                ss << std::endl;
            },
            [&](const DefineLibraryExpression& e) {
                indent(ss, indentLevel);
                ss << "DefineLibraryExpression:\n";

                indent(ss, indentLevel + 1);
                ss << "Library Name:\n";
                for (const auto& part : e.libraryName) {
                    part->ASTToString(ss, indentLevel + 2);
                }

                if (!e.exports.empty()) {
                    indent(ss, indentLevel + 1);
                    ss << "Exports:\n";
                    for (const auto& exp : e.exports) {
                        indent(ss, indentLevel + 2);
                        ss << exp.token.lexeme << "\n"; // Add hygiene info if needed
                    }
                }

                if (!e.imports.empty()) {
                    indent(ss, indentLevel + 1);
                    ss << "Imports:\n";
                    for (const auto& spec : e.imports) {
                        indent(ss, indentLevel + 2);
                        ss << "...ImportSpec AST...\n"; // Placeholder
                    }
                }

                if (!e.body.empty()) {
                    indent(ss, indentLevel + 1);
                    ss << "Body (begin):\n";
                    for (const auto& expr : e.body) {
                        expr->ASTToString(ss, indentLevel + 2);
                    }
                }
            },
            [&](const QuasiQuoteExpression& e) {
                indent(ss, indentLevel);
                ss << "QuasiQuoteExpression: ";
                e.value->toString(ss);
                ss << std::endl;
            },

            [&](const UnquoteExpression& e) {
                indent(ss, indentLevel);
                ss << "UnquoteExpression: ";
                e.value->toString(ss);
                ss << std::endl;
            },

            [&](const SpliceExpression& e) {
                indent(ss, indentLevel);
                ss << "SpliceExpression: ";
                e.value->toString(ss);
                ss << std::endl;
            },
            [&](const ListExpression& e) {
                indent(ss, indentLevel);
                ss << "ListExpression";
                if (e.isVariadic)
                    ss << " (variadic)";
                ss << ":\n";
                for (auto& elem : e.elements) {
                    elem->ASTToString(ss, indentLevel + 1);
                }
            },
            [&](const sExpression& e) {
                indent(ss, indentLevel);
                ss << "sExpression";
                if (e.isVariadic)
                    ss << " (variadic)";
                ss << ":\n";
                for (auto& elem : e.elements) {
                    elem->ASTToString(ss, indentLevel + 1);
                }
            },
            [&](const DefineExpression& e) {
                indent(ss, indentLevel);
                ss << "DefineExpression: name=\"" << e.name.token.lexeme << "\"";
                // Print hygiene information if needed
                if (!e.name.context.marks.empty()) {
                    ss << " [marks: ";
                    for (auto it = e.name.context.marks.begin(); it != e.name.context.marks.end(); ++it) {
                        if (it != e.name.context.marks.begin()) {
                            ss << ", ";
                        }
                        ss << *it;
                    }
                    ss << "]";
                }
                ss << "\n";
                indent(ss, indentLevel);
                ss << "Value:\n";
                e.value->ASTToString(ss, indentLevel + 1);
            },
            [&](const DefineProcedure& e) {
                indent(ss, indentLevel);
                ss << "DefineProcedure: name=\"" << e.name.token.lexeme << "\"";
                // Print hygiene information if needed
                if (!e.name.context.marks.empty()) {
                    ss << " [marks: ";
                    for (auto it = e.name.context.marks.begin(); it != e.name.context.marks.end(); ++it) {
                        if (it != e.name.context.marks.begin()) {
                            ss << ", ";
                        }
                        ss << *it;
                    }
                    ss << "]";
                }
                if (e.isVariadic)
                    ss << " (variadic)";
                ss << "\n";
                if (!e.parameters.empty()) {
                    indent(ss, indentLevel + 1);
                    ss << "Params:\n";
                    for (auto& param : e.parameters) {
                        indent(ss, indentLevel + 2);
                        ss << param.token.lexeme;
                        // Print hygiene information if needed
                        if (!param.context.marks.empty()) {
                            ss << " [marks: ";
                            for (auto it = param.context.marks.begin(); it != param.context.marks.end(); ++it) {
                                if (it != param.context.marks.begin()) {
                                    ss << ", ";
                                }
                                ss << *it;
                            }
                            ss << "]";
                        }
                        ss << "\n";
                    }
                }
                indent(ss, indentLevel + 1);
                ss << "Body:\n";
                for (auto& expr : e.body) {
                    expr->ASTToString(ss, indentLevel + 2);
                }
            },
            [&](const VectorExpression& e) {
                indent(ss, indentLevel);
                ss << "VectorExpression:\n";
                for (auto& elem : e.elements) {
                    elem->ASTToString(ss, indentLevel + 1);
                }
            },
            [&](const LambdaExpression& e) {
                indent(ss, indentLevel);
                ss << "LambdaExpression";
                if (e.isVariadic)
                    ss << " (variadic)";
                ss << ":\n";
                if (!e.parameters.empty()) {
                    indent(ss, indentLevel + 1);
                    ss << "Parameters:\n";
                    for (auto& param : e.parameters) {
                        indent(ss, indentLevel + 2);
                        ss << param.token.lexeme;
                        // Print hygiene information if needed
                        if (!param.context.marks.empty()) {
                            ss << " [marks: ";
                            for (auto it = param.context.marks.begin(); it != param.context.marks.end(); ++it) {
                                if (it != param.context.marks.begin()) {
                                    ss << ", ";
                                }
                                ss << *it;
                            }
                            ss << "]";
                        }
                        ss << "\n";
                    }
                }
                indent(ss, indentLevel + 1);
                ss << "Body:\n";
                for (auto& expr : e.body) {
                    expr->ASTToString(ss, indentLevel + 2);
                }
            },
            [&](const IfExpression& e) {
                indent(ss, indentLevel);
                ss << "IfExpression:\n";

                indent(ss, indentLevel + 1);
                ss << "Condition:\n";
                e.condition->ASTToString(ss, indentLevel + 2);

                indent(ss, indentLevel + 1);
                ss << "Then:\n";
                e.then->ASTToString(ss, indentLevel + 2);

                if (e.el) {
                    indent(ss, indentLevel + 1);
                    ss << "Else:\n";
                    (*e.el)->ASTToString(ss, indentLevel + 2);
                }
            },
            [&](const QuoteExpression& e) {
                indent(ss, indentLevel);
                ss << "QuoteExpression:\n";
                e.expression->ASTToString(ss, indentLevel + 1);
            },
            [&](const SetExpression& e) {
                indent(ss, indentLevel);
                ss << "SetExpression: identifier=\"" << e.identifier.token.lexeme << "\"";
                // Print hygiene information if needed
                if (!e.identifier.context.marks.empty()) {
                    ss << " [marks: ";
                    for (auto it = e.identifier.context.marks.begin(); it != e.identifier.context.marks.end(); ++it) {
                        if (it != e.identifier.context.marks.begin()) {
                            ss << ", ";
                        }
                        ss << *it;
                    }
                    ss << "]";
                }
                ss << "\n";
                indent(ss, indentLevel);
                ss << "Value:\n";
                e.value->ASTToString(ss, indentLevel + 1);
            },
            [&](const TailExpression& e) {
                indent(ss, indentLevel);
                ss << "TailExpression:\n";
                e.expression->ASTToString(ss, indentLevel + 1);
            },
            [&](const ImportExpression& e) {
                indent(ss, indentLevel);
                ss << "ImportExpression:\n";
                for (auto& spec : e.imports) {
                    indent(ss, indentLevel + 1);
                    ss << "ImportSpec: ";
                    switch (spec.type) {
                    case ImportExpression::ImportSet::Type::DIRECT:
                        ss << "DIRECT\n";
                        break;
                    case ImportExpression::ImportSet::Type::ONLY:
                        ss << "ONLY\n";
                        break;
                    case ImportExpression::ImportSet::Type::EXCEPT:
                        ss << "EXCEPT\n";
                        break;
                    case ImportExpression::ImportSet::Type::PREFIX:
                        ss << "PREFIX (prefix=\"" << spec.prefix.token.lexeme << "\")\n";
                        break;
                    case ImportExpression::ImportSet::Type::RENAME:
                        ss << "RENAME\n";
                        break;
                    }
                    indent(ss, indentLevel + 2);
                    ss << "Library:\n";
                    for (auto& part : spec.library) {
                        part->ASTToString(ss, indentLevel + 3);
                    }
                    if (!spec.identifiers.empty()) {
                        indent(ss, indentLevel + 2);
                        ss << "Identifiers:\n";
                        for (auto& id : spec.identifiers) {
                            indent(ss, indentLevel + 3);
                            ss << id.token.lexeme;
                            // Print hygiene information if needed
                            if (!id.context.marks.empty()) {
                                ss << " [marks: ";
                                for (auto it = id.context.marks.begin(); it != id.context.marks.end(); ++it) {
                                    if (it != id.context.marks.begin()) {
                                        ss << ", ";
                                    }
                                    ss << *it;
                                }
                                ss << "]";
                            }
                            ss << "\n";
                        }
                    }
                    if (!spec.renames.empty()) {
                        indent(ss, indentLevel + 2);
                        ss << "Renames:\n";
                        for (auto& [old_name, new_name] : spec.renames) {
                            indent(ss, indentLevel + 3);
                            ss << "(" << old_name.token.lexeme << " -> " << new_name.token.lexeme << ")";
                            // Print hygiene information if needed for old_name
                            if (!old_name.context.marks.empty()) {
                                ss << " [old marks: ";
                                for (auto it = old_name.context.marks.begin(); it != old_name.context.marks.end(); ++it) {
                                    if (it != old_name.context.marks.begin()) {
                                        ss << ", ";
                                    }
                                    ss << *it;
                                }
                                ss << "]";
                            }
                            // Print hygiene information if needed for new_name
                            if (!new_name.context.marks.empty()) {
                                ss << " [new marks: ";
                                for (auto it = new_name.context.marks.begin(); it != new_name.context.marks.end(); ++it) {
                                    if (it != new_name.context.marks.begin()) {
                                        ss << ", ";
                                    }
                                    ss << *it;
                                }
                                ss << "]";
                            }
                            ss << "\n";
                        }
                    }
                }
            },
            [&](const SyntaxRulesExpression& e) {
                indent(ss, indentLevel);
                ss << "SyntaxRulesExpression:\n";
                indent(ss, indentLevel + 1);
                ss << "Literals:\n";
                for (auto& lit : e.literals) {
                    indent(ss, indentLevel + 2);
                    ss << lit.lexeme << "\n";
                }

                indent(ss, indentLevel + 1);
                ss << "Rules:\n";
                for (auto& rule : e.rules) {
                    indent(ss, indentLevel + 2);
                    ss << "Rule:\n";
                    indent(ss, indentLevel + 3);
                    ss << "Pattern:\n";
                    rule.pattern->ASTToString(ss, indentLevel + 4);
                    indent(ss, indentLevel + 3);
                    ss << "Template:\n";
                    rule.template_expr->ASTToString(ss, indentLevel + 4);
                }
            },
            [&](const DefineSyntaxExpression& e) {
                indent(ss, indentLevel);
                ss << "DefineSyntaxExpression: name=\"" << e.name.token.lexeme << "\"";
                // Print hygiene information if needed
                if (!e.name.context.marks.empty()) {
                    ss << " [marks: ";
                    for (auto it = e.name.context.marks.begin(); it != e.name.context.marks.end(); ++it) {
                        if (it != e.name.context.marks.begin()) {
                            ss << ", ";
                        }
                        ss << *it;
                    }
                    ss << "]";
                }
                ss << "\n";
                indent(ss, indentLevel + 1);
                ss << "Rule:\n";
                e.rule->ASTToString(ss, indentLevel + 2);
            },
            [&](const LetExpression& e) {
                indent(ss, indentLevel);
                ss << "LetExpression";
                if (e.name.has_value()) {
                    ss << " named=\"" << e.name->token.lexeme << "\"";
                    // Print hygiene information if needed
                    if (!e.name->context.marks.empty()) {
                        ss << " [marks: ";
                        for (auto it = e.name->context.marks.begin(); it != e.name->context.marks.end(); ++it) {
                            if (it != e.name->context.marks.begin()) {
                                ss << ", ";
                            }
                            ss << *it;
                        }
                        ss << "]";
                    }
                }
                ss << ":\n";
                if (!e.arguments.empty()) {
                    indent(ss, indentLevel + 1);
                    ss << "Bindings:\n";
                    for (auto& [syntax, boundExpr] : e.arguments) {
                        indent(ss, indentLevel + 2);
                        ss << syntax.token.lexeme;
                        // Print hygiene information if needed
                        if (!syntax.context.marks.empty()) {
                            ss << " [marks: ";
                            for (auto it = syntax.context.marks.begin(); it != syntax.context.marks.end(); ++it) {
                                if (it != syntax.context.marks.begin()) {
                                    ss << ", ";
                                }
                                ss << *it;
                            }
                            ss << "]";
                        }
                        ss << " =>\n";
                        boundExpr->ASTToString(ss, indentLevel + 3);
                    }
                }
                indent(ss, indentLevel + 1);
                ss << "Body:\n";
                for (auto& expr : e.body) {
                    expr->ASTToString(ss, indentLevel + 2);
                }
            } },
        as);
}

bool compareExpressionVectors(
    const std::vector<std::shared_ptr<Expression>>& lhs,
    const std::vector<std::shared_ptr<Expression>>& rhs)
{
    if (lhs.size() != rhs.size())
        return false;
    for (size_t i = 0; i < lhs.size(); ++i) {
        if (!lhs[i] && !rhs[i])
            continue;
        if (!lhs[i] || !rhs[i])
            return false;
        if (!(*lhs[i] == *rhs[i]))
            return false;
    }
    return true;
}

bool compareTokenVectors(const std::vector<Token>& lhs, const std::vector<Token>& rhs)
{
    if (lhs.size() != rhs.size())
        return false;
    for (size_t i = 0; i < lhs.size(); ++i) {
        if (lhs[i].type != rhs[i].type || lhs[i].lexeme != rhs[i].lexeme) {
            return false;
        }
    }
    return true;
}

bool compareTokens(const Token& lhs, const Token& rhs)
{
    return lhs.type == rhs.type && lhs.lexeme == rhs.lexeme;
}

// Updated for HygienicSyntax
bool compareHygienicSyntax(const HygienicSyntax& lhs, const HygienicSyntax& rhs)
{
    // Compare tokens first
    if (lhs.token.type != rhs.token.type || lhs.token.lexeme != rhs.token.lexeme) {
        return false;
    }

    // For now, just comparing lexemes without contexts for compatibility
    return true;
}

bool compareHygienicSyntaxVectors(const std::vector<HygienicSyntax>& lhs, const std::vector<HygienicSyntax>& rhs)
{
    if (lhs.size() != rhs.size())
        return false;
    for (size_t i = 0; i < lhs.size(); ++i) {
        if (!compareHygienicSyntax(lhs[i], rhs[i])) {
            return false;
        }
    }
    return true;
}
bool compareImportSpecVectors(const std::vector<ImportExpression::ImportSpec>& lhs, const std::vector<ImportExpression::ImportSpec>& rhs)
{
    if (lhs.size() != rhs.size())
        return false;
    for (size_t i = 0; i < lhs.size(); ++i) {
        if (lhs[i].type != rhs[i].type)
            return false;
    }
    return true;
}

bool operator==(const Expression& lhs, const Expression& rhs)
{

    if (lhs.as.index() != rhs.as.index()) {
        return false;
    }

    return std::visit(overloaded {
                          // --- Compare Specific Variants ---
                          [&](const AtomExpression& lhs_v) {
                              const auto& rhs_v = std::get<AtomExpression>(rhs.as);
                              return compareHygienicSyntax(lhs_v.value, rhs_v.value);
                          },
                          [&](const sExpression& lhs_v) {
                              const auto& rhs_v = std::get<sExpression>(rhs.as);
                              return compareExpressionVectors(lhs_v.elements, rhs_v.elements);
                          },
                          [&](const ListExpression& lhs_v) {
                              const auto& rhs_v = std::get<ListExpression>(rhs.as);
                              return compareExpressionVectors(lhs_v.elements, rhs_v.elements);
                          },
                          [&](const VectorExpression& lhs_v) {
                              const auto& rhs_v = std::get<VectorExpression>(rhs.as);
                              return compareExpressionVectors(lhs_v.elements, rhs_v.elements);
                          },
                          [&](const QuoteExpression& lhs_v) {
                              const auto& rhs_v = std::get<QuoteExpression>(rhs.as);
                              if (!lhs_v.expression && !rhs_v.expression)
                                  return true;
                              if (!lhs_v.expression || !rhs_v.expression)
                                  return false;
                              return *lhs_v.expression == *rhs_v.expression; // Recursive call
                          },
                          [&](const IfExpression& lhs_v) {
                              const auto& rhs_v = std::get<IfExpression>(rhs.as);
                              // Null checks needed before dereferencing shared_ptr
                              if (!lhs_v.condition || !rhs_v.condition || !(*lhs_v.condition == *rhs_v.condition))
                                  return false;
                              if (!lhs_v.then || !rhs_v.then || !(*lhs_v.then == *rhs_v.then))
                                  return false;
                              if (lhs_v.el.has_value() != rhs_v.el.has_value())
                                  return false;
                              if (lhs_v.el.has_value()) {
                                  if (!*lhs_v.el || !*rhs_v.el || !(**lhs_v.el == **rhs_v.el))
                                      return false;
                              }
                              return true;
                          },
                          [&](const DefineLibraryExpression& lhs_v) {
                              const auto& rhs_v = std::get<DefineLibraryExpression>(rhs.as);
                              if (!compareExpressionVectors(lhs_v.libraryName, rhs_v.libraryName))
                                  return false;
                              if (!compareHygienicSyntaxVectors(lhs_v.exports, rhs_v.exports))
                                  return false;
                              if (!compareImportSpecVectors(lhs_v.imports, rhs_v.imports))
                                  return false; // Needs proper implementation
                              if (!compareExpressionVectors(lhs_v.body, rhs_v.body))
                                  return false;
                              return true;
                          },
                          [&](const LambdaExpression& lhs_v) {
                              const auto& rhs_v = std::get<LambdaExpression>(rhs.as);
                              if (lhs_v.isVariadic != rhs_v.isVariadic)
                                  return false;
                              if (!compareHygienicSyntaxVectors(lhs_v.parameters, rhs_v.parameters))
                                  return false;
                              if (!compareExpressionVectors(lhs_v.body, rhs_v.body))
                                  return false;
                              return true;
                          },
                          [&](const DefineProcedure& lhs_v) {
                              const auto& rhs_v = std::get<DefineProcedure>(rhs.as);
                              if (!compareHygienicSyntax(lhs_v.name, rhs_v.name))
                                  return false;
                              if (lhs_v.isVariadic != rhs_v.isVariadic)
                                  return false;
                              if (!compareHygienicSyntaxVectors(lhs_v.parameters, rhs_v.parameters))
                                  return false;
                              if (!compareExpressionVectors(lhs_v.body, rhs_v.body))
                                  return false;
                              return true;
                          },
                          [&](const DefineExpression& lhs_v) {
                              const auto& rhs_v = std::get<DefineExpression>(rhs.as);
                              if (!compareHygienicSyntax(lhs_v.name, rhs_v.name))
                                  return false;
                              if (!lhs_v.value || !rhs_v.value || !(*lhs_v.value == *rhs_v.value))
                                  return false;
                              return true;
                          },
                          [&](const SetExpression& lhs_v) {
                              const auto& rhs_v = std::get<SetExpression>(rhs.as);
                              if (!compareHygienicSyntax(lhs_v.identifier, rhs_v.identifier))
                                  return false;
                              if (!lhs_v.value || !rhs_v.value || !(*lhs_v.value == *rhs_v.value))
                                  return false;
                              return true;
                          },
                          [&](const TailExpression& lhs_v) {
                              const auto& rhs_v = std::get<TailExpression>(rhs.as);
                              if (!lhs_v.expression && !rhs_v.expression)
                                  return true;
                              if (!lhs_v.expression || !rhs_v.expression)
                                  return false;
                              return *lhs_v.expression == *rhs_v.expression;
                          },
                          [&](const LetExpression& lhs_v) {
                              const auto& rhs_v = std::get<LetExpression>(rhs.as);
                              if (lhs_v.name.has_value() != rhs_v.name.has_value())
                                  return false;
                              if (lhs_v.name.has_value() && !compareHygienicSyntax(*lhs_v.name, *rhs_v.name))
                                  return false;
                              if (lhs_v.arguments.size() != rhs_v.arguments.size())
                                  return false;
                              for (size_t i = 0; i < lhs_v.arguments.size(); ++i) {
                                  if (!compareHygienicSyntax(lhs_v.arguments[i].first, rhs_v.arguments[i].first))
                                      return false;
                                  if (!lhs_v.arguments[i].second || !rhs_v.arguments[i].second || !(*lhs_v.arguments[i].second == *rhs_v.arguments[i].second))
                                      return false;
                              }
                              if (!compareExpressionVectors(lhs_v.body, rhs_v.body))
                                  return false;
                              return true;
                          },
                          [&](const QuasiQuoteExpression&) { return false; },

                          [&](const SyntaxRulesExpression& lhs_v) {
                              const auto& rhs_v = std::get<SyntaxRulesExpression>(rhs.as);
                              if (!compareTokenVectors(lhs_v.literals, rhs_v.literals))
                                  return false;
                              if (lhs_v.rules.size() != rhs_v.rules.size())
                                  return false;
                              for (size_t i = 0; i < lhs_v.rules.size(); ++i) {
                                  if (!(*lhs_v.rules[i].pattern == *rhs_v.rules[i].pattern))
                                      return false;
                                  if (!(*lhs_v.rules[i].template_expr == *rhs_v.rules[i].template_expr))
                                      return false;
                              }
                              return true;
                          },
                          [&](const DefineSyntaxExpression& lhs_v) {
                              const auto& rhs_v = std::get<DefineSyntaxExpression>(rhs.as);
                              if (!compareHygienicSyntax(lhs_v.name, rhs_v.name))
                                  return false;
                              if (!(*lhs_v.rule == *rhs_v.rule))
                                  return false;
                              return true;
                          },
                          [&](const ImportExpression& lhs_v) {
                              const auto& rhs_v = std::get<ImportExpression>(rhs.as);
                              if (lhs_v.imports.size() != rhs_v.imports.size())
                                  return false;
                              return false;
                          },
                          [&](const auto&) {
                              std::cerr << "Warning: operator== not fully implemented for Expression variant index " << lhs.as.index() << std::endl;
                              return false;
                          } },
        lhs.as);
}

bool operator!=(const Expression& lhs, const Expression& rhs)
{
    return !(lhs == rhs);
}
