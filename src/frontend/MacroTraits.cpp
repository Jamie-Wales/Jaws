#include "MacroTraits.h"
#include "Expression.h"
#include "Syntax.h"
#include "Token.h"
#include "parse.h"
#include "scan.h"
#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

namespace macroexp {

namespace {

    std::string formatMacroError(const std::string& message, const std::string& macroName, int line)
    {
        std::stringstream ss;
        if (!macroName.empty())
            ss << "in macro '" << macroName << "'";
        if (line >= 0)
            ss << (macroName.empty() ? "" : " ") << "near line " << line;
        if (ss.tellp() > 0)
            ss << ": ";
        ss << message;
        return ss.str();
    }

}

MacroError::MacroError(const std::string& message, const std::string& name, int ln)
    : std::runtime_error(formatMacroError(message, name, ln))
    , macroName(name)
    , line(ln)
{
}

MacroExpression::MacroExpression(const MacroAtom& atom, bool variadic, int lineNum)
    : value(atom)
    , isVariadic(variadic)
    , line(lineNum)
{
}

MacroExpression::MacroExpression(const MacroList& list, bool variadic, int lineNum)
    : value(list)
    , isVariadic(variadic)
    , line(lineNum)
{
}

std::string MacroExpression::toString() const
{
    return std::visit(overloaded {
                          [&](const MacroAtom& me) -> std::string {
                              if (isVariadic)
                                  return me.syntax.token.lexeme + "...";
                              return me.syntax.token.lexeme;
                          },
                          [&](const MacroList& me) -> std::string {
                              std::stringstream ss;
                              ss << "(";
                              for (size_t i = 0; i < me.elements.size(); i++) {
                                  if (i > 0)
                                      ss << " ";
                                  ss << me.elements[i]->toString();
                              }
                              ss << ")";
                              if (isVariadic)
                                  ss << "...";
                              return ss.str();
                          } },
        value);
}

void MacroExpression::print() const
{
    std::cout << toString() << std::endl;
}

const MatchTree* MatchEnvView::lookup(const std::string& name) const
{
    auto it = narrowed.find(name);
    if (it != narrowed.end())
        return it->second;
    return base->lookup(name);
}

MatchEnvView MatchEnvView::child(const std::vector<std::string>& drivers, size_t iteration) const
{
    MatchEnvView next = *this;
    for (const auto& name : drivers) {
        next.narrowed[name] = &lookup(name)->seq()[iteration];
    }
    return next;
}

bool isEllipsis(const MacroExprPtr& me)
{
    auto* atom = std::get_if<MacroAtom>(&me->value);
    return atom && atom->syntax.token.type == Tokentype::ELLIPSIS;
}

bool isPatternVariable(const Token& token, const std::vector<Token>& literals)
{
    if (token.lexeme == "_")
        return true;
    if (token.lexeme == ".")
        return false;

    bool isLiteral = std::any_of(literals.begin(), literals.end(),
        [&](const Token& lit) { return lit.lexeme == token.lexeme; });

    return !isLiteral && token.type == Tokentype::IDENTIFIER;
}

MacroExprPtr fromExpr(const std::shared_ptr<Expression>& expr)
{
    if (!expr)
        throw std::runtime_error("Cannot generate MacroExpression from nullptr");

    const auto toProcess = ::exprToList(expr);
    return std::visit(overloaded {
                          [&](const AtomExpression& atom) -> MacroExprPtr {
                              return std::make_shared<MacroExpression>(MacroAtom { atom.value }, false, expr->line);
                          },
                          [&](const ListExpression& list) -> MacroExprPtr {
                              std::vector<MacroExprPtr> processed;
                              for (const auto& element : list.elements) {
                                  auto elem = fromExpr(element);
                                  if (!elem)
                                      continue;

                                  if (isEllipsis(elem) && !processed.empty()) {
                                      processed.back()->isVariadic = true;
                                      continue;
                                  }
                                  processed.push_back(elem);
                              }
                              return std::make_shared<MacroExpression>(MacroList { std::move(processed) }, false, expr->line);
                          },
                          [&](const VectorExpression& vec) -> MacroExprPtr {
                              std::vector<MacroExprPtr> processed;
                              for (const auto& el : vec.elements) {
                                  processed.push_back(fromExpr(el));
                              }
                              return std::make_shared<MacroExpression>(MacroList { std::move(processed) }, false, expr->line);
                          },
                          [&](const auto&) -> MacroExprPtr {
                              throw std::runtime_error("Unsupported expression type in fromExpr: " + expr->toString());
                          } },
        toProcess->as);
}

namespace {

    void findPatternVariables(
        const MacroExprPtr& pattern,
        std::vector<std::string>& vars,
        const std::vector<Token>& literals,
        std::set<std::string>& visited)
    {
        if (!pattern)
            return;

        std::visit(overloaded {
                       [&](const MacroAtom& atom) {
                           if (isPatternVariable(atom.syntax.token, literals) && atom.syntax.token.lexeme != "_") {
                               if (visited.insert(atom.syntax.token.lexeme).second) {
                                   vars.push_back(atom.syntax.token.lexeme);
                               }
                           }
                       },
                       [&](const MacroList& list) {
                           for (const auto& elem : list.elements) {
                               findPatternVariables(elem, vars, literals, visited);
                           }
                       } },
            pattern->value);
    }

}

void findPatternVariables(
    const MacroExprPtr& pattern,
    std::vector<std::string>& vars,
    const std::vector<Token>& literals)
{
    std::set<std::string> visited;
    findPatternVariables(pattern, vars, literals, visited);
}

MacroExprPtr createNonVariadicCopy(const MacroExprPtr& expr)
{
    return std::visit([&](const auto& value) {
        return std::make_shared<MacroExpression>(value, false, expr->line);
    },
        expr->value);
}

namespace {

    bool matchInto(
        const MacroExprPtr& pattern,
        const MacroExprPtr& form,
        const std::vector<Token>& literals,
        MatchEnv& out);

    // Matches (p1 ... pk  pe ...  q1 ... qn) against a list form. The ellipsis
    // length is fixed by the number of trailing patterns, so no backtracking
    // is needed, but every repetition must match pe.
    bool matchListInto(
        const MacroList& pat,
        const MacroExprPtr& patNode,
        const MacroList& form,
        const std::vector<Token>& literals,
        MatchEnv& out)
    {
        std::optional<size_t> ellipsisIndex;
        for (size_t i = 0; i < pat.elements.size(); ++i) {
            if (pat.elements[i]->isVariadic) {
                if (ellipsisIndex)
                    throw MacroError("pattern has more than one ellipsis in the same list", "", patNode->line);
                ellipsisIndex = i;
            }
        }

        if (!ellipsisIndex) {
            if (pat.elements.size() != form.elements.size())
                return false;
            for (size_t i = 0; i < pat.elements.size(); ++i) {
                if (!matchInto(pat.elements[i], form.elements[i], literals, out))
                    return false;
            }
            return true;
        }

        const size_t before = *ellipsisIndex;
        const size_t after = pat.elements.size() - before - 1;
        if (form.elements.size() < before + after)
            return false;
        const size_t repetitions = form.elements.size() - before - after;

        for (size_t i = 0; i < before; ++i) {
            if (!matchInto(pat.elements[i], form.elements[i], literals, out))
                return false;
        }
        for (size_t i = 0; i < after; ++i) {
            if (!matchInto(pat.elements[before + 1 + i], form.elements[before + repetitions + i], literals, out))
                return false;
        }

        const auto repeated = createNonVariadicCopy(pat.elements[before]);
        std::vector<MatchEnv> iterations(repetitions);
        for (size_t i = 0; i < repetitions; ++i) {
            if (!matchInto(repeated, form.elements[before + i], literals, iterations[i]))
                return false;
        }

        // Every variable under the ellipsis gets a Seq, including when nothing
        // repeated, so zero matches is distinguishable from unbound.
        std::vector<std::string> vars;
        findPatternVariables(repeated, vars, literals);
        for (const auto& name : vars) {
            MatchTree::Seq seq;
            seq.reserve(repetitions);
            for (const auto& iteration : iterations) {
                seq.push_back(*iteration.lookup(name));
            }
            out.bind(name, MatchTree::seqOf(std::move(seq)));
        }
        return true;
    }

    bool matchInto(
        const MacroExprPtr& pattern,
        const MacroExprPtr& form,
        const std::vector<Token>& literals,
        MatchEnv& out)
    {
        if (!pattern || !form)
            return false;

        auto bindVariable = [&](const MacroAtom& patAtom) {
            if (patAtom.syntax.token.lexeme != "_")
                out.bind(patAtom.syntax.token.lexeme, MatchTree::leafOf(createNonVariadicCopy(form)));
            return true;
        };

        return visit_many(multi_visitor {
                              [&](const MacroAtom& patAtom, const MacroAtom& formAtom) -> bool {
                                  if (isPatternVariable(patAtom.syntax.token, literals))
                                      return bindVariable(patAtom);
                                  return patAtom.syntax.token.lexeme == formAtom.syntax.token.lexeme;
                              },
                              [&](const MacroAtom& patAtom, const MacroList&) -> bool {
                                  if (isPatternVariable(patAtom.syntax.token, literals))
                                      return bindVariable(patAtom);
                                  return false;
                              },
                              [&](const MacroList& patList, const MacroList& formList) -> bool {
                                  return matchListInto(patList, pattern, formList, literals, out);
                              },
                              [&](const MacroList&, const MacroAtom&) -> bool {
                                  return false;
                              } },
            pattern->value, form->value);
    }

    int ellipsisDepth(const PatternVariableInfo& info, const std::string& name)
    {
        auto it = info.ellipsis_depth.find(name);
        return it == info.ellipsis_depth.end() ? 0 : it->second;
    }

    // The pattern variables an ellipsis at `depth` iterates over: those in the
    // sub-template whose pattern depth is deeper than the current iteration.
    void collectDrivers(
        const MacroExprPtr& templateExpr,
        const MatchEnvView& env,
        const PatternVariableInfo& info,
        int depth,
        std::vector<std::string>& drivers)
    {
        std::visit(overloaded {
                       [&](const MacroAtom& atom) {
                           const auto& name = atom.syntax.token.lexeme;
                           if (env.lookup(name) && ellipsisDepth(info, name) > depth
                               && std::find(drivers.begin(), drivers.end(), name) == drivers.end()) {
                               drivers.push_back(name);
                           }
                       },
                       [&](const MacroList& list) {
                           for (const auto& elem : list.elements) {
                               collectDrivers(elem, env, info, depth, drivers);
                           }
                       } },
            templateExpr->value);
    }

    void expandEllipsis(
        const MacroExprPtr& element,
        const MatchEnvView& env,
        const PatternVariableInfo& info,
        const SyntaxContext& macroScope,
        int depth,
        std::vector<MacroExprPtr>& out)
    {
        const auto repeated = createNonVariadicCopy(element);
        std::vector<std::string> drivers;
        collectDrivers(repeated, env, info, depth, drivers);

        if (drivers.empty())
            throw MacroError("ellipsis in template has no pattern variable to iterate over: " + element->toString(), "", element->line);

        const size_t count = env.lookup(drivers[0])->seq().size();
        for (const auto& name : drivers) {
            if (env.lookup(name)->seq().size() != count)
                throw MacroError("pattern variables '" + drivers[0] + "' and '" + name + "' matched different numbers of forms", "", element->line);
        }

        for (size_t i = 0; i < count; ++i) {
            out.push_back(expandTemplate(repeated, env.child(drivers, i), info, macroScope, depth + 1));
        }
    }

}

std::optional<MatchEnv> matchPattern(
    const MacroExprPtr& pattern,
    const MacroExprPtr& form,
    const std::vector<Token>& literals)
{
    MatchEnv env;
    if (!matchInto(pattern, form, literals, env))
        return std::nullopt;
    return env;
}

MacroExprPtr expandTemplate(
    const MacroExprPtr& templateExpr,
    const MatchEnvView& env,
    const PatternVariableInfo& patternInfo,
    const SyntaxContext& macroScope,
    int depth)
{
    if (!templateExpr)
        return nullptr;

    return std::visit(overloaded {
                          [&](const MacroAtom& atom) -> MacroExprPtr {
                              const auto& name = atom.syntax.token.lexeme;
                              if (const MatchTree* match = env.lookup(name)) {
                                  if (!match->isLeaf())
                                      throw MacroError("pattern variable '" + name + "' is used with too few ellipses in the template", "", templateExpr->line);
                                  return match->leaf();
                              }

                              if (atom.syntax.token.type == Tokentype::IDENTIFIER) {
                                  HygienicSyntax introduced { atom.syntax.token, atom.syntax.context.addMarks(macroScope.marks) };
                                  return std::make_shared<MacroExpression>(MacroAtom { introduced }, templateExpr->isVariadic, templateExpr->line);
                              }
                              return std::make_shared<MacroExpression>(atom, templateExpr->isVariadic, templateExpr->line);
                          },
                          [&](const MacroList& list) -> MacroExprPtr {
                              std::vector<MacroExprPtr> elements;
                              elements.reserve(list.elements.size());
                              for (const auto& element : list.elements) {
                                  if (element->isVariadic)
                                      expandEllipsis(element, env, patternInfo, macroScope, depth, elements);
                                  else
                                      elements.push_back(expandTemplate(element, env, patternInfo, macroScope, depth));
                              }
                              return std::make_shared<MacroExpression>(MacroList { std::move(elements) }, false, templateExpr->line);
                          } },
        templateExpr->value);
}

HygienicSyntax createFreshSyntaxObject(const Token& token, SyntaxContext context)
{
    return HygienicSyntax { token, context };
}

namespace {

    constexpr int kMaxExpansionSteps = 1000;
    constexpr int kMaxNestingDepth = 5000;

    // The identifier in macro-call position: the atom itself, or a list's head.
    std::string macroCallName(const MacroExprPtr& node)
    {
        if (auto* atom = std::get_if<MacroAtom>(&node->value))
            return atom->syntax.token.lexeme;
        if (auto* list = std::get_if<MacroList>(&node->value); list && !list->elements.empty()) {
            if (auto* head = std::get_if<MacroAtom>(&list->elements[0]->value))
                return head->syntax.token.lexeme;
        }
        return "";
    }

    std::optional<MacroExprPtr> tryExpandOnce(
        const MacroExprPtr& node,
        const std::shared_ptr<pattern::MacroEnvironment>& env)
    {
        const std::string name = macroCallName(node);
        if (name.empty())
            return std::nullopt;

        auto definition = env->getMacroDefinition(name);
        if (!definition || !*definition || !std::holds_alternative<SyntaxRulesExpression>((*definition)->as))
            return std::nullopt;

        const auto& syntaxRules = std::get<SyntaxRulesExpression>((*definition)->as);
        const SyntaxContext macroScope = SyntaxContext::createFresh();

        for (const auto& rule : syntaxRules.rules) {
            if (auto bindings = matchPattern(fromExpr(rule.pattern), node, syntaxRules.literals)) {
                try {
                    return expandTemplate(fromExpr(rule.template_expr), MatchEnvView { *bindings }, rule.pattern_info, macroScope);
                } catch (const MacroError& e) {
                    throw MacroError(e.what(), name, node->line);
                }
            }
        }
        // A bare keyword (e.g. one shadowed by a local binding) is left alone;
        // a call that matches no rule is a syntax error.
        if (std::holds_alternative<MacroAtom>(node->value))
            return std::nullopt;
        throw MacroError("no syntax-rules pattern matches " + node->toString(), name, node->line);
    }

}

// A node is expanded to a fixed point, then its children once each. That is
// enough: a syntax-rules call is identified by its head identifier, and
// expanding children cannot turn a non-macro head into a macro one.
MacroExprPtr expandFully(
    const MacroExprPtr& node,
    const std::shared_ptr<pattern::MacroEnvironment>& env,
    int depth)
{
    if (!node || !env)
        return node;
    if (depth > kMaxNestingDepth)
        throw MacroError("expansion nested too deeply", macroCallName(node), node->line);

    auto current = node;
    for (int step = 0;; ++step) {
        if (step > kMaxExpansionSteps)
            throw MacroError("expansion did not terminate", macroCallName(current), current->line);
        auto next = tryExpandOnce(current, env);
        if (!next)
            break;
        current = *next;
    }

    auto* list = std::get_if<MacroList>(&current->value);
    if (list && macroCallName(current) != "quote") {
        std::vector<MacroExprPtr> elements;
        elements.reserve(list->elements.size());
        bool changed = false;
        for (const auto& element : list->elements) {
            auto expanded = expandFully(element, env, depth + 1);
            changed = changed || expanded != element;
            elements.push_back(std::move(expanded));
        }
        if (changed)
            current = std::make_shared<MacroExpression>(MacroList { std::move(elements) }, current->isVariadic, current->line);
    }
    return current;
}

std::shared_ptr<Expression> convertBegin(const macroexp::MacroList& ml, int line)
{
    std::vector<std::shared_ptr<Expression>> values = {};
    values.reserve(ml.elements.size() > 0 ? ml.elements.size() - 1 : 0);
    for (size_t i = 1; i < ml.elements.size(); i++) { // Use size_t
        auto datum = convertMacroResultToExpressionInternal(ml.elements[i]);
        if (!datum) {
            throw std::runtime_error("Invalid expression in 'begin' body during conversion");
        }
        values.push_back(datum);
    }

    wrapLastBodyExpression(values);
    return std::make_shared<Expression>(BeginExpression { std::move(values) }, line);
}

std::shared_ptr<Expression> convertMacroResultToExpressionInternal(
    const std::shared_ptr<MacroExpression>& macroResult)
{
    if (!macroResult)
        return nullptr;
    int line = macroResult->line;

    return std::visit(overloaded {
                          [&](const MacroAtom& ma) -> std::shared_ptr<Expression> {
                              return std::make_shared<Expression>(AtomExpression { ma.syntax }, line);
                          },

                          [&](const MacroList& ml) -> std::shared_ptr<Expression> {
                              if (ml.elements.empty()) {
                                  return std::make_shared<Expression>(sExpression { {} }, line);
                              }

                              std::string keyword = getKeyword(ml);
                              if (keyword == "quote") {
                                  return convertQuote(ml, line);
                              } else if (keyword == "quasiquote") {
                                  return convertQuasiQuote(ml, line);
                              } else if (keyword == "begin") {
                                  return convertBegin(ml, line);
                              } else if (keyword == "unquote") {
                                  return convertUnquote(ml, line);
                              } else if (keyword == "unquote-splice") {
                                  return convertSplice(ml, line);
                              } else if (keyword == "set!") {
                                  return convertSet(ml, line);
                              } else if (keyword == "if") {
                                  return convertIf(ml, line);
                              } else if (keyword == "let") {
                                  return convertLet(ml, line);
                              } else if (keyword == "lambda") {
                                  return convertLambda(ml, line);
                              } else if (keyword == "define") {
                                  return convertDefine(ml, line);
                              } else if (keyword == "#") {
                                  return convertVector(ml, line);
                              } else {
                                  std::vector<std::shared_ptr<Expression>> convertedElements;
                                  convertedElements.reserve(ml.elements.size());
                                  for (const auto& elem : ml.elements) {
                                      if (auto convertedElem = convertMacroResultToExpressionInternal(elem)) {
                                          convertedElements.push_back(convertedElem);
                                      } else {
                                          throw std::runtime_error("Null expression encountered during sExpression conversion");
                                      }
                                  }
                                  return std::make_shared<Expression>(sExpression { std::move(convertedElements) }, line);
                              }
                          },

                          [&](const auto& other) -> std::shared_ptr<Expression> {
                              throw std::runtime_error("Internal Error: Unexpected variant type found within MacroExpression");
                          } },
        macroResult->value);
}

std::string getKeyword(const MacroList& ml)
{
    if (ml.elements.empty())
        return "";
    const auto& firstElement = ml.elements[0];
    if (!firstElement)
        return "";

    if (const MacroAtom* atom = std::get_if<MacroAtom>(&firstElement->value)) {
        return atom->syntax.token.lexeme;
    }
    return "";
}

std::shared_ptr<Expression> convertQuote(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(QuoteExpression { datum }, line);
}

std::shared_ptr<Expression> convertQuasiQuote(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(QuasiQuoteExpression { datum }, line);
}

std::shared_ptr<Expression> convertUnquote(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(UnquoteExpression { datum }, line);
}

std::shared_ptr<Expression> convertSplice(const MacroList& ml, int line)
{
    if (ml.elements.size() != 2)
        throw std::runtime_error("Invalid quote structure during conversion");
    auto datum = convertMacroResultToExpressionInternal(ml.elements[1]);
    if (!datum)
        throw std::runtime_error("Invalid quote datum during conversion");
    return std::make_shared<Expression>(SpliceExpression { datum }, line);
}

std::shared_ptr<Expression> convertSet(const MacroList& ml, int line)
{
    if (ml.elements.size() != 3)
        throw std::runtime_error("Invalid set! structure during conversion");
    auto identExpr = convertMacroResultToExpressionInternal(ml.elements[1]);
    auto valueExpr = convertMacroResultToExpressionInternal(ml.elements[2]);
    if (!identExpr || !valueExpr || !std::holds_alternative<AtomExpression>(identExpr->as)) {
        throw std::runtime_error("Invalid set! parts during conversion (expected identifier and value)");
    }
    HygienicSyntax identifier = std::get<AtomExpression>(identExpr->as).value;
    return std::make_shared<Expression>(SetExpression { identifier, valueExpr }, line);
}

std::shared_ptr<Expression> convertIf(const MacroList& ml, int line)
{
    if (ml.elements.size() < 3 || ml.elements.size() > 4)
        throw std::runtime_error("Invalid if structure during conversion");
    auto condition = convertMacroResultToExpressionInternal(ml.elements[1]);
    auto thenBranch = convertMacroResultToExpressionInternal(ml.elements[2]);
    std::optional<std::shared_ptr<Expression>> elseBranchOpt = std::nullopt;

    if (!condition || !thenBranch)
        throw std::runtime_error("Invalid if parts during conversion");
    thenBranch = std::make_shared<Expression>(TailExpression { thenBranch }, thenBranch->line);

    if (ml.elements.size() == 4) {
        auto elseConv = convertMacroResultToExpressionInternal(ml.elements[3]);
        if (!elseConv)
            throw std::runtime_error("Invalid if else part during conversion");
        elseBranchOpt = std::make_shared<Expression>(TailExpression { elseConv }, elseConv->line);
    }
    return std::make_shared<Expression>(IfExpression { condition, thenBranch, elseBranchOpt }, line);
}

std::pair<std::vector<HygienicSyntax>, bool> parseMacroParameters(
    const std::shared_ptr<MacroExpression>& paramsMacroExpr)
{
    std::vector<HygienicSyntax> params;
    bool isVariadic = false;

    if (!paramsMacroExpr) {
        throw std::runtime_error("Invalid null parameter list structure during conversion");
    }

    if (const MacroList* paramList = std::get_if<MacroList>(&paramsMacroExpr->value)) {
        for (size_t i = 0; i < paramList->elements.size(); ++i) {
            const auto& paramNode = paramList->elements[i];
            if (!paramNode)
                throw std::runtime_error("Null parameter node during conversion");

            if (const MacroAtom* paramAtom = std::get_if<MacroAtom>(&paramNode->value)) {
                if (paramAtom->syntax.token.type == Tokentype::DOT) {
                    if (isVariadic)
                        throw std::runtime_error("Multiple dots found in params");
                    isVariadic = true;
                    continue;
                }
                if (isVariadic) {
                    params.push_back(paramAtom->syntax);
                    if (i != paramList->elements.size() - 1)
                        throw std::runtime_error("More tokens found after dot parameter");
                    break;
                }
                params.push_back(paramAtom->syntax);
            } else {
                throw std::runtime_error("Non-atom found in parameter list structure during conversion");
            }
        }

    } else if (const MacroAtom* paramAtom = std::get_if<MacroAtom>(&paramsMacroExpr->value)) {
        params.push_back(paramAtom->syntax);
        isVariadic = false;
    } else {
        throw std::runtime_error("Unsupported parameter structure during conversion (neither list nor atom)");
    }
    return { params, isVariadic };
}

std::shared_ptr<Expression> convertVector(const MacroList& ml, int line)
{
    std::vector<std::shared_ptr<Expression>> body;
    body.reserve(ml.elements.size() - 1);
    for (size_t i = 1; i < ml.elements.size(); ++i) {
        if (auto converted = convertMacroResultToExpressionInternal(ml.elements[i])) {
            body.push_back(converted);
        } else {
            throw std::runtime_error("Invalid lambda body element during conversion");
        }
    }
    wrapLastBodyExpression(body);
    return std::make_shared<Expression>(VectorExpression { std::move(body) }, line);
}

std::shared_ptr<Expression> convertLambda(const MacroList& ml, int line)
{
    if (ml.elements.size() < 2)
        throw std::runtime_error("Invalid lambda structure (needs params)");
    auto [params, isVariadic] = parseMacroParameters(ml.elements[1]);

    std::vector<std::shared_ptr<Expression>> body;
    body.reserve(ml.elements.size() - 2);
    for (size_t i = 2; i < ml.elements.size(); ++i) {
        if (auto converted = convertMacroResultToExpressionInternal(ml.elements[i])) {
            body.push_back(converted);
        } else {
            throw std::runtime_error("Invalid lambda body element during conversion");
        }
    }
    wrapLastBodyExpression(body);

    return std::make_shared<Expression>(LambdaExpression { params, std::move(body), isVariadic }, line);
}

std::shared_ptr<Expression> convertDefine(const MacroList& ml, int line)
{
    if (ml.elements.size() < 3)
        throw std::runtime_error("Invalid define structure during conversion");

    if (ml.elements[1] && std::holds_alternative<MacroList>(ml.elements[1]->value)) {
        const auto& procHeaderNode = ml.elements[1];
        const auto& procHeaderList = std::get<MacroList>(procHeaderNode->value);

        if (procHeaderList.elements.empty())
            throw std::runtime_error("Empty define procedure header");

        HygienicSyntax name;
        if (procHeaderList.elements[0] && std::holds_alternative<MacroAtom>(procHeaderList.elements[0]->value)) {
            name = std::get<MacroAtom>(procHeaderList.elements[0]->value).syntax;
        } else {
            throw std::runtime_error("Expected procedure name atom in define");
        }

        auto paramsOnlyList = std::make_shared<MacroList>();
        paramsOnlyList->elements.assign(procHeaderList.elements.begin() + 1, procHeaderList.elements.end());
        int paramsLine = procHeaderNode->line;
        auto paramsMacroExpr = std::make_shared<MacroExpression>(*paramsOnlyList, false, paramsLine);

        auto [params, isVariadic] = parseMacroParameters(paramsMacroExpr);

        std::vector<std::shared_ptr<Expression>> body;
        body.reserve(ml.elements.size() - 2);
        for (size_t i = 2; i < ml.elements.size(); ++i) {
            if (auto converted = convertMacroResultToExpressionInternal(ml.elements[i])) {
                body.push_back(converted);
            } else {
                throw std::runtime_error("Invalid define body element during conversion");
            }
        }
        if (body.empty())
            throw std::runtime_error("Define procedure requires a body");
        wrapLastBodyExpression(body);

        return std::make_shared<Expression>(DefineProcedure { name, params, std::move(body), isVariadic }, line);

    } else if (ml.elements[1] && std::holds_alternative<MacroAtom>(ml.elements[1]->value)) {
        if (ml.elements.size() != 3)
            throw std::runtime_error("Invalid define variable structure");
        HygienicSyntax name = std::get<MacroAtom>(ml.elements[1]->value).syntax;
        auto value = convertMacroResultToExpressionInternal(ml.elements[2]);
        if (!value)
            throw std::runtime_error("Invalid define value part during conversion");
        return std::make_shared<Expression>(DefineExpression { name, value }, line);
    } else {
        throw std::runtime_error("Invalid structure after define keyword during conversion");
    }
}

std::shared_ptr<Expression> convertLet(const MacroList& ml, int line)
{
    if (ml.elements.size() < 2) {
        throw std::runtime_error("Invalid let structure during conversion (too few parts)");
    }

    std::optional<HygienicSyntax> letNameSyntax = std::nullopt;
    size_t bindingsIndex = 0;
    size_t bodyStartIndex = 0;

    const auto& secondElementNode = ml.elements[1];
    if (!secondElementNode)
        throw std::runtime_error("Internal error: null node after let");

    if (const MacroAtom* nameAtom = std::get_if<MacroAtom>(&secondElementNode->value)) {
        letNameSyntax = nameAtom->syntax;
        bindingsIndex = 2;
        bodyStartIndex = 3;
        if (ml.elements.size() < 3) {
            throw std::runtime_error("Invalid named let structure (missing bindings list)");
        }
    } else if (std::holds_alternative<MacroList>(secondElementNode->value)) {
        letNameSyntax = std::nullopt;
        bindingsIndex = 1;
        bodyStartIndex = 2;
    } else {
        throw std::runtime_error("Invalid let structure: expected identifier or bindings list after 'let'");
    }

    if (bindingsIndex >= ml.elements.size()) {
        throw std::runtime_error("Missing bindings list in let structure");
    }
    const auto& bindingsNode = ml.elements[bindingsIndex];
    if (!bindingsNode || !std::holds_alternative<MacroList>(bindingsNode->value)) {
        throw std::runtime_error("Expected list structure for let bindings");
    }
    const auto& bindingsList = std::get<MacroList>(bindingsNode->value);

    std::vector<std::pair<HygienicSyntax, std::shared_ptr<Expression>>> bindings;
    bindings.reserve(bindingsList.elements.size());
    for (const auto& bindingPairNode : bindingsList.elements) {
        if (!bindingPairNode || !std::holds_alternative<MacroList>(bindingPairNode->value)) {
            throw std::runtime_error("Expected list for let binding pair");
        }
        const auto& bindingPairList = std::get<MacroList>(bindingPairNode->value);
        if (bindingPairList.elements.size() != 2) {
            throw std::runtime_error("Let binding pair must have exactly size 2 (variable value)");
        }

        if (!bindingPairList.elements[0] || !std::holds_alternative<MacroAtom>(bindingPairList.elements[0]->value)) {
            throw std::runtime_error("Expected identifier atom for let binding variable");
        }
        HygienicSyntax varSyntax = std::get<MacroAtom>(bindingPairList.elements[0]->value).syntax;
        auto valExpr = convertMacroResultToExpressionInternal(bindingPairList.elements[1]);
        if (!valExpr)
            throw std::runtime_error("Invalid let binding value during conversion");

        bindings.push_back({ varSyntax, valExpr });
    }

    std::vector<std::shared_ptr<Expression>> body;
    body.reserve(ml.elements.size() - bodyStartIndex);
    for (size_t i = bodyStartIndex; i < ml.elements.size(); ++i) {
        if (auto converted = convertMacroResultToExpressionInternal(ml.elements[i])) {
            body.push_back(converted);
        } else {
            throw std::runtime_error("Invalid let body element during conversion");
        }
    }
    wrapLastBodyExpression(body);
    return std::make_shared<Expression>(LetExpression { letNameSyntax, std::move(bindings), std::move(body) }, line);
}

void wrapLastBodyExpression(std::vector<std::shared_ptr<Expression>>& body)
{
    if (!body.empty() && body.back()) {
        if (!std::holds_alternative<TailExpression>(body.back()->as)) {
            body.back() = std::make_shared<Expression>(TailExpression { body.back() }, body.back()->line);
        }
    }
}

std::shared_ptr<Expression> convertMacroResultToExpression(const MacroExprPtr& macroResult)
{
    try {
        return convertMacroResultToExpressionInternal(macroResult);
    } catch (const MacroError&) {
        throw;
    } catch (const std::exception& e) {
        throw MacroError(e.what(), "", macroResult ? macroResult->line : -1);
    }
}

std::vector<std::shared_ptr<Expression>> expandMacros(
    const std::vector<std::shared_ptr<Expression>>& exprs,
    std::shared_ptr<pattern::MacroEnvironment> env)
{
    std::vector<std::shared_ptr<Expression>> expanded;
    auto macroEnv = env ? env : std::make_shared<pattern::MacroEnvironment>();

    for (const auto& expr : exprs) {
        if (std::holds_alternative<DefineSyntaxExpression>(expr->as))
            continue;
        expanded.push_back(convertMacroResultToExpression(expandFully(fromExpr(expr), macroEnv)));
    }
    return expanded;
}
}
