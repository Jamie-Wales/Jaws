#include "Interpreter.h"
#include "Error.h"
#include "Expression.h"
#include "Parser.h"
#include "Procedure.h"
#include "Scanner.h"
#include "Value.h"
#include "run.h"
#include <memory>
#include <optional>
#include <variant>

void Interpreter::init()
{
    auto exprs = p->parse();
    if (exprs) {
        run(*exprs);
    } else {
        throw InterpreterError("Parsing failed");
    }
}

Interpreter::Interpreter(std::shared_ptr<Scanner> s, std::shared_ptr<Parser> p)
    : scope(std::make_shared<Environment>())
    , s { s }
    , p { p }
    , macroExpander { *this }
{
    auto define = [this](const std::string& name, BuiltInProcedure::Func func) {
        scope->define(name, SchemeValue(std::make_shared<BuiltInProcedure>(func)));
    };

    define("+", plus);
    define("-", minus);
    define("*", mult);
    define("/", div);
    define("<=", lessOrEqual);
    define(">=", greaterOrEqual);
    define("<", less);
    define(">", greater);

    // Equality
    auto eq = std::make_shared<BuiltInProcedure>(equal);
    scope->define("=", SchemeValue(eq));
    scope->define("eq?", SchemeValue(eq));
    define("boolean?", isBooleanProc);
    define("open-input-file", openInputFile);
    define("open-output-file", openOutputFile);
    define("close-port", closePort);
    define("read", read);
    define("write", write);
    define("display", display);
    define("newline", newline);
    define("eval", eval);
    define("help", printHelp);
    define("map", map);
    define("list", listProcedure);
    define("car", carProcudure);
    define("cdr", cdrProcedure);
    define("cadr", cadrProcedure);
    define("cons", cons);
    define("length", length);
    define("append", append);
    define("reverse", reverse);
    define("list-ref", listRef);
    define("list-tail", listTail);
    define("make-vector", makeVector);
    define("vector", vectorProcedure);
    define("vector-ref", vectorRef);
    define("vector-set!", vectorSet);
    define("vector-length", vectorLength);
}
std::optional<SchemeValue> Interpreter::interpretQuoteExpression(const QuoteExpression& qe, const Expression& e)
{
    return SchemeValue(qe.expression);
}

std::optional<SchemeValue> Interpreter::interpretAtom(const AtomExpression& atom, const Expression& expr)
{
    const Token& token = atom.value;
    try {
        switch (token.type) {
        case Tokentype::INTEGER:
            return SchemeValue(Number(std::stoi(token.lexeme)));
        case Tokentype::FLOAT:
            return SchemeValue(Number(std::stod(token.lexeme)));
        case Tokentype::COMPLEX: {
            std::string complexStr = token.lexeme.substr(0, token.lexeme.length() - 1);
            size_t plusPos = complexStr.find('+', 1);
            size_t minusPos = complexStr.find('-', 1);
            size_t separatorPos = std::min(plusPos, minusPos);

            if (separatorPos == std::string::npos) {
                return SchemeValue(Number(std::complex<double>(0, std::stod(complexStr))));
            } else {
                double real = std::stod(complexStr.substr(0, separatorPos));
                double imag = std::stod(complexStr.substr(separatorPos));
                return SchemeValue(Number(std::complex<double>(real, imag)));
            }
        }
        case Tokentype::RATIONAL: {
            size_t slashPos = token.lexeme.find('/');
            if (slashPos != std::string::npos) {
                int numerator = std::stoi(token.lexeme.substr(0, slashPos));
                int denominator = std::stoi(token.lexeme.substr(slashPos + 1));
                return SchemeValue(Number(Number::Rational(numerator, denominator)));
            }
            throw InterpreterError("Invalid rational number format", expr);
        }
        case Tokentype::STRING:
            return SchemeValue(token.lexeme.substr(1, token.lexeme.length() - 2));
        case Tokentype::TRUE:
            return SchemeValue(true);
        case Tokentype::FALSE:
            return SchemeValue(false);
        case Tokentype::SYMBOL:
        case Tokentype::IDENTIFIER:
            if (auto value = scope->get(token.lexeme)) {
                return value;
            }
            throw InterpreterError("Undefined variable: " + token.lexeme, expr);
        default:
            expr.print();
            throw InterpreterError("Unexpected token type in atom");
        }
    } catch (const std::exception& e) {
        throw InterpreterError(e.what(), expr);
    }
}

std::optional<SchemeValue> Interpreter::interpretList(const ListExpression& list, const Expression& expr)
{
    std::list<SchemeValue> elements;
    for (const auto& ele : list.elements) {
        std::optional<SchemeValue> item = interpret(ele);
        if (!item) {
            return std::nullopt;
        }
        elements.push_back(*item);
    }
    return SchemeValue(std::move(elements));
}

std::optional<SchemeValue> Interpreter::interpretSExpression(const sExpression& se, const Expression& expr)
{
    if (se.elements.empty()) {
        throw InterpreterError("Empty procedure call", expr);
    }
    if (auto atom = std::get_if<AtomExpression>(&se.elements[0]->as)) {
        if (macroExpander.isMacro(atom->value.lexeme)) {
            std::vector<std::shared_ptr<Expression>> args(
                se.elements.begin() + 1,
                se.elements.end());

            if (auto expanded = macroExpander.expand(atom->value.lexeme, args)) {
                return interpret(*expanded);
            }
            throw InterpreterError("Macro expansion failed", expr);
        }
    }

    std::optional<SchemeValue> proc = interpret(se.elements[0]);
    if (proc) {
        std::vector<SchemeValue> args;
        args.reserve(se.elements.size() - 1);
        for (size_t i = 1; i < se.elements.size(); ++i) {
            auto ele = interpret(se.elements[i]);
            if (ele) {
                args.push_back(*ele);
            } else {
                return std::nullopt;
            }
        }
        auto ele = proc->call(*this, args);
        if (ele) {
            return *ele;
        } else {
            return std::nullopt;
        }
    }

    throw InterpreterError("Cannot interpret sExpression", expr);
}

std::optional<SchemeValue> Interpreter::defineExpression(const DefineExpression& de, const Expression& expr)
{
    std::optional<SchemeValue> exprValue = interpret(de.value);
    if (exprValue) {
        scope->define(de.name.lexeme, *exprValue);
        return std::nullopt;
    }
    throw InterpreterError("Cannot interpret define with name " + de.name.lexeme, expr);
}
std::optional<SchemeValue> Interpreter::defineProcedure(DefineProcedure& dp, const Expression&)
{
    auto proc = std::make_shared<UserProcedure>(
        dp.parameters,
        (dp.body));
    scope->define(dp.name.lexeme, SchemeValue { std::move(proc) });
    return std::nullopt;
}
void Interpreter::run(const std::vector<std::shared_ptr<Expression>>& expressions)
{
    for (const auto& expr : expressions) {
        try {
            auto result = interpret(expr);
            if (result) {
                outputStream << result->toString() << std::endl;
            }
        } catch (const InterpreterError& e) {
            e.printFormattedError();
        }
    }
}

std::optional<SchemeValue> Interpreter::lambda(LambdaExpression& l, const Expression& e)
{
    auto proc = std::make_shared<UserProcedure>(
        std::move(l.parameters),
        std::move(l.body));

    auto lambda = SchemeValue(std::move(proc));
    return lambda;
}

std::optional<SchemeValue> Interpreter::ifExpression(const IfExpression& i, const Expression& e)
{

    auto condval = interpret(i.condition);

    if (condval->isTrue()) {
        return interpret(i.then);
    } else if (i.el) {
        return interpret(*i.el);
    }
    return std::nullopt;
}

std::optional<SchemeValue> Interpreter::interpretLetExpression(const LetExpression& le, const Expression& e)
{
    scope->pushFrame();
    auto define = [this, e](const std::string& name, SchemeValue val) {
        scope->define(name, val);
    };
    std::vector<Token> args = {};
    std::vector<SchemeValue> values = {};
    for (const auto& [k, v] : le.arguments) {
        auto val = *interpret(v);
        define(k.lexeme, val);
        values.push_back(val);
        args.push_back(k);
    }
    auto proc = std::make_shared<UserProcedure>(
        args,
        le.body);
    auto val = SchemeValue { proc };
    define(le.name->lexeme, val);
    return val.call(*this, values);
};

std::optional<SchemeValue> Interpreter::interpretTailExpression(const TailExpression& t, const Expression& expr)
{
    if (auto se = std::get_if<sExpression>(&t.expression->as)) {
        if (se->elements.empty()) {
            throw InterpreterError("Empty procedure call", expr);
        }
        auto procVal = interpret(se->elements[0]);
        if (!procVal || !procVal->isProc()) {
            throw InterpreterError("First element is not a procedure", expr);
        } else if (procVal->asProc()->isBuiltin()) {
            return interpret(t.expression);
        }
        auto proc = procVal->asProc();

        std::cout << "Tail call to procedure: " << procVal->toString() << std::endl;

        std::vector<SchemeValue> args;
        for (size_t i = 1; i < se->elements.size(); ++i) {
            auto argVal = interpret(se->elements[i]);
            if (argVal) {
                args.push_back(*argVal);
                std::cout << "Argument " << i << ": " << argVal->toString() << std::endl;
            } else {
                return std::nullopt;
            }
        }

        auto tailCall = std::make_shared<TailCall>(proc, args);
        std::cout << "Created TailCall" << std::endl;
        return SchemeValue(tailCall);
    } else {
        std::cout << "Expression is not an sExpression. Evaluating normally." << std::endl;
        return interpret(t.expression);
    }
}

std::optional<SchemeValue> Interpreter::interpretImport(const ImportExpression& im, const Expression& e)
{
    for (auto& tok : im.import) {
        auto f = readFile(std::format("../lib/{}.scm", tok.lexeme));
        p->load(s->tokenize(f));
        auto exprs = p->parse();
        if (exprs) {
            run(*exprs);
        } else {
            throw InterpreterError("Cannot parser import" + tok.lexeme);
        }
    }
    return std::nullopt;
}

std::optional<SchemeValue> Interpreter::defineSyntax(const DefineSyntaxExpression& dse, const Expression& e)
{
    macroExpander.defineMacro(dse.name.lexeme, dse.rule);
    return std::nullopt;
}
std::optional<SchemeValue> Interpreter::interpret(const std::shared_ptr<Expression>& e)
{
    return std::visit(overloaded {

                          [this, &e](const DefineSyntaxExpression& a) -> std::optional<SchemeValue> { return defineSyntax(a, *e); },
                          [this, &e](const ImportExpression& a) -> std::optional<SchemeValue> { return interpretImport(a, *e); },
                          [this, &e](const AtomExpression& a) -> std::optional<SchemeValue> { return interpretAtom(a, *e); },
                          [this, &e](const ListExpression& l) -> std::optional<SchemeValue> { return interpretList(l, *e); },
                          [this, &e](const sExpression& se) -> std::optional<SchemeValue> { return interpretSExpression(se, *e); },
                          [this, &e](const DefineExpression& de) -> std::optional<SchemeValue> { return defineExpression(de, *e); },
                          [this, &e](const VectorExpression& v) -> std::optional<SchemeValue> { return interpretVector(v, *e); },
                          [this, &e](DefineProcedure& dp) -> std::optional<SchemeValue> { return defineProcedure(dp, *e); },
                          [this, &e](LambdaExpression& l) -> std::optional<SchemeValue> { return lambda(l, *e); },
                          [this, &e](IfExpression& i) -> std::optional<SchemeValue> { return ifExpression(i, *e); },
                          [this, &e](QuoteExpression& q) -> std::optional<SchemeValue> { return interpretQuoteExpression(q, *e); },
                          [this, &e](TailExpression& t) -> std::optional<SchemeValue> { return interpretTailExpression(t, *e); },
                          [this, &e](LetExpression& t) -> std::optional<SchemeValue> { return interpretLetExpression(t, *e); },
                          [&e](const auto&) -> std::optional<SchemeValue> {
                              throw InterpreterError("Unknown expression type", *e);
                          } },
        e->as);
}

std::optional<SchemeValue> Interpreter::interpretVector(const VectorExpression& v, const Expression& e)
{

    std::vector<SchemeValue> elements;
    elements.reserve(v.elements.size());
    for (const auto& ele : v.elements) {
        std::optional<SchemeValue> item = interpret(ele);
        if (!item) {
            return std::nullopt;
        }
        elements.push_back(*item);
    }
    return SchemeValue(std::move(elements));
}
