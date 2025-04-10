#include "interpret.h"
#include "Expression.h"
#include "Procedure.h"
#include "jaws_values.h"

// define DEBUG_LOGGING
#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "(interp) " << x << std::endl
#else
#define DEBUG_LOG(x)
#endif

#include "Value.h"
#include "builtins/JawsEq.h"
#include "builtins/JawsHof.h"
#include "builtins/JawsIO.h"
#include "builtins/JawsList.h"
#include "builtins/JawsMath.h"
#include "builtins/JawsThread.h"
#include "builtins/JawsVector.h"
#include "builtins/jaws_ffi.h"
#include "builtins/jaws_string.h"

namespace interpret {

InterpreterState createInterpreter()
{
    InterpreterState state;

    auto define = [&state](const HygienicSyntax& syntax, BuiltInProcedure::Func func) {
        state.env->define(syntax, SchemeValue(std::make_shared<BuiltInProcedure>(func)));
    };

    // Create a helper for creating identifier syntax objects
    auto identifier = [](const std::string& name) -> HygienicSyntax {
        Token token;
        token.type = Tokentype::IDENTIFIER;
        token.lexeme = name;
        return HygienicSyntax { token, SyntaxContext() };
    };

    define(identifier("+"), jaws_math::plus);
    define(identifier("-"), jaws_math::minus);
    define(identifier("*"), jaws_math::mult);
    define(identifier("/"), jaws_math::div);
    define(identifier("<="), jaws_math::lessOrEqual);
    define(identifier(">="), jaws_math::greaterOrEqual);
    define(identifier("<"), jaws_math::less);
    define(identifier(">"), jaws_math::greater);
    define(identifier("="), jaws_eq::equal);
    define(identifier("eq?"), jaws_eq::equal);
    define(identifier("equal?"), jaws_eq::equal);
    define(identifier("boolean?"), jaws_eq::isBooleanProc);
    define(identifier("procedure?"), jaws_eq::isProcedure);
    define(identifier("char?"), jaws_eq::isChar);
    define(identifier("pair?"), jaws_eq::isPair);
    define(identifier("null?"), jaws_eq::isNull);
    define(identifier("port?"), jaws_eq::isPort);
    define(identifier("eqv?"), jaws_eq::isEqv);
    define(identifier("socket-server"), jaws_io::socketServer);
    define(identifier("socket-connect"), jaws_io::socketConnect);
    define(identifier("socket-accept"), jaws_io::socketAccept);
    define(identifier("socket-read"), jaws_io::socketRead);
    define(identifier("socket-write"), jaws_io::socketWrite);
    define(identifier("socket-close"), jaws_io::socketClose);
    define(identifier("socket-set-nonblocking!"), jaws_io::socketSetNonBlocking);
    define(identifier("error"), jaws_io::error);
    define(identifier("symbol?"), jaws_eq::isSymbol);
    define(identifier("number?"), jaws_eq::isNumber);
    define(identifier("string?"), jaws_eq::isString);
    define(identifier("open-input-file"), jaws_io::openInputFile);
    define(identifier("open-output-file"), jaws_io::openOutputFile);
    define(identifier("close-port"), jaws_io::closePort);
    define(identifier("read"), jaws_io::read);
    define(identifier("write"), jaws_io::write);
    define(identifier("display"), jaws_io::display);
    define(identifier("newline"), jaws_io::newline);
    define(identifier("map"), jaws_hof::map);
    define(identifier("list?"), jaws_eq::isList);
    define(identifier("list"), jaws_list::listProcedure);
    define(identifier("car"), jaws_list::carProcudure);
    define(identifier("cdr"), jaws_list::cdrProcedure);
    define(identifier("cons"), jaws_list::cons);
    define(identifier("append"), jaws_list::append);
    define(identifier("load-library"), jaws_ffi::loadLibrary);
    define(identifier("register-function"), jaws_ffi::registerFunction);
    define(identifier("list-ref"), jaws_list::listRef);
    define(identifier("list-set!"), jaws_list::listSet);
    define(identifier("make-vector"), jaws_vec::makeVector);
    define(identifier("vector"), jaws_vec::vectorProcedure);
    define(identifier("vector-ref"), jaws_vec::vectorRef);
    define(identifier("vector-set!"), jaws_vec::vectorSet);
    define(identifier("vector-length"), jaws_vec::vectorLength);
    define(identifier("vector-copy"), jaws_vec::vectorCopy);
    define(identifier("vector-copy!"), jaws_vec::vectorCopyTo);
    define(identifier("vector-fill!"), jaws_vec::vectorFill);
    define(identifier("eval"), jaws_hof::eval);
    define(identifier("apply"), jaws_hof::apply);
    define(identifier("call/cc"), jaws_hof::callCC);
    define(identifier("call-with-current-continuation"), jaws_hof::callCC);
    define(identifier("number->string"), jaws_string::numberToString);
    define(identifier("string=?"), jaws_string::stringEqual);
    define(identifier("string<?"), jaws_string::stringLess);
    define(identifier("string>?"), jaws_string::stringGreater);
    define(identifier("string-ci=?"), jaws_string::stringCiEqual);
    define(identifier("string-length"), jaws_string::stringLength);
    define(identifier("string-append"), jaws_string::stringAppend);
    define(identifier("substring"), jaws_string::substring);
    define(identifier("string-ref"), jaws_string::stringRef);
    define(identifier("string->list"), jaws_string::stringToList);
    define(identifier("list->string"), jaws_string::listToString);
    define(identifier("string-copy"), jaws_string::stringCopy);
    define(identifier("string-upcase"), jaws_string::stringUpcase);
    define(identifier("string-downcase"), jaws_string::stringDowncase);
    define(identifier("thread-spawn"), jaws_thread::threadSpawn);
    define(identifier("thread-join"), jaws_thread::threadJoin);
    define(identifier("thread-sleep!"), jaws_thread::threadSleep);
    define(identifier("thread-current-id"), jaws_thread::threadCurrentId);
    define(identifier("mutex-create"), jaws_thread::mutexCreate);
    define(identifier("mutex-lock!"), jaws_thread::mutexLock);
    define(identifier("mutex-unlock!"), jaws_thread::mutexUnlock);
    define(identifier("condition-variable-create"), jaws_thread::conditionCreate);
    define(identifier("condition-variable-wait"), jaws_thread::conditionWait);
    define(identifier("condition-variable-signal!"), jaws_thread::conditionSignal);
    define(identifier("condition-variable-broadcast!"), jaws_thread::conditionBroadcast);
    define(identifier("vector->list"), jaws_values::vectorToList);
    define(identifier("list->vector"), jaws_values::listToVector);

    return state;
}

std::optional<SchemeValue> interpret(
    InterpreterState& state,
    const std::vector<std::shared_ptr<Expression>>& exprs)
{
    std::optional<SchemeValue> result = std::nullopt;
    for (const auto& expr : exprs) {
        result = interpret(state, expr);
    }
    return result;
}

std::optional<SchemeValue> interpret(
    InterpreterState& state,
    const std::shared_ptr<Expression>& expr)
{
    try {
        return std::visit(overloaded {
                              [&](const AtomExpression& e) -> std::optional<SchemeValue> {
                                  return interpretAtom(state, e);
                              },
                              [&](const ListExpression& e) -> std::optional<SchemeValue> {
                                  return interpretList(state, e);
                              },
                              [&](const sExpression& e) -> std::optional<SchemeValue> {
                                  return interpretSExpression(state, e);
                              },
                              [&](const DefineExpression& e) -> std::optional<SchemeValue> {
                                  return interpretDefine(state, e);
                              },
                              [&](const DefineSyntaxExpression& e) -> std::optional<SchemeValue> {
                                  return std::nullopt;
                              },
                              [&](const DefineProcedure& e) -> std::optional<SchemeValue> {
                                  return interpretDefineProcedure(state, e);
                              },
                              [&](const LambdaExpression& e) -> std::optional<SchemeValue> {
                                  return interpretLambda(state, e);
                              },
                              [&](const IfExpression& e) -> std::optional<SchemeValue> {
                                  return interpretIf(state, e);
                              },
                              [&](const QuoteExpression& e) -> std::optional<SchemeValue> {
                                  return interpretQuote(state, e);
                              },
                              [&](const VectorExpression& e) -> std::optional<SchemeValue> {
                                  return interpretVector(state, e);
                              },
                              [&](const TailExpression& e) -> std::optional<SchemeValue> {
                                  return interpretTailCall(state, e);
                              },
                              [&](const LetExpression& e) -> std::optional<SchemeValue> {
                                  return interpretLet(state, e);
                              },
                              [&](const ImportExpression& e) -> std::optional<SchemeValue> {
                                  return std::nullopt;
                              },
                              [&](const SetExpression& e) -> std::optional<SchemeValue> {
                                  return interpretSet(state, e);
                              },
                              [&](const auto&) -> std::optional<SchemeValue> {
                                  throw InterpreterError("Unknown expression type");
                              } },
            expr->as);
    } catch (const ContinuationInvocationException& e) {
        // Re-throw the exception to continue unwinding the stack
        throw;
    }
}
std::optional<SchemeValue> interpretAtom(InterpreterState& state, const AtomExpression& atom)
{
    const Token& token = atom.value.token;
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
            }
            double real = std::stod(complexStr.substr(0, separatorPos));
            double imag = std::stod(complexStr.substr(separatorPos));
            return SchemeValue(Number(std::complex<double>(real, imag)));
        }

        case Tokentype::RATIONAL: {
            size_t slashPos = token.lexeme.find('/');
            if (slashPos != std::string::npos) {
                int numerator = std::stoi(token.lexeme.substr(0, slashPos));
                int denominator = std::stoi(token.lexeme.substr(slashPos + 1));
                return SchemeValue(Number(Number::Rational(numerator, denominator)));
            }
            throw InterpreterError("Invalid rational number format");
        }

        case Tokentype::STRING:
            return SchemeValue(token.lexeme.substr(1, token.lexeme.length() - 2));

        case Tokentype::TRUE:
            return SchemeValue(true);

        case Tokentype::FALSE:
            return SchemeValue(false);

        case Tokentype::CHAR: // or Tokentype::CHAR
        {
            std::string charLiteral = token.lexeme;
            std::string charValue = charLiteral.substr(2);
            if (charValue == "space") {
                return SchemeValue(char(' '));
            } else if (charValue == "newline") {
                return SchemeValue(char('\n'));
            } else if (charValue == "tab") {
                return SchemeValue(char('\t'));
            } else if (charValue == "return") {
                return SchemeValue(char('\r'));
            } else if (charValue.length() == 1) {
                return SchemeValue(charValue[0]);
            } else {
                throw InterpreterError("Unknown character literal: " + charLiteral);
            }
        }
        case Tokentype::IDENTIFIER:
            if (auto value = state.env->get(atom.value)) {
                return value;
            }
            throw InterpreterError("Undefined variable: " + token.lexeme);

        default:
            throw InterpreterError("Unexpected token type in atom");
        }
    } catch (const std::exception& e) {
        throw InterpreterError(e.what());
    }
}

std::optional<SchemeValue> interpretList(InterpreterState& state, const ListExpression& list)
{
    std::list<SchemeValue> elements;
    for (const auto& ele : list.elements) {
        auto item = interpret(state, ele);
        if (!item)
            return std::nullopt;
        elements.push_back(*item);
    }
    return SchemeValue(std::make_shared<std::list<SchemeValue>>(std::move(elements)));
}

std::optional<SchemeValue> interpretSExpression(InterpreterState& state, const sExpression& sexpr)
{
    auto call = evaluateProcedureCall(state, sexpr); // Uses current state.env
    if (!call) {
        throw InterpreterError("Cannot evaluate procedure call: Expression did not evaluate to a callable procedure.");
    }
    DEBUG_LOG("InterpretSExpr: Calling executeProcedure for proc: " << call->procedure.toString() << " from Env@ " << state.env.get());

    auto savedEnv = state.env; // <<< SAVE ENV HERE
    std::optional<SchemeValue> result;
    try {
        result = executeProcedure(state, call->procedure, std::move(call->arguments));
    } catch (const ContinuationInvocationException& e) {
        throw;
    }
    state.env = savedEnv; // <<< RESTORE ENV HERE
    DEBUG_LOG("InterpretSExpr: Restored Env@ " << savedEnv.get() << " after procedure execution completed.");
    return result;
}
std::optional<SchemeValue> interpretDefine(InterpreterState& state, const DefineExpression& def)
{
    auto value = interpret(state, def.value);
    if (!value)
        return std::nullopt;
    if (value->isProc() && !value->asProc()->isBuiltin()) {
        auto proc = value->asProc();
        if (auto userProc = std::dynamic_pointer_cast<UserProcedure>(proc)) {
            userProc->closure->define(def.name, *value);
        }
    }
    state.env->define(def.name, *value);
    return std::nullopt;
}

std::optional<SchemeValue> interpretDefineProcedure(InterpreterState& state, const DefineProcedure& proc)
{
    std::vector<HygienicSyntax> params;
    params.reserve(proc.parameters.size());
    for (const auto& param : proc.parameters) {
        params.push_back(param); // Now param is already a HygienicSyntax
    }
    auto procedure = std::make_shared<UserProcedure>(params, proc.body, state.env, proc.isVariadic);
    state.env->define(proc.name, SchemeValue(std::move(procedure)));
    return std::nullopt;
}
std::optional<SchemeValue> interpretLambda(InterpreterState& state, const LambdaExpression& lambda)
{
    std::vector<HygienicSyntax> params;
    params.reserve(lambda.parameters.size());
    for (const auto& param : lambda.parameters) {
        params.push_back(param); // Now param is already a HygienicSyntax
    }
    auto closureEnv = state.env;
    auto proc = std::make_shared<UserProcedure>(
        params, lambda.body, closureEnv, lambda.isVariadic);
    return SchemeValue(std::move(proc));
}
std::optional<SchemeValue> interpretIf(InterpreterState& state, const IfExpression& ifexpr)
{
    auto condition = interpret(state, ifexpr.condition);
    if (!condition)
        return std::nullopt;

    if (condition->isTrue()) {
        return interpret(state, ifexpr.then);
    } else if (ifexpr.el) {
        return interpret(state, *ifexpr.el);
    }
    return std::nullopt;
}

std::optional<SchemeValue> interpretLet(InterpreterState& state, const LetExpression& let)
{
    auto closureEnv = state.env;
    std::vector<HygienicSyntax> paramTokens = let.getParameterSyntax();
    std::vector<SchemeValue> args;
    args.reserve(let.arguments.size());
    DEBUG_LOG("InterpretLet: Evaluating arguments in Env@ " << state.env.get());
    for (const auto& binding : let.arguments) {
        auto argValOpt = interpret(state, binding.second);
        if (!argValOpt)
            throw InterpreterError("Let binding value evaluated to void.");
        args.push_back(*argValOpt);
    }
    auto lambdaProc = std::make_shared<UserProcedure>(
        paramTokens, let.body, closureEnv, false);
    if (let.name) {
        DEBUG_LOG("InterpretLet: Setting up named let '" << let.name->token.lexeme << "'");
        auto letExecEnv = closureEnv->extend();
        for (size_t k = 0; k < let.arguments.size(); ++k) {
            DEBUG_LOG("InterpretLet: Defining '" << let.arguments[k].first.token.lexeme << "' in named let Env@ " << letExecEnv.get());
            letExecEnv->define(let.arguments[k].first, args[k]);
        }
        lambdaProc->closure = letExecEnv;
        DEBUG_LOG("InterpretLet: Defining '" << let.name->token.lexeme << "' in named let Env@ " << letExecEnv.get() << " (points to self)");
        letExecEnv->define(*let.name, SchemeValue(lambdaProc));
    }

    DEBUG_LOG("InterpretLet: Calling executeProcedure for let body/proc from Env@ " << state.env.get());
    auto savedEnv = state.env;
    std::optional<SchemeValue> result;
    try {
        result = executeProcedure(state, SchemeValue(lambdaProc), std::move(args));
    } catch (const ContinuationInvocationException& e) {
        throw;
    }
    state.env = savedEnv;
    DEBUG_LOG("InterpretLet: Restored Env@ " << savedEnv.get() << " after let body execution completed.");
    return result;
}
std::optional<SchemeValue> interpretQuote(InterpreterState& state, const QuoteExpression& quote)
{
    return SchemeValue(quote.expression);
}

std::optional<SchemeValue> interpretVector(InterpreterState& state, const VectorExpression& vec)
{
    std::vector<SchemeValue> elements;
    elements.reserve(vec.elements.size());

    for (const auto& ele : vec.elements) {
        auto item = interpret(state, ele);
        if (!item)
            return std::nullopt;
        elements.push_back(*item);
    }
    return SchemeValue(std::make_shared<std::vector<SchemeValue>>(std::move(elements)));
}

std::optional<SchemeValue> interpretTailCall(InterpreterState& state, const TailExpression& tail)
{
    auto innerExpr = tail.expression;
    if (auto sexpr = std::get_if<sExpression>(&innerExpr->as)) {
        auto callOpt = evaluateProcedureCall(state, *sexpr);
        if (!callOpt) {
            throw InterpreterError("Failed to evaluate potential tail call sExpression.");
        }

        if (callOpt->procedure.isProc() && callOpt->procedure.asProc()->isUserProcedure()) {
            state.isTailCallPending = true;
            state.pendingProcedure = callOpt->procedure;
            state.pendingArguments = std::move(callOpt->arguments);
            return std::nullopt; // Signal TCO state was set
        } else if (callOpt->procedure.isProc()) {
            return (*callOpt->procedure.asProc())(state, callOpt->arguments);
        } else {
            throw InterpreterError("Tail call target evaluated to non-procedure.");
        }

    } else {
        return interpret(state, innerExpr);
    }
}

bool fileExists(const std::string& path)
{
    std::ifstream f(path.c_str());
    return f.good();
}

std::optional<SchemeValue> interpretSet(InterpreterState& state, const SetExpression& set)
{
    auto val = interpret(state, set.value);
    if (!val) {
        throw InterpreterError("Cannot set undefined value");
    }
    state.env->set(set.identifier, *val);
    return std::nullopt;
}

std::optional<ProcedureCall> evaluateProcedureCall(
    InterpreterState& state,
    const sExpression& sexpr)
{
    if (sexpr.elements.empty()) {
        throw InterpreterError("Empty procedure call");
    }
    DEBUG_LOG("EvaluateProcCall: Interpreting operator: " << sexpr.elements[0]->toString());

    auto procValueOpt = interpret(state, sexpr.elements[0]);
    if (!procValueOpt) {
        DEBUG_LOG("EvaluateProcCall: Operator interpretation failed!");
        throw InterpreterError("Procedure expression did not evaluate to a value.");
    }
    if (!procValueOpt->isProc()) {
        DEBUG_LOG("EvaluateProcCall: Operator not a procedure: " << procValueOpt->toString());
        throw InterpreterError("Expression does not evaluate to a procedure: " + procValueOpt->toString());
    }

    DEBUG_LOG("EvaluateProcCall: Operator is: " << procValueOpt->toString());

    std::vector<SchemeValue> args;
    args.reserve(sexpr.elements.size() - 1);
    for (size_t i = 1; i < sexpr.elements.size(); i++) {
        DEBUG_LOG("EvaluateProcCall: Interpreting argument " << i << ": " << sexpr.elements[i]->toString());
        auto argOpt = interpret(state, sexpr.elements[i]);
        if (!argOpt) {
            DEBUG_LOG("EvaluateProcCall: Argument " << i << " interpretation failed!");
            throw InterpreterError("cannot evaluate argument " + std::to_string(i));
        }
        DEBUG_LOG("EvaluateProcCall: Argument " << i << " result: " << argOpt->toString());
        args.push_back(*argOpt);
    }
    DEBUG_LOG("EvaluateProcCall: Evaluation successful.");
    return ProcedureCall { *procValueOpt, std::move(args) };
}
std::optional<SchemeValue> executeProcedure(
    InterpreterState& state,
    SchemeValue initialProcedure,
    std::vector<SchemeValue> initialArgs)
{
    SchemeValue currentProcedure = std::move(initialProcedure);
    std::vector<SchemeValue> currentArgs = std::move(initialArgs);

    while (true) {
        if (state.isTailCallPending) {
            DEBUG_LOG("ExecuteProc: Handling pending TAIL CALL to: " << state.pendingProcedure->toString());
            currentProcedure = *state.pendingProcedure;
            currentArgs = std::move(state.pendingArguments);
            state.isTailCallPending = false;
            state.pendingProcedure.reset();
            state.pendingArguments.clear();
            DEBUG_LOG("ExecuteProc: Loaded pending call. Continuing loop.");
        }

        DEBUG_LOG("ExecuteProc: Executing: " << currentProcedure.toString() << " in Env@ " << state.env.get());
        if (!currentProcedure.isProc()) {
            throw InterpreterError("Not a procedure: " + currentProcedure.toString());
        }
        auto procedurePtr = currentProcedure.asProc();
        std::optional<SchemeValue> result;
        try {
            if (procedurePtr->isBuiltin()) {
                result = (*procedurePtr)(state, currentArgs);
            } else if (auto userProc = std::dynamic_pointer_cast<UserProcedure>(procedurePtr)) {
                result = userProc->executeBody(state, currentArgs);
            } else {
                throw InterpreterError("Unhandled procedure type: " + currentProcedure.toString());
            }
        } catch (const ContinuationInvocationException& e) {
            throw;
        }

        if (state.isTailCallPending) {
            DEBUG_LOG("ExecuteProc: TCO requested by the call just executed. Looping.");
            continue;
        }

        DEBUG_LOG("ExecuteProc: Normal return from procedure call.");
        return result;
    }
}
}
