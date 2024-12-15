/**
 * @file Interpreter.h
 * Core interpreter for the Jaws Scheme implementation
 *
 * Copyright (c) 2024. All rights reserved.
 * Licensed under the MIT license. See LICENSE file in the project root.
 */

#pragma once
#include "Environment.h"
#include "Expression.h"
#include "MacroProcessor.h"
#include "Parser.h"
#include "Scanner.h"
#include "Value.h"
#include <memory>
#include <optional>

class InterpreterError;

/**
 * @brief Core interpreter class for evaluating Scheme expressions
 *
 * Handles the evaluation of Scheme expressions, maintains the execution environment,
 * and manages variable bindings and procedure calls.
 */
class Interpreter {
private:
    /**
     * @brief Helper structure for procedure calls
     */
    struct ProcedureCall {
        SchemeValue procedure; ///< The procedure to be called
        std::vector<SchemeValue> arguments; ///< Evaluated arguments for the procedure
    };

    std::optional<SchemeValue> interpretSetExpression(const SetExpression& s, const Expression& e);
    /**
     * @brief Evaluates a let expression
     * @param le The let expression to evaluate
     * @param e Original expression for error context
     * @return Result of the evaluation
     */
    std::optional<SchemeValue> interpretLetExpression(const LetExpression& le, const Expression& e);

    /**
     * @brief Processes a define-syntax expression for macro definitions
     */
    std::optional<SchemeValue> defineSyntax(const DefineSyntaxExpression& dse, const Expression& e);

    /**
     * @brief Handles import expressions for loading external files
     */
    std::optional<SchemeValue> interpretImport(const ImportExpression& ie, const Expression& expr);

    /**
     * @brief Processes variable definitions
     */
    std::optional<SchemeValue> defineExpression(const DefineExpression& de, const Expression& expr);

    /**
     * @brief Evaluates atomic expressions (numbers, strings, symbols)
     */
    std::optional<SchemeValue> interpretAtom(const AtomExpression& atom, const Expression& expr);

    /**
     * @brief Evaluates list expressions
     */
    std::optional<SchemeValue> interpretList(const ListExpression& list, const Expression& expr);

    /**
     * @brief Evaluates lambda expressions
     */
    std::optional<SchemeValue> lambda(LambdaExpression& l, const Expression& e);

    /**
     * @brief Evaluates S-expressions (procedure applications)
     */
    std::optional<SchemeValue> interpretSExpression(const sExpression& se, const Expression& expr);

    /**
     * @brief Processes procedure definitions
     */
    std::optional<SchemeValue> defineProcedure(DefineProcedure& dp, const Expression& e);

    /**
     * @brief Evaluates vector expressions
     */
    std::optional<SchemeValue> interpretVector(const VectorExpression& v, const Expression& e);

    /**
     * @brief Evaluates if expressions
     */
    std::optional<SchemeValue> ifExpression(const IfExpression& i, const Expression& e);

    /**
     * @brief Evaluates quote expressions
     */
    std::optional<SchemeValue> interpretQuoteExpression(const QuoteExpression& qe, const Expression& e);

    /**
     * @brief Handles tail-call optimization expressions
     */
    std::optional<SchemeValue> interpretTailExpression(const TailExpression& t, const Expression& e);

    /**
     * @brief Evaluates a procedure call
     * @param se The S-expression representing the procedure call
     * @param expr Original expression for error context
     * @return Procedure and its evaluated arguments
     */
    std::optional<ProcedureCall> evaluateProcedureCall(const sExpression& se, const Expression& expr);

public:
    /**
     * @brief Constructs a new Interpreter instance
     * @param s Scanner for tokenizing input
     * @param p Parser for creating AST
     */
    Interpreter(std::shared_ptr<Scanner> s, std::shared_ptr<Parser> p);

    std::shared_ptr<Scanner> s; ///< Scanner instance for tokenizing
    std::shared_ptr<Parser> p; ///< Parser instance for AST creation
    std::shared_ptr<Environment> scope; ///< Current execution environment
    std::stringstream outputStream; ///< Output stream for results
    MacroProcessor macroProcessor; ///< Handles macro expansion

    /**
     * @brief Initializes the interpreter
     */
    void init();

    /**
     * @brief Runs a sequence of expressions
     * @param expressions Vector of expressions to evaluate
     */
    void run(const std::vector<std::shared_ptr<Expression>>& expressions);

    /**
     * @brief Executes a procedure with given arguments
     * @param proc The procedure to execute
     * @param args Vector of evaluated arguments
     * @return Result of the procedure execution
     */
    std::optional<SchemeValue> executeProcedure(
        const SchemeValue& proc,
        const std::vector<SchemeValue>& args);

    /**
     * @brief Interprets a single expression
     * @param e Expression to interpret
     * @return Result of the interpretation
     */
    std::optional<SchemeValue> interpret(const std::shared_ptr<Expression>& e);

    /**
     * @brief Looks up a variable in the current environment
     * @param name Name of the variable to look up
     * @return Value of the variable if found
     */
    std::optional<SchemeValue> lookupVariable(const std::string& name) const;
};
