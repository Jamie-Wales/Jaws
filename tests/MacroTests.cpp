#include "MacroTraits.h"
#include "parse.h"
#include "scan.h"
#include <gtest/gtest.h>

namespace {

// Helper: parse a single expression from input.
std::shared_ptr<Expression> parseExpr(const std::string& input) {
    auto tokens = scanner::tokenize(input);
    auto exprs = parse::parse(std::move(tokens));
    return (*exprs)[0];
}

// Helper: parse all expressions from input.
std::vector<std::shared_ptr<Expression>> parseAll(const std::string& input) {
    auto tokens = scanner::tokenize(input);
    auto exprs = parse::parse(std::move(tokens));
    return *exprs;
}

// Helper: define a macro in the given environment using the provided source.
void defineMacro(pattern::MacroEnvironment& env, const std::string& input) {
    auto exprs = parseAll(input);
    for (const auto& expr : exprs) {
        std::visit(
            overloaded{
                [&](const DefineSyntaxExpression &de) {
                    // Create a rule expression from the SyntaxRulesExpression.
                    auto rule = std::make_shared<Expression>(
                        Expression{std::get<SyntaxRulesExpression>(de.rule->as), expr->line});
                    env.defineMacro(de.name.lexeme, rule);
                },
                [](const auto &) {}
            },
            expr->as);
    }
}

// Helper: expand a single expression in the given environment.
std::string expandOne(pattern::MacroEnvironment& env, const std::string &input) {
    auto expr = parseExpr(input);
    auto expanded = macroexp::expandMacro(expr, env);
    return macroexp::fromExpr(expanded)->toString();
}

} // end anonymous namespace

TEST(MacroExpanderTest, ConstantMacro) {
    pattern::MacroEnvironment env;
    defineMacro(env, R"(
        (define-syntax constant
          (syntax-rules ()
            ((constant) 42)))
    )");

    EXPECT_TRUE(env.isMacro("constant"));
    EXPECT_EQ(expandOne(env, "(constant)"), "42");
}

TEST(MacroExpanderTest, LetStarMacro) {
    pattern::MacroEnvironment env;
    defineMacro(env, R"(
        (define-syntax let*
          (syntax-rules ()
            ((let* () body1 body2 ...)
             (let () body1 body2 ...))
            ((let* ((name1 val1) (name2 val2) ...)
             body1 body2 ...)
             (let ((name1 val1))
               (let* ((name2 val2) ...)
                 body1 body2 ...)))))
    )");

    EXPECT_TRUE(env.isMacro("let*"));

    // Test empty bindings.
    EXPECT_EQ(expandOne(env, "(let* () 42)"), "(let () 42)");

    // Test single binding.
    EXPECT_EQ(expandOne(env, "(let* ((x 1)) x)"), "(let ((x 1)) x)");

    // Test multiple bindings.
    auto result = expandOne(env, "(let* ((x 1) (y (+ x 1))) (* x y))");
    EXPECT_EQ(result, "(let ((x 1)) (let* ((y (+ x 1))) (* x y)))");

    // Test nested expansion.
    result = expandOne(env, "(let* ((x 1) (y (+ x 1)) (z (+ y 2))) (* x y z))");
    EXPECT_EQ(result, "(let ((x 1)) (let* ((y (+ x 1)) (z (+ y 2))) (* x y z)))");
}

TEST(MacroExpanderTest, CondMacro) {
    pattern::MacroEnvironment env;
    defineMacro(env, R"(
        (define-syntax cond
          (syntax-rules (else =>)
            ((cond (else result1 result2 ...))
             (begin result1 result2 ...))
            ((cond (test => result))
             (let ((temp test))
               (if temp (result temp))))
            ((cond (test => result) clause1 clause2 ...)
             (let ((temp test))
               (if temp
                   (result temp)
                   (cond clause1 clause2 ...))))
            ((cond (test)) test)
            ((cond (test) clause1 clause2 ...)
             (let ((temp test))
               (if temp
                   temp
                   (cond clause1 clause2 ...))))
            ((cond (test result1 result2 ...))
             (if test (begin result1 result2 ...)))
            ((cond (test result1 result2 ...)
                   clause1 clause2 ...)
             (if test
                 (begin result1 result2 ...)
                 (cond clause1 clause2 ...)))))
    )");

    EXPECT_TRUE(env.isMacro("cond"));

    // Test else clause.
    EXPECT_EQ(expandOne(env, "(cond (else 42))"), "(begin 42)");

    // Test => syntax.
    auto result = expandOne(env, "(cond (x => f))");
    EXPECT_EQ(result, "(let ((temp x)) (if temp (f temp)))");

    // Test multiple => clauses.
    result = expandOne(env, "(cond (x => f) (y => g))");
    EXPECT_EQ(result, "(let ((temp x)) (if temp (f temp) (cond (y => g))))");

    // Test simple test clause.
    EXPECT_EQ(expandOne(env, "(cond (x))"), "x");

    // Test test with expressions.
    result = expandOne(env, "(cond (x 1 2 3))");
    EXPECT_EQ(result, "(if x (begin 1 2 3))");

    // Test multiple clauses.
    result = expandOne(env, "(cond (x 1) (y 2) (else 3))");
    EXPECT_EQ(result, "(if x (begin 1) (cond (y 2) (else 3)))");
}

TEST(MacroExpanderTest, MacroLiterals) {
    pattern::MacroEnvironment env;
    // Define a cond macro that only handles a literal else.
    defineMacro(env, R"(
        (define-syntax cond
          (syntax-rules (else =>)
            ((cond (else result1 result2 ...))
             (begin result1 result2 ...))))
    )");

    // The literal else should match.
    EXPECT_EQ(expandOne(env, "(cond (else 42))"), "(begin 42)");

    // A variable named else should not match the literal.
    auto tokens = scanner::tokenize("(define else 42)");
    auto exprs = parse::parse(std::move(tokens));
    bool throwsError = false;
    try {
        macroexp::expandMacro((*exprs)[0], env);
    } catch (...) {
        throwsError = true;
    }
    EXPECT_FALSE(throwsError);
}

TEST(MacroExpanderTest, MacroRecursion) {
    pattern::MacroEnvironment env;
    // Define let* macro.
    defineMacro(env, R"(
        (define-syntax let*
          (syntax-rules ()
            ((let* () body1 body2 ...)
             (let () body1 body2 ...))
            ((let* ((name1 val1) (name2 val2) ...)
             body1 body2 ...)
             (let ((name1 val1))
               (let* ((name2 val2) ...)
                 body1 body2 ...)))))
    )");

    // Define constant macro.
    defineMacro(env, R"(
        (define-syntax constant
          (syntax-rules ()
            ((constant) 42)))
    )");

    // Test let* with constant.
    auto result = expandOne(env, "(let* ((x (constant))) x)");
    EXPECT_EQ(result, "(let ((x 42)) x)");

    // Test deeply nested let*.
    result = expandOne(env, "(let* ((x 1) (y (constant)) (z (+ x y))) z)");
    EXPECT_EQ(result, "(let ((x 1)) (let* ((y 42) (z (+ x y))) z))");
}
