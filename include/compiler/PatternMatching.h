#pragma once
#include "Expression.h"
#include <map>
#include <memory>
#include <optional>

class PatternEnvironment {
public:
    std::map<std::string, std::vector<std::shared_ptr<Expression>>> bindings;
    void bind(const std::string& identifier, std::shared_ptr<Expression> syntax);
    void bindList(const std::string& identifier, const std::vector<std::shared_ptr<Expression>>& syntaxList);
    bool merge(const PatternEnvironment& other);
};

class PatternMatcher {
public:
    std::optional<PatternEnvironment> match(std::shared_ptr<Expression> pattern,
        std::shared_ptr<Expression> syntax);

private:
    std::optional<PatternEnvironment> matchLiteral(const AtomExpression& pattern,
        std::shared_ptr<Expression> syntax);
    std::optional<PatternEnvironment> matchVariadicList(const ListExpression& pattern,
        const ListExpression& syntax);
    std::optional<PatternEnvironment> matchList(const ListExpression& pattern,
        const ListExpression& syntax);
};

class PatternSubstitutor {
public:
    std::shared_ptr<Expression> substitute(std::shared_ptr<Expression> templateExpr,
        const PatternEnvironment& env);
};
