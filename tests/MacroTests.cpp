#include "MacroTraits.h"
#include "parse.h"
#include "scan.h"
#include <gtest/gtest.h>

namespace {

// Helper: parse a single expression from input.
std::shared_ptr<Expression> parseExpr(const std::string& input)
{
    auto tokens = scanner::tokenize(input);
    auto exprs = parse::parse(std::move(tokens));
    return (*exprs)[0];
}

// Helper: parse all expressions from input.
std::vector<std::shared_ptr<Expression>> parseAll(const std::string& input)
{
    auto tokens = scanner::tokenize(input);
    auto exprs = parse::parse(std::move(tokens));
    return *exprs;
}
}
