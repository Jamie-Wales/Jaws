#pragma once

#include "ANF.h"
#include "Expression.h"
#include <cstdlib>
#include <vector>
namespace ir {

std::vector<std::shared_ptr<ANF>> ANFtransform(std::vector<std::shared_ptr<Expression>> expressions);
std::optional<std::shared_ptr<ANF>> transform(const std::shared_ptr<Expression>& toTransform, size_t& currentNumber);
}
