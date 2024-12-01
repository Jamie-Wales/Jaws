#pragma once
#include "Expression.h"
#include <memory>
#include <string>

std::shared_ptr<Expression> makeAtom(const std::string& lexeme, Tokentype type = Tokentype::IDENTIFIER);
std::shared_ptr<Expression> exprToList(std::shared_ptr<Expression> expr);
