#pragma once
#include "ForwardDecs.h"
#include <memory>

template<typename T>
struct ValueTrait {
    static SchemeValue toValue(const T& val);
};

template<>
struct ValueTrait<std::shared_ptr<Expression>> {
    static SchemeValue toValue(const std::shared_ptr<Expression>& expr);
};
