#include "ValueTraits.h"
#include "Value.h"
#include "Expression.h"

template<typename T>
SchemeValue ValueTrait<T>::toValue(const T& val) {
    return SchemeValue(val);
}

SchemeValue ValueTrait<std::shared_ptr<Expression>>::toValue(
    const std::shared_ptr<Expression>& expr) {
    return expressionToValue(*expr);
}

template struct ValueTrait<Number>;
template struct ValueTrait<bool>;
template struct ValueTrait<std::string>;
template struct ValueTrait<Symbol>;
template struct ValueTrait<std::list<SchemeValue>>;
template struct ValueTrait<std::vector<SchemeValue>>;
template struct ValueTrait<std::shared_ptr<Procedure>>;
template struct ValueTrait<Port>;