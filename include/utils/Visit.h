#pragma once
#include <tuple>
#include <variant>

// Original single-variant visitor
template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

template <typename... Visitors>
struct multi_visitor : Visitors... {
    using Visitors::operator()...;
    
    template <typename... Ts>
    multi_visitor(Ts&&... visitors) : Visitors(std::forward<Ts>(visitors))... {}
};

template <typename... Ts>
multi_visitor(Ts...) -> multi_visitor<std::remove_reference_t<Ts>...>;

template<typename... Variants, typename Visitor>
decltype(auto) visit_many(Visitor&& visitor, Variants&&... variants) {
    return std::visit(std::forward<Visitor>(visitor), 
                     std::forward<Variants>(variants)...);
}
