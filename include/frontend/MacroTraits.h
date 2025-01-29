#pragma once
#include "Expression.h"
#include <memory>
#include <type_traits>
#include <unordered_map>
#include <vector>

namespace expr {

// Forward declarations for pattern matching types
struct PatternMatch {
    std::vector<std::shared_ptr<Expression>> matches;
};
using MatchEnv = std::unordered_map<std::string, PatternMatch>;

namespace traits {
    ///////////////////////////////
    // Expression Type Traits
    ///////////////////////////////
    
    template <typename T>
    struct is_expression_type : std::false_type { };
    
    template <> struct is_expression_type<AtomExpression> : std::true_type { };
    template <> struct is_expression_type<ListExpression> : std::true_type { };
    template <> struct is_expression_type<sExpression> : std::true_type { };
    template <> struct is_expression_type<QuoteExpression> : std::true_type { };
    template <> struct is_expression_type<LambdaExpression> : std::true_type { };
    template <> struct is_expression_type<IfExpression> : std::true_type { };
    template <> struct is_expression_type<BeginExpression> : std::true_type { };
    template <> struct is_expression_type<LetExpression> : std::true_type { };
    
    template <typename T>
    inline constexpr bool is_expression_type_v = is_expression_type<T>::value;

    ///////////////////////////////
    // Nested Expression Traits
    ///////////////////////////////
    
    template <typename T>
    struct has_nested_expressions : std::false_type { };
    
    template <> struct has_nested_expressions<ListExpression> : std::true_type { };
    template <> struct has_nested_expressions<sExpression> : std::true_type { };
    template <> struct has_nested_expressions<BeginExpression> : std::true_type { };
    template <> struct has_nested_expressions<LetExpression> : std::true_type { };
    
    template <typename T>
    inline constexpr bool has_nested_expressions_v = has_nested_expressions<T>::value;

    ///////////////////////////////
    // Atomic Expression Traits
    ///////////////////////////////
    
    template <typename T>
    struct is_atomic_expression : std::false_type { };
    
    template <> struct is_atomic_expression<AtomExpression> : std::true_type { };
    template <> struct is_atomic_expression<QuoteExpression> : std::true_type { };
    
    template <typename T>
    inline constexpr bool is_atomic_expression_v = is_atomic_expression<T>::value;

    ///////////////////////////////
    // Pattern Matching Traits
    ///////////////////////////////
    
    template <typename T>
    struct is_pattern_variable : std::false_type { };
    
    template <> 
    struct is_pattern_variable<AtomExpression> : std::true_type { };

    template <typename T>
    struct is_literal_matchable : std::false_type { };
    
    template <> 
    struct is_literal_matchable<AtomExpression> : std::true_type { };

    template <typename T>
    struct is_list_like : std::false_type { };
    
    template <> struct is_list_like<ListExpression> : std::true_type { };
    template <> struct is_list_like<sExpression> : std::true_type { };
    template <> struct is_list_like<BeginExpression> : std::true_type { };
    
    template <typename T>
    inline constexpr bool is_list_like_v = is_list_like<T>::value;

    // Helper for checking if a type has variadic capability
    template <typename T, typename = void>
    struct has_variadic : std::false_type { };
    
    template <typename T>
    struct has_variadic<T, std::void_t<decltype(std::declval<T>().isVariadic)>> 
        : std::true_type { };
    
    template <typename T>
    inline constexpr bool has_variadic_v = has_variadic<T>::value;

    ///////////////////////////////
    // Element Access Traits
    ///////////////////////////////
    
    // Primary template for expression elements
    template <typename T, typename = void>
    struct get_elements {
        static std::vector<std::shared_ptr<Expression>> get(const T&) { 
            return std::vector<std::shared_ptr<Expression>> {}; 
        }
    };

    // Specialization for nested expressions
    template <typename T>
    struct get_elements<T, std::enable_if_t<has_nested_expressions_v<T>>> {
        static std::vector<std::shared_ptr<Expression>> get(const T& expr)
        {
            if constexpr (std::is_same_v<T, ListExpression> || 
                         std::is_same_v<T, sExpression>) {
                return expr.elements;
            } 
            else if constexpr (std::is_same_v<T, BeginExpression>) {
                return expr.body;
            } 
            else if constexpr (std::is_same_v<T, LetExpression>) {
                std::vector<std::shared_ptr<Expression>> all_elements;
                for (const auto& binding : expr.arguments) {
                    all_elements.push_back(binding.second);
                }
                all_elements.insert(all_elements.end(), 
                                  expr.body.begin(), 
                                  expr.body.end());
                return all_elements;
            }
            return std::vector<std::shared_ptr<Expression>> {};
        }
    };

    // Helper function to get elements
    template <typename T>
    std::vector<std::shared_ptr<Expression>> get_expression_elements(const T& expr) {
        return get_elements<T>::get(expr);
    }

} // namespace traits
} // namespace expr
