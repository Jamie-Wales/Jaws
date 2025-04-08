#pragma once
#include "Token.h"
#include <atomic>
#include <iostream>
#include <set>
#include <sstream>
#include <string>

// #define DEBUG_LOGGING
#ifdef DEBUG_LOGGING
#define DEBUG_LOG(x) std::cerr << "[DEBUG] " << x << "\n"
#else
#define DEBUG_LOG(x)
#endif

using ScopeID = unsigned long long;

inline ScopeID generateFreshScopeID()
{
    static std::atomic<ScopeID> counter = 0;
    return counter++;
}

struct SyntaxContext {
    std::set<ScopeID> marks;

    SyntaxContext() = default;

    SyntaxContext(std::set<ScopeID> m)
        : marks(std::move(m))
    {
    }

    static SyntaxContext createFresh()
    {
        std::set<ScopeID> newMarks;
        ScopeID newId = generateFreshScopeID();
        DEBUG_LOG("Creating fresh syntax context with mark: " << newId);
        newMarks.insert(newId);
        return SyntaxContext(newMarks);
    }

    SyntaxContext addMarks(const std::set<ScopeID>& newMarks) const
    {
        SyntaxContext next = *this;
        DEBUG_LOG("Adding marks to context. Original marks: " << toString());
        next.marks.insert(newMarks.begin(), newMarks.end());
        DEBUG_LOG("Resulting marks: " << next.toString());
        return next;
    }

    SyntaxContext addMark(ScopeID mark) const
    {
        SyntaxContext next = *this;
        DEBUG_LOG("Adding single mark " << mark << " to context. Original marks: " << toString());
        next.marks.insert(mark);
        DEBUG_LOG("Resulting marks: " << next.toString());
        return next;
    }

    bool isCompatibleWith(const SyntaxContext& binding_context) const
    {
        // If we're looking up an identifier with marks
        if (!marks.empty()) {
            // And the binding is local (has marks)
            if (!binding_context.marks.empty()) {
                // They must match exactly for hygiene
                bool matches = marks == binding_context.marks;
                DEBUG_LOG("Both have marks - checking exact match: " << (matches ? "true" : "false"));
                return matches;
            }
            // Binding is global, that's okay
            DEBUG_LOG("Identifier has marks but binding is global - allowing access");
            return true;
        }

        // If we're looking up an identifier without marks
        if (marks.empty()) {
            // And the binding has marks
            if (!binding_context.marks.empty()) {
                DEBUG_LOG("Unmarked identifier cannot access marked binding");
                return false;
            }
            // Neither has marks
            DEBUG_LOG("Neither has marks - allowing access");
            return true;
        }

        return false; // Shouldn't reach here
    }
    std::string toString() const
    {
        std::stringstream ss;
        ss << "[";
        bool first = true;
        for (const auto& mark : marks) {
            if (!first)
                ss << " ";
            ss << mark;
            first = false;
        }
        ss << "]";
        return ss.str();
    }

    bool operator<(const SyntaxContext& other) const
    {
        return marks < other.marks;
    }

    bool operator==(const SyntaxContext& other) const
    {
        return marks == other.marks;
    }
};

struct HygienicSyntax {
    Token token;
    SyntaxContext context;

    std::string toString() const
    {
        std::stringstream ss;
        ss << token.lexeme << " " << context.toString();
        return ss.str();
    }

    bool operator<(const HygienicSyntax& other) const
    {
        if (token.lexeme != other.token.lexeme)
            return token.lexeme < other.token.lexeme;
        return context < other.context;
    }

    bool operator==(const HygienicSyntax& other) const
    {
        return token.lexeme == other.token.lexeme && context == other.context;
    }
};

struct SetScopeIDHash {
    std::size_t operator()(const std::set<ScopeID>& s) const noexcept
    {
        std::size_t seed = s.size();
        std::hash<ScopeID> hasher;
        for (const auto& elem : s) {
            seed ^= hasher(elem) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
        return seed;
    }
};

namespace std {

template <>
struct hash<SyntaxContext> {
    std::size_t operator()(const SyntaxContext& ctx) const noexcept
    {
        return SetScopeIDHash()(ctx.marks);
    }
};

template <>
struct hash<HygienicSyntax> {
    std::size_t operator()(const HygienicSyntax& id) const noexcept
    {
        std::size_t h1 = std::hash<std::string> {}(id.token.lexeme);
        std::size_t h2 = std::hash<SyntaxContext> {}(id.context);
        return h1 ^ (h2 << 1);
    }
};

} // namespace std
