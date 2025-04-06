#pragma once
#include "Token.h"
#include <atomic>
#include <set>
#include <string>
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
        newMarks.insert(generateFreshScopeID());
        return SyntaxContext(newMarks);
    }

    SyntaxContext addMarks(const std::set<ScopeID>& newMarks) const
    {
        SyntaxContext next = *this;
        next.marks.insert(newMarks.begin(), newMarks.end());
        return next;
    }
    SyntaxContext addMark(ScopeID mark) const
    {
        SyntaxContext next = *this;
        next.marks.insert(mark);
        return next;
    }

    bool isCompatibleWith(const SyntaxContext& other) const
    {
        // #TODO: this needs work
        //
        if (marks.empty() || other.marks.empty()) {
            return true;
        }
        for (ScopeID mark : marks) {
            if (other.marks.count(mark)) {

                return true;
            }
        }
        return false;
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
