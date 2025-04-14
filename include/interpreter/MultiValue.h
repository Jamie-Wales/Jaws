
#pragma once

#include <vector>
class SchemeValue;

struct MultiValue {
    std::vector<SchemeValue> values;
    MultiValue() = default;
    explicit MultiValue(std::vector<SchemeValue> vals);
    MultiValue(const MultiValue&) = default;
    MultiValue(MultiValue&&) noexcept = default;
    MultiValue& operator=(const MultiValue&) = default;
    MultiValue& operator=(MultiValue&&) noexcept = default;
    ~MultiValue() = default;
};
