#include "../types.hpp"
#include "misc.hpp"

// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers,cppcoreguidelines-pro-bounds-pointer-arithmetic)
u32 hash_string(char const *st) {
    u32 hash = 2166136261;

    for (u32 i = 0; st[i] != '\0'; i++) {
        hash ^= (u8)st[i];
        hash *= 16777619;
    }

    return hash;
}
// NOLINTEND(cppcoreguidelines-avoid-magic-numbers,readability-magic-numbers,cppcoreguidelines-pro-bounds-pointer-arithmetic)
