#include "misc.hpp"

u32 hash_string(char const* st) {
    u32 hash = 2166136261;

    for (u32 i = 0; st[i] != '\0'; i++) {
        hash ^= (u8)st[i];
        hash *= 16777619;
    }

    return hash;
}