#ifndef TYPES_H
#define TYPES_H

#include <cstdint>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;

using f32 = float;
using f64 = double;

using string = char *;
using str = char *;

enum ParseResult {
    ParseResultAccept,
    ParseResultReject,
};

enum Order {
    OrderLess = -1,
    OrderEqual = 0,
    OrderGreater = 1,
};

#endif // TYPES_H
