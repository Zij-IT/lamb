#ifndef TYPES_H
#define TYPES_H

#include <stdbool.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef float f32;
typedef double f64;

typedef char *string;
typedef char *str;

typedef enum ParseResult_E {
    ParseResultAccept,
    ParseResultReject,
} ParseResult;

enum Order {
    OrderLess = -1,
    OrderEqual = 0,
    OrderGreater = 1,
};

#endif // TYPES_H
