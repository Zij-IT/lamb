# Values

This chapter will go through some of the types available in Lamb.

## Nil

---

`nil` is the simplest value in Lamb as it is both a type, as well as a value. `nil` is the default value of a block that did not end with an expression, or a function that doesn't explicitly return a value. It's similar to `void` from C except that it is also a value.

```
assert_eq(nil, {});
assert_eq(nil, if true {});
assert_eq(nil, case 1 {});
assert_eq(nil, println());
```

## Boolean

---

Booleans like in other languages are either of the value `true` or `false`.

```
assert_eq(true, !false);
assert_eq(false, !true);
```

## Numbers

---

Unlike most other languages of this size, Lamb makes a hard distinction between floating point numbers and integers. They cannot be used interchangeably, and attempting to do so will result in a type error. Numbers look just as in other languages:

```
-- Integer Literals
0b123 -- Binary
0x123 -- Hex
0123  -- Octal
123   -- Decimal

-- Float Literals
1.2
1.2e3
```

## Char

---

A `char` is a [unicode scalar value](https://www.unicode.org/glossary/#unicode_scalar_value) and therefor 4 bytes in length. Because they are unicode scalar values, they may not be the item that most people expect. For example, `é` is actually two characters, and as such would be two `char` in Lamb., namely `'\u{0065}'` and `'\u{0301}'`

```
'h'
'e'
'l'
'l'
'o'
```

### Escaping

Escaping in Lamb is currently done with the `:` character, so to represent a newline char in Lamb, one must write: `':n'`.

```
'::' -- Results in :
':n' -- Results in \n
':r' -- Results in \r
':t' -- Results in \t
':'' -- Results in '
```

## Strings

---

A string is simply a list of `char` and can be constructed using the double-quote character:

```
"This is a string"
"This is also a string"
```

## Functions

---

Functions are just like other values, and can be passed around as variables to be used. They follow the syntax specified in the previous chapter: `[rec] fn(<args>) -> <expr>`. The `rec`keyword is optional and denotes that a function is recursive,and that the name should be visible within the body of the function.

```
-- This will fail because the function is not marked as recursive and so `fib` is
-- not defined
fib := fn(i) -> case i {
  0 | 1 -> 1,
  _ -> fib(i - 1) + fib(i - 2),
};

-- Now it will work!
fib := rec fn(i) -> case i {
  0 | 1 -> 1,
  _ -> fib(i - 1) + fib(i - 2),
};
```

Functions in Lamb are able to capture items from outside their scope, and because of this are actually closures. If an item cannot be located in the current scope, Lamb will walk backwards until it can find an item with the same name. If it cannot, it will assume the value is a global variable.

## Array

---

An array is a collection of any other kind of values. Unlike in other languages, those values are not required to share a type. Thus, the following are all valid arrays:

```
[1, nil, "hi"]
[1, 2, 3]
[fn(i) -> i + 1, fn(i) -> i - 1]
```

To access an element in an array, you can use the postfix index operator `[]`, like in C:

```
my_array[0]
my_array[{ x := 1; x - 1}]
my_array[if cond { 0 } else { 1 }]
```
