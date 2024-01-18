# Control Flow

Unlike many other scripting languages, Lamb doesn't have a concept of "truthy". As a consequence, values must be converted to booleans before being used in places where a boolean is expected, like `if` conditions.

## If Expressions

---

In Lamb, the simplest control flow  is an `if` expression. They look like this:

```
if <cond-1> {
  <body-1>
} elif <cond-2> {
  <body-2>
} else {
  <body-3>
}
```

Unlike in many languages, there are no parenthesis requires around the conditions, so `if true { } else { }` is syntactically correct.

An `if` expression evaluates to the final value expression of the branch that is taken. If there is no `else` branch, and none of the branches are taken, the expression evaluates to `nil`. 

## Case Expressions

---

When looking to test if a variable is one of a series of values, one could write:

```
if val = 1 || val = 7 {
  ...
} elif val = 4 || value = 8 {
  ...
} else {
  ...
}
```

But if we want to extend this to arrays it gets messy:

```
-- Check if an array's first two elements are 1 and 2
if len(arr) > 2 && arr[0] = 1 && arr[1] = 2 {
  ...
} else {
  ...
}
```

Lamb offers a more concise way to do this through pattern matching with `case`:

```
case val {
  1 | 7 -> {}
  4 | 8 -> {}
  _ -> {}
}

case arr {
  [1, 2, ..] -> print("Has 1 then 2"),
  _ -> print("Doesn't have 1 then 2"),
}
```

Patterns can be arbitrarily nested and you can even bind patterns to values. For example, to sum an array, one could write:

```
sum := rec fn(xs) -> case xs {
  [x, rest @ ..] -> x + sum(rest),
  [] -> 0,
};

sum([1,2,3,4]);
```

This function looks at the structure of the array, and if the array contains at least one element, assigns the first to `x`and assigns the rest of the array the value `rest`.

The syntax for a `case` expression is roughly:

```
case <value> {
  (<pattern> -> <body>)*
}
```

If `<body>` is a block expression, no `,` is necessary. Otherwise, it is required to put a `,`.

Similarly to `if`, a `case` expression evaluates to the value of the branch that is taken, or `nil` if no arms are taken.

## Loops

---

Ah loops... yeah... we don't do that here. Loops can be imitated through the use of recursion, so be prepared to have to rid your mind of loops ;)
