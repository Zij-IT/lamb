# Syntax

Lamb's syntax most closely resembles that of the Haskell-like languages, but introduces a more familiar C-style like feel through its use of blocks and brace delimiters.

Scripts are stored in `.lb` or `.lamb` files. These files are then fed from the lexer to the parser, and from the parser to the compiler to be turned into bytecode.

## Comments

---

Lamb only features lime comments, and these begin with a `--`:

```
-- This is a line comment
```

## Reserved Words

---

Lamb doesn't feature many reserved words: 

```
fn case if elif else return struct enum rec mod use
```

So... just don't go using these for variable names.

## Identifiers

---

Identifiers in Lamb are able to be a series of numbers, letters or underscores. Ergo, the following are possible identifiers:

```
x
_
X_123
x_123
_x_123  
```

Identifiers are case sensitive in Lamb, though choosing between camelCase and snake_case is a matter of personal preference.

## Blocks

---

Blocks in Lamb are denoted by `{` `}` and are allowed to contain any series of statements followed by an expression. In fact, blocks themselves are expressions, and when the final item in a block is a statement, they evaluate to `nil`.

```
block := {
  print("hello");
  print(" world");
  println(" I am writing from Lamb!");
  "And the value of this block is this string!"
}; 
```

## Function Syntax

---

Function definitions in Lamb take the form `fn(<args>) -> <expr>`. This allows them to either be in the short form, like:

```
inc := fn(x) -> x + 1;  
```

or the more familiar form (from the perspective of a C developer):

```
inc := fn(x) -> {
  x + 1
};  
```

Because blocks are just expressions, you can omit a return statement in a block if you would like, though you can of course still use one:

```
inc := fn(x) -> {
  return x + 1;
};  
```

## Precedence and Associativity

---

Most operators in Lamb follow C, with the exception that the bit operators have higher precedence than the comparison operators. 

Operators                  | Description                     | Precedence | Associativity
---------------------------|---------------------------------|------------|--------------
<code>- ~ !</code>         | Negate, Compliment, Logical Not | 12         | Right
<code>* / %</code>         | Multiply, Divide, Modulo        | 11         | Left
<code>+</code>             | Add, Subtract                   | 10         | Left
<code><< >></code>         | Left Shift, Right Shift         | 9          | Left
<code>&</code>             | Bitwise And                     | 8          | Left
<code>^</code>             | Bitwise Xor                     | 7          | Left
<code>\|</code>            | Bitwise Or                      | 6          | Left
<code>> >= < <= = !=</code>| Comparison                      | 5          | Left
<code>\|\|</code>          | Logical Or                      | 4          | Left
<code>&&</code>            | Logical And                     | 3          | Left
<code><. .></code>         | Left Compose, Right Compose     | 2          | Left
<code><$ $></code>         | Left Apply, Right Apply         | 1          | Left
