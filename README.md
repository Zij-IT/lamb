# Lamb

A _WIP_ dynamically-typed functional programming language with enough features to work.

Here is a solution to the first [Advent of Code](https://adventofcode.com/2015/day/1) puzzle from 2015:

```Lamb
part_one := rec fn(xs) -> case xs {
  ['(', rest @ ..] -> 1 + part_one(rest),
  [')', rest @ ..] -> -1 + part_one(rest),
  [] -> 0,
};

print("Part One:: ");
input $> part_one .> println;
```

More examples can be found in the [examples](./examples) folder.

## Installing Lamb

Please see the [releases page](https://github.com/zij-it/lamb/releases) for instructions how to install Lamb, or download and install it from source using `cargo`.

## Goals

I have no farfetched hopes for Lamb to being a production language. This project is being developed for
personal use. My goal is to just make it good enough for most Advent of Code problems, and also in combination with a debugger to serve as an intro to virtual machines

## Features

So, Lamb features the necessities for simple programs. This includes:

- Built-in list type
- Recursion
- Pattern matching (`case`)

In the future I would like to be able to add the following:

- [ ] A TUI debugger for the VM
- [ ] A strong static type system (Hindely-Milner)
- [ ] Module System
  - [ ] Rust-like modules
  - [ ] Rust-like project structure
  - [ ] Bellweather manager (`bell`)
- [ ] Combined data types
  - [ ] Sum Types
  - [ ] Product Types 
  - [ ] Generic Types 
  - [ ] Type Aliases
- [ ] Optimizations
  - [ ] Constantant Propagation
  - [ ] Constant Folding
  - [ ] ...
  - [ ] Dead-Code Removal
- [ ] Pattern exhaustivity checking

## Resources and Tools

- [Lamb Book](https://zij-it.github.io/lamb) contains an up-to-date book for the Lamb language
- [tools/tree-sitter-lamb](tools/tree-sitter-lamb) contains a tree-sitter grammar for use with editors that are able to use this for syntax highlighting.
