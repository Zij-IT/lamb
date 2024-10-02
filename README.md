# Lamb

A _WIP_ statically-typed functional programming language with just enough features to work.

Here is a solution to the first [Advent of Code](https://adventofcode.com/2015/day/1) puzzle from 2015:

```Lamb
def input: list[usv] = <YOUR_INPUT>;

def part_one
  : list[usv] -> int
  = rec fn(xs) -> case xs {
    ['(', rest @ ..] -> 1 + part_one(rest),
    [')', rest @ ..] -> -1 + part_one(rest),
    [] -> 0,
  };

  print("Part One:: ");
  input $> part_one .> println;
```

More examples can be found in the [examples](./examples) folder (though these are only updated per release, and not in between).

## Installing Lamb

Please see the [releases page](https://github.com/zij-it/lamb/releases) for instructions how to install Lamb, or download and install it from source using `cargo`. If downloading from source, make sure to use the same commit as from the latest release, as features and bug-fixes in the new features are being hammered out.

## Goals

Lamb is a playground for me to apply techniques that I have picked up while learning, such as Pratt Parsing. Additionally it serves as a long-term project that I can always turn to to learn more, whether that be about programming, project management or project structure. There is _SO_ much out there to learn about, and within this project I get to apply what I learn. Lamb is not intended to be a production level language, but a language for me to use and enjoy. I also hope that the project can be a place where someone unfamiliar with writing a programming language can learn how it can be done. To this purpose there is a list below of resources that I have used while developing this project.

## Features

Lamb is a relatively simple language with the following notable features:
- Recursion using `rec`
- Control flow with `if` and `case`
- Types (`int`, `usv`, `list[t]`, ...)
- Imports (`from "<PATH>" import { item_name as alias };` with `export { item_name as alias };`)
- Not completely trash error messages!

In the future I would like to be able to add the following:

- [ ] A TUI debugger for the VM
- [x] A strong static type system (Bidirectional Type-Checking)
- [ ] Module System
  - [ ] Rust-like modules
  - [ ] Rust-like project structure
  - [ ] Bellweather manager (`bell`)
- [ ] Combined data types
  - [ ] Sum Types
  - [ ] Product Types 
  - [x] Generic Types 
  - [ ] Type Aliases
- [ ] Optimizations
  - [ ] Constantant Propagation
  - [ ] Constant Folding
  - [ ] ...
  - [ ] Dead-Code Removal
- [ ] Pattern exhaustivity checking

## Resources and Tools for Learning or Writing Lamb

- [Lamb Book](https://zij-it.github.io/lamb) contains an up-to-date book for the Lamb language
- [tools/tree-sitter-lamb](tools/tree-sitter-lamb) contains a tree-sitter grammar for use with editors that are able to use this for syntax highlighting.

## Resources that I've enjoyed and used while developing Lamb

- [Crafting Interpreters](https://craftinginterpreters.com/)
- [Simple but Powerfull Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
- [Typing Haskell in Haskell](https://gist.github.com/chrisdone/0075a16b32bfd4f62b7b)
- [Making a Lanuage](https://thunderseethe.dev/series/making-a-language/)
