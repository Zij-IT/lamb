## Version 0.2.0

### Features

* Add a type-checker which enforces strict and static typing, meaning:

  * functions can only be called if the proper number of arguments are supplied

  * functions can only be called if the arguments have the proper types

  * the following operators only function with integers on the left and right of the operator:

    * `<<` `>>` `|` `&` `^` and `~`

  * the following operators only function with numeric values on the left and right of the operator (both must be the same):

    * `*` `+` `-` `/`

  * lists can also be concatenated with the `+` operator

* Add type-inference for local variables. Global variables still require type annotations.

### Fixes

* [#26](https://github.com/Zij-IT/lamb/issues/26) Recursive statements not handled as such during name resolution

### Breaking Changes

* `examples/poor_mans_objects.lb` isn't able to run due to untypeable method-function

* Lamb is now a statically typed language, and so any previously valid program that cannot be typed is broken

* the `::` operator has been removed and is no longer recognized

## Version 0.1.1

* Add a fragile import system to Lamb
* Add a CI for cargo-dist
* Fix `print` when used within the REPL

