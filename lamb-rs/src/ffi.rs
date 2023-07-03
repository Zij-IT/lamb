#[allow(warnings, clippy::all)]
mod bindings;
mod convert;

use crate::ast::Script;
use bindings::AstNode_T;
use convert::Convert;

#[cfg(test)]
mod repr;

#[cfg(test)]
use repr::AstRepr as Ast;

#[allow(dead_code)]
pub fn run_script(script: &Script, print_fns: bool, print_main: bool) {
    let ptr = script.convert();
    unsafe {
        // Safety:
        // `Script::to_ptr` creates a valid `*mut AstNode_T` which is the only
        // requirement for `run_ast`
        bindings::run_ast(ptr, print_fns, print_main);

        // Safety:
        // `Script::to_ptr` creates a valid `*mut AstNode_T` that is allocated via C,
        // and must be therefor freed via `free_ast`
        bindings::free_ast(ptr);
    };
}
