#[allow(warnings, clippy::all)]
mod bindings;
mod convert;
mod repr;

use bindings::AstNode_T;
#[cfg(test)]
use repr::AstRepr as Ast;

use crate::ast::Script;

pub fn run_script(script: &Script, print_fns: bool, print_main: bool) {
    let ptr = script.to_ptr();
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

impl Script {
    pub fn to_ptr(&self) -> *mut AstNode_T {
        convert::Convert::convert(self)
    }
}
