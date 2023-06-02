#[allow(warnings, clippy::all)]
mod bindings;
mod convert;
mod repr;

use std::{ffi::CString, path::PathBuf};

pub use bindings::{free_ast, AstNode_T};
pub use repr::{AstRepr as Ast, NodeError};

use crate::ast::Script;

extern "C" {
    fn run_ast(root: *mut AstNode_T, print_fns: bool, print_main: bool);
    fn parse_stdin() -> *mut AstNode_T;
    fn parse_path(path: *const i8) -> *mut AstNode_T;
}

pub fn run_script(script: Script, print_fns: bool, print_main: bool) {
    let ptr = script.to_ptr();
    unsafe {
        // Safety:
        // `Script::to_ptr` creates a valid `*mut AstNode_T` which is the only
        // requirement for `run_ast`
        run_ast(ptr, print_fns, print_main);

        // Safety:
        // `Script::to_ptr` creates a valid `*mut AstNode_T` that is allocated via C,
        // and must be therefor freed via `free_ast`
        bindings::free_ast(ptr);
    };
}

pub fn parse_script(path: Option<PathBuf>) -> Result<Script, NodeError> {
    let path = path
        .as_ref()
        .and_then(|x| x.to_str())
        .map(|path| CString::new(path).expect("There should be no null bytes in the path."));

    // Safety:
    // + Here we trust that `path` is never modified, which I know it isn't because
    //   I implemented it :D
    let node = match path {
        Some(p) => unsafe { parse_path(p.as_ptr()) },
        None => unsafe { parse_stdin() },
    };

    // Safety:
    // + Node was obtained via Bison / Flex and must therefore be
    //   validly constructed as per `Script::from_ptr`
    unsafe { Script::from_ptr(node) }
}

impl Script {
    pub fn to_ptr(self) -> *mut AstNode_T {
        convert::Convert::convert(self)
    }
}
