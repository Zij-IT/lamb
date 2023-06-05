#[allow(warnings, clippy::all)]
mod bindings;
mod convert;
mod repr;

use std::ffi::CString;

use bindings::AstNode_T;
#[cfg(test)]
use repr::AstRepr as Ast;
use repr::NodeError;

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

pub fn parse_script<P: AsRef<std::path::Path>>(path: &Option<P>) -> Result<Script, NodeError> {
    let path = path
        .as_ref()
        .and_then(|x| x.as_ref().to_str())
        .map(|path| CString::new(path).expect("There should be no null bytes in the path."));

    // Safety:
    // + Here we trust that `path` is never modified, which I know it isn't because
    //   I implemented it :D
    let node = match path {
        Some(p) => unsafe { bindings::parse_path(p.as_ptr().cast_mut()) },
        None => unsafe { bindings::parse_stdin() },
    };

    // Safety:
    // + Node was obtained via Bison / Flex and must therefore be
    //   validly constructed as per `Script::from_ptr`
    unsafe { Script::from_ptr(node) }
}

impl Script {
    pub fn to_ptr(&self) -> *mut AstNode_T {
        convert::Convert::convert(self)
    }
}
