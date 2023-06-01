use std::ffi::CString;

use clap::Parser;
use cli::{CSafeCli, Cli};
use optimization::Optimize;

mod cli;
mod convert;
mod optimization;
mod wrap;

/// Constructs a Rust representation of the given AST, and debug prints the
/// result to `stdout`.
///
/// # Safety
/// The caller must ensure that the pointer points to a validly constructed `AstNode_T`
/// which respects the structure generated by the current Flex / Bison definition.
#[no_mangle]
pub unsafe extern "C" fn pretty_print(x: *mut wrap::AstNode_T) {
    let script = wrap::Script::from_ptr(x);
    println!("{script:#?}");
}

#[no_mangle]
pub unsafe extern "C" fn assert_ast_parse(x: *mut wrap::AstNode_T) {
    println!("{:#?}", wrap::Ast::from_ptr(x));
}

/// Parses the arguments of the command line using `clap` and converts the
/// result to an FFI-Safe representation.
///
/// `CSafeCli` contains a raw `*const c_char`, which is provided by `CString::into_raw`.
/// This *will* result in a memory leak if the user does not call `drop_options`
#[no_mangle]
pub extern "C" fn parse_options() -> CSafeCli {
    Cli::parse().into()
}

/// Frees the memory of the `*const c_char` created by the `parse_options` function,
/// by calling `CString::from_raw`.
///
/// `CSafeCli` contains a raw `*const c_char`, which is provided by `CString::into_raw`.
/// This *will* result in a memory leak if the user does not call `drop_options`
/// # Safety
/// The same invariants from `CString::from_raw` must hold true
#[no_mangle]
pub unsafe extern "C" fn drop_options(opts: CSafeCli) {
    if !opts.path.is_null() {
        let _cstring = CString::from_raw(opts.path.cast_mut());
    }
}

/// Applies optimizations to the tree until no more are made, and returns a C version of the
/// tree, with all allocations called by C. Consumes `node`, meaning that no `C` code should
/// have any references to any parts of the `node` or it's children.
///
/// # Safety
/// The caller must ensure that the pointer points to a validly constructed `AstNode_T`
/// which respects the structure generated by the current Flex / Bison definition.
#[no_mangle]
pub unsafe extern "C" fn optimize(node: *mut wrap::AstNode_T) -> *mut wrap::AstNode_T {
    if node.is_null() {
        return node;
    }

    match wrap::Script::from_ptr(node) {
        Ok(mut script) => {
            while (&mut script.block).optimize() {
                println!("Applied optimizations!");
                println!("New tree: {script:#?}")
            }

            // Safety:
            // Given by caller of `optimize`
            crate::wrap::free_ast(node);

            script.to_ptr()
        }
        Err(_e) => node,
    }
}

#[no_mangle]
pub unsafe extern "C" fn test_all() {
    convert::test_all();
}
