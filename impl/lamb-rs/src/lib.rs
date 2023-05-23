use std::ffi::CString;

use clap::Parser;
use cli::{CSafeCli, Cli};

mod cli;
mod wrap;

/// # Safety
/// The caller must ensure that the pointer points to a validly constructed AstNode
/// which respects the structure generated by the current Flex / Bison definition.
#[no_mangle]
pub unsafe extern "C" fn pretty_print(x: *mut wrap::AstNode_T) {
    let repr = wrap::AstRepr::from_ptr(x);
    println!("{repr:#?}");
}

#[no_mangle]
pub extern "C" fn parse_options() -> CSafeCli {
    Cli::parse().into()
}

/// # Safety
/// The same invariants from `CString::from_raw` must hold true
#[no_mangle]
pub unsafe extern "C" fn drop_options(opts: CSafeCli) {
    if !opts.path.is_null() {
        let _cstring = CString::from_raw(opts.path.cast_mut());
    }
}
