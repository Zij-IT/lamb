#[allow(warnings, clippy::all)]
mod bindings;
mod convert;
mod repr;

pub use bindings::{free_ast, AstNode_T};
pub use convert::test_all;
pub use repr::{AstRepr as Ast, NodeError};

impl crate::ast::Script {
    pub fn to_ptr(self) -> *mut AstNode_T {
        convert::Convert::convert(self)
    }
}
