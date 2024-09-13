use std::collections::HashMap;

use lambc_compiler::PathRef;
use lambc_parse::{Export, Ident, Import};

use crate::{
    gc::GcRef,
    value::{Closure, Str},
};

/// An executable for the [`Vm`](crate::Vm) to run
pub struct Exe {
    pub(crate) main: GcRef<Str>,
    pub(crate) modules: HashMap<GcRef<Str>, CompiledModule>,
}

/// A module which was compiled by the Bytecode backend
// todo: replace pub with pub(crate)
pub struct CompiledModule {
    pub(crate) path: GcRef<Str>,
    pub(crate) imports: Vec<CompiledImport>,
    pub(crate) export: Option<Export<Ident>>,
    pub(crate) code: GcRef<Closure>,
}

/// An import which was compiled by the Bytecode backend
// todo: replace pub with pub(crate)
pub struct CompiledImport {
    pub(crate) raw: Import<Ident, PathRef>,
    pub(crate) path: GcRef<Str>,
}
