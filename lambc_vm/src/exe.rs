use std::collections::HashMap;

use lambc_compiler::PathRef;
use lambc_parse::{Export, Import};

use crate::{
    gc::GcRef,
    value::{Closure, Str},
};

pub struct Exe {
    pub(crate) main: GcRef<Str>,
    pub(crate) modules: HashMap<GcRef<Str>, CompiledModule>,
}

pub struct CompiledModule {
    pub(crate) path: GcRef<Str>,
    pub(crate) imports: Vec<CompiledImport>,
    pub(crate) export: Option<Export>,
    pub(crate) code: GcRef<Closure>,
}

pub struct CompiledImport {
    pub(crate) raw: Import<PathRef>,
    pub(crate) path: GcRef<Str>,
}
