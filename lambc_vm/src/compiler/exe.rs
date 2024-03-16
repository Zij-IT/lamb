use std::collections::HashMap;

use lambc_parse::{Export, Import};

use crate::{
    gc::GcRef,
    value::{Closure, Str},
};

pub struct Exe {
    pub main: GcRef<Str>,
    pub modules: HashMap<GcRef<Str>, CompiledModule>,
}

pub struct CompiledModule {
    pub path: GcRef<Str>,
    pub imports: Vec<CompiledImport>,
    pub export: Option<Export>,
    pub code: GcRef<Closure>,
}

pub struct CompiledImport {
    pub raw: Import,
    pub path: GcRef<Str>,
}
