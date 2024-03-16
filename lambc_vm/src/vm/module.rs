use std::collections::HashMap;

use crate::{
    gc::{GcRef, LambGc},
    value::{Str, Value},
};

pub struct ModuleExport {
    pub name: GcRef<Str>,
    pub alias: Option<GcRef<Str>>,
}

#[derive(Debug)]
pub struct Module {
    globals: HashMap<GcRef<Str>, Value>,
    exports: HashMap<GcRef<Str>, Value>,
}

impl Module {
    pub fn new() -> Self {
        Self { globals: Default::default(), exports: Default::default() }
    }

    pub fn build_exports<I>(&mut self, exports: I)
    where
        I: Iterator<Item = ModuleExport>,
    {
        for ModuleExport { name, alias } in exports {
            let item = self.get_global(name).unwrap();
            let export_name = match alias {
                Some(alias) => alias,
                None => name,
            };

            self.exports.insert(export_name, item);
        }
    }

    pub fn define_global(&mut self, name: GcRef<Str>, value: Value) {
        self.globals.insert(name, value);
    }

    // Used for `Op::GetGlobal`
    pub fn get_global(&self, item: GcRef<Str>) -> Option<Value> {
        self.globals.get(&item).copied()
    }

    // Used for `Op::Access`
    pub fn get_export(&self, item: GcRef<Str>) -> Option<Value> {
        self.exports.get(&item).copied()
    }

    pub fn mark_items(&self, gc: &mut LambGc) {
        for (k, value) in &self.globals {
            gc.mark_object(*k);
            gc.mark_value(*value);
        }

        for (k, v) in &self.exports {
            gc.mark_object(*k);
            gc.mark_value(*v);
        }
    }
}
