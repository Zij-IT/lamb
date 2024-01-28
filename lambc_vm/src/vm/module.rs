use std::collections::HashMap;

use crate::{
    gc::{GcRef, LambGc},
    value::{Str, Value},
};

#[derive(Debug)]
pub struct Module {
    globals: HashMap<GcRef<Str>, Value>,
    exports: HashMap<GcRef<Str>, Value>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            globals: Default::default(),
            exports: Default::default(),
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
