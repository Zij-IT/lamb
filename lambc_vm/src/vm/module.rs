use std::collections::HashMap;

use crate::{
    gc::{GcRef, LambGc},
    value::{Str, Value},
};

/// A module export whose name and alias are represented via [`GcRef<Str>`](GcRef)
pub struct ModuleExport {
    pub name: GcRef<Str>,
    pub alias: Option<GcRef<Str>>,
}

/// A representation of the internal parts of a module  that are required
/// by the virtual machine.
#[derive(Debug)]
pub struct Module {
    globals: HashMap<GcRef<Str>, Value>,
    exports: HashMap<GcRef<Str>, Value>,
}

impl Module {
    /// Constructs a new empty module
    pub fn new() -> Self {
        Self { globals: Default::default(), exports: Default::default() }
    }

    /// Adds the exports within `exports` into the module so they can later be
    /// accessed via [`get_export`](Self::get_export)
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

    /// Define a global variable within a module
    pub fn define_global(&mut self, name: GcRef<Str>, value: Value) {
        self.globals.insert(name, value);
    }

    // Used for `Op::GetGlobal`
    /// Gets a global variable from within a module via the name `item`
    pub fn get_global(&self, item: GcRef<Str>) -> Option<Value> {
        self.globals.get(&item).copied()
    }

    // Used for `Op::Access`
    /// Gets an exported variable via the name `item`
    pub fn get_export(&self, item: GcRef<Str>) -> Option<Value> {
        self.exports.get(&item).copied()
    }

    /// Marks all items within the module so that they aren't collected
    /// by the garbage collector during the next collection.
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
