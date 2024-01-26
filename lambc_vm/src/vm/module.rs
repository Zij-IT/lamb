use std::collections::HashMap;

use crate::{
    gc::{GcRef, LambGc},
    value::{Str, Value},
};

#[derive(PartialEq, Eq, Debug, thiserror::Error)]
pub enum Error {
    #[error("The item is not exported by the module.")]
    IsPrivate,
    #[error("The item is not contained within the module.")]
    NotInModule,
    #[error("Unable to import the item. The module is not finished loading.")]
    ModuleNotLoaded,
    #[error("The item is exported by the module, but under a different name")]
    NotExposedByThisName,
}

pub struct Exportable {
    name: GcRef<Str>,
    alias: Option<GcRef<Str>>,
}

pub struct Module {
    inner: ModuleInner,
}

impl Module {
    pub fn new(exports: Vec<Exportable>) -> Self {
        Self {
            inner: ModuleInner::Loading { exports },
        }
    }

    // Used for `Op::GetGlobal`
    pub fn get_global(&self, item: GcRef<Str>) -> Result<Value, Error> {
        self.get(item).map(|i| i.value)
    }

    // Used for `Op::GetModuleItem`
    pub fn get_export(&self, item: GcRef<Str>) -> Result<Value, Error> {
        self.get(item).and_then(|export| {
            if export.is_public {
                Ok(export.value)
            } else {
                Err(Error::IsPrivate)
            }
        })
    }

    fn get(&self, item: GcRef<Str>) -> Result<ModuleItem, Error> {
        match &self.inner {
            ModuleInner::Finished { items, exports } => {
                if let Some(t) = items.get(&item) {
                    Ok(*t)
                } else if exports.iter().any(|(name, _)| *name == item) {
                    Err(Error::NotExposedByThisName)
                } else {
                    Err(Error::NotInModule)
                }
            }
            ModuleInner::Loading { exports } => {
                if let Some(export) = exports.iter().find(|i| i.name == item) {
                    if export.alias.is_some() {
                        Err(Error::NotExposedByThisName)
                    } else {
                        Err(Error::NotInModule)
                    }
                } else {
                    Err(Error::NotInModule)
                }
            }
        }
    }

    pub fn mark_items(&self, gc: &mut LambGc) {
        match &self.inner {
            ModuleInner::Loading { exports } => {
                for Exportable { name, alias } in exports {
                    gc.mark_object(*name);
                    if let Some(alias) = alias {
                        gc.mark_object(*alias);
                    }
                }
            }
            ModuleInner::Finished { items, exports } => {
                for (k, ModuleItem { value, .. }) in items {
                    gc.mark_object(*k);
                    gc.mark_value(*value);
                }

                for (k, v) in exports {
                    gc.mark_object(*k);
                    if let Some(alias) = v {
                        gc.mark_object(*alias);
                    }
                }
            }
        }
    }
}

enum ModuleInner {
    Loading {
        exports: Vec<Exportable>,
    },
    Finished {
        items: HashMap<GcRef<Str>, ModuleItem>,
        exports: HashMap<GcRef<Str>, Option<GcRef<Str>>>,
    },
}

#[derive(Copy, Clone)]
struct ModuleItem {
    value: Value,
    is_public: bool,
}
