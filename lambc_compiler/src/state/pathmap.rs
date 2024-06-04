use std::{
    ops::Deref,
    path::{Path, PathBuf},
};

use slotmap::SlotMap;

slotmap::new_key_type! {
    pub struct PathRef;
}

pub struct PathMap {
    inner: SlotMap<PathRef, PathBuf>,
}

impl PathMap {
    pub fn new() -> Self {
        Self { inner: SlotMap::default() }
    }

    pub fn insert<P: Into<PathBuf>>(&mut self, path: P) -> PathRef {
        let path = path.into();
        let path = path.canonicalize().unwrap_or(path);
        self.inner.insert(path.into())
    }

    pub fn resolve(&mut self, pref: PathRef) -> Option<&Path> {
        self.inner.get(pref).map(Deref::deref)
    }
}
