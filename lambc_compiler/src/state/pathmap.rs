use std::{
    collections::{hash_map::Entry, HashMap},
    ops::Deref,
    path::{Path, PathBuf},
};

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct PathRef(usize);

pub struct PathMap {
    from_ref: HashMap<PathRef, PathBuf>,
    to_ref: HashMap<PathBuf, PathRef>,
    inserts: usize,
}

impl PathMap {
    pub fn new() -> Self {
        Self {
            from_ref: Default::default(),
            to_ref: Default::default(),
            inserts: 1,
        }
    }

    pub fn insert<P: Into<PathBuf>>(&mut self, path: P) -> PathRef {
        let path = path.into();
        let path = path.canonicalize().unwrap_or(path);

        match self.to_ref.entry(path.clone()) {
            Entry::Occupied(o) => return *o.get(),
            Entry::Vacant(v) => {
                let pref = PathRef(self.inserts);
                v.insert(pref);
                self.from_ref.insert(pref, path);
                self.inserts += 1;
                pref
            }
        }
    }

    pub fn resolve(&self, pref: PathRef) -> Option<&Path> {
        self.from_ref.get(&pref).map(Deref::deref)
    }
}

#[cfg(test)]
mod tests {
    use super::PathMap;

    #[test]
    fn same_key_same_ref() {
        let mut map = PathMap::new();
        let pref1 = map.insert("");
        let pref2 = map.insert("");
        assert_eq!(pref1, pref2);

        let path1 = map.resolve(pref1);
        let path2 = map.resolve(pref2);
        assert_eq!(path1, path2);
    }
}
