use std::collections::HashMap;

use crate::{gc::Gc, value::LambString};

pub struct Interner {
    strings: HashMap<String, Gc<LambString>>,
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

impl Interner {
    pub fn new() -> Self {
        Self {
            strings: Default::default(),
        }
    }

    pub fn intern(&mut self, s: impl Into<String>) -> Gc<LambString> {
        let s = s.into();
        if let Some(s) = self.strings.get(&s) {
            s.clone()
        } else {
            let str = LambString::new(s.clone());
            let str_ref = Gc::new(str);
            self.strings.insert(s, str_ref.clone());
            str_ref
        }
    }
}
