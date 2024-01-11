pub mod ref_count;
use std::collections::HashMap;

use crate::value::LambString;

pub struct LambGc {
    strings: HashMap<String, ref_count::Gc<LambString>>,
}

impl Default for LambGc {
    fn default() -> Self {
        Self::new()
    }
}

impl LambGc {
    pub fn new() -> Self {
        Self {
            strings: Default::default(),
        }
    }

    pub fn intern(&mut self, s: impl Into<String>) -> ref_count::Gc<LambString> {
        let s = s.into();
        if let Some(s) = self.strings.get(&s) {
            s.clone()
        } else {
            let str = LambString::new(s.clone());
            let str_ref = ref_count::Gc::new(str);
            self.strings.insert(s, str_ref.clone());
            str_ref
        }
    }
}
