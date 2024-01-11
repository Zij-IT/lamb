use std::cell::RefCell;

use dumpster::Collectable;

use crate::value::{LambArray, LambClosure, LambFunc, LambString, NativeFunc, Upvalue, Value};

pub trait LambAllocable: 'static + dumpster::Collectable {}

pub struct Gc<T: LambAllocable>(dumpster::unsync::Gc<T>);

impl<T: LambAllocable> Gc<T> {
    pub fn new(item: T) -> Self {
        Self(dumpster::unsync::Gc::new(item))
    }
}

impl<T: LambAllocable> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

/// Safety: See `<dumpster::sync::Gc<T> as Collectable>::accept`
unsafe impl<T: LambAllocable> dumpster::Collectable for Gc<T> {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        self.0.accept(visitor)?;
        Ok(())
    }
}

impl<T: LambAllocable> std::ops::Deref for Gc<T> {
    type Target = <dumpster::unsync::Gc<T> as std::ops::Deref>::Target;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: LambAllocable + std::hash::Hash> std::hash::Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T: LambAllocable + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        *self.0 == *other.0
    }
}

impl<T: LambAllocable + Eq> Eq for Gc<T> {}

impl<T: LambAllocable + std::fmt::Debug> std::fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&*self.0, f)
    }
}

impl<T: LambAllocable> std::borrow::Borrow<T> for Gc<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T: LambAllocable> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

// `Value` does *NOT* get a LambAllocable impl. This one is more for my convenience :D
unsafe impl Collectable for Value {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Value::Nil => (),
            Value::Int(_) => (),
            Value::Bool(_) => (),
            Value::Char(_) => (),
            Value::Double(_) => (),
            Value::Array(arr) => arr.accept(visitor)?,
            Value::String(str) => str.accept(visitor)?,
            Value::Native(nat) => nat.accept(visitor)?,
            Value::Closure(clo) => clo.accept(visitor)?,
        }

        Ok(())
    }
}

impl LambAllocable for LambString {}
unsafe impl Collectable for LambString {
    fn accept<V: dumpster::Visitor>(&self, _: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

impl LambAllocable for LambArray {}
unsafe impl Collectable for LambArray {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        for item in self.into_iter() {
            item.accept(visitor)?;
        }

        Ok(())
    }
}

impl LambAllocable for NativeFunc {}
unsafe impl Collectable for NativeFunc {
    fn accept<V: dumpster::Visitor>(&self, _: &mut V) -> Result<(), ()> {
        Ok(())
    }
}

impl LambAllocable for LambClosure {}
unsafe impl Collectable for LambClosure {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        self.func.accept(visitor)?;
        self.upvalues.accept(visitor)?;
        Ok(())
    }
}

impl LambAllocable for LambFunc {}
unsafe impl Collectable for LambFunc {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        let Self {
            arity: _,
            chunk,
            name,
            upvalues: _,
        } = self;

        name.accept(visitor)?;
        for value in &chunk.constants {
            value.accept(visitor)?;
        }

        Ok(())
    }
}

impl LambAllocable for Upvalue {}
unsafe impl Collectable for Upvalue {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        let Self { index: _, closed } = self;
        closed.accept(visitor)?;
        Ok(())
    }
}

impl<T: LambAllocable> LambAllocable for RefCell<T> {}
