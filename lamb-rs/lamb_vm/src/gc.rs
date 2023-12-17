use std::marker::PhantomData;

pub struct GcRef<T> {
    idx: usize,
    _type: PhantomData<T>,
}

impl<T> Copy for GcRef<T> {}

impl<T> Clone for GcRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> std::fmt::Debug for GcRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GcRef").field(&self.idx).finish()
    }
}

impl<T> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}
