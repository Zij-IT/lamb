use std::{
    cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd},
    ops::{Deref, DerefMut},
};

#[derive(Clone, Debug)]
pub struct Node<T, M> {
    item: T,
    meta: M,
}

impl<T, M> Node<T, M> {
    pub fn new(item: T, meta: M) -> Self {
        Self { item, meta }
    }

    pub fn inner(&self) -> &T {
        &self.item
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.item
    }

    pub fn into_inner(self) -> T {
        self.item
    }
}

impl<T, M> Deref for Node<T, M> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T, M> DerefMut for Node<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner_mut()
    }
}

impl<T: PartialEq, M> PartialEq for Node<T, M> {
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item
    }
}

impl<T: Eq, M> Eq for Node<T, M> {}

impl<T: Ord, M> Ord for Node<T, M> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.item.cmp(&other.item)
    }
}

impl<T: PartialOrd, M> PartialOrd for Node<T, M> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.item.partial_cmp(&other.item)
    }
}
