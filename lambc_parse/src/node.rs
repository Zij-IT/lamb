use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};

#[derive(Clone, Debug)]
pub struct Node<T, M> {
    item: T,
    meta: M,
}

impl<T, M> Node<T, M> {
    pub fn new(item: T, meta: M) -> Self {
        Self { item, meta }
    }

    pub fn into_inner(self) -> T {
        self.item
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
