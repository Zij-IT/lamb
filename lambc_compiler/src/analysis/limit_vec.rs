pub struct TooManyItems<T>(pub Vec<T>);

/// A `Vec<T>` with a maximum length of 255
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LimitVec<T>(Vec<T>);

impl<T> LimitVec<T> {
    pub const MAX_ELEMS: usize = 255;
    pub fn new_from_vec_truncating(mut v: Vec<T>) -> (Self, Vec<T>) {
        let extra = if v.len() > Self::MAX_ELEMS {
            v.split_off(Self::MAX_ELEMS)
        } else {
            vec![]
        };

        (Self(v), extra)
    }
}

impl<T> std::ops::Deref for LimitVec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

impl<T> std::ops::DerefMut for LimitVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut_slice()
    }
}

impl<T> TryFrom<Vec<T>> for LimitVec<T> {
    type Error = TooManyItems<T>;

    fn try_from(value: Vec<T>) -> Result<Self, Self::Error> {
        if value.len() <= 255 {
            Ok(Self(value))
        } else {
            Err(TooManyItems(value))
        }
    }
}

impl<T, Idx> std::ops::Index<Idx> for LimitVec<T>
where
    Vec<T>: std::ops::Index<Idx>,
{
    type Output = <Vec<T> as std::ops::Index<Idx>>::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        self.0.index(index)
    }
}

impl<T> IntoIterator for LimitVec<T> {
    type Item = <Vec<T> as IntoIterator>::Item;

    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a LimitVec<T> {
    type Item = <&'a Vec<T> as IntoIterator>::Item;

    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a, T> IntoIterator for &'a mut LimitVec<T> {
    type Item = <&'a mut Vec<T> as IntoIterator>::Item;

    type IntoIter = <&'a mut Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}
