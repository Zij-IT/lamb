use super::Typ;

#[derive(Debug, PartialEq)]
pub enum Constraint {
    Eq(Typ, Typ),
}
