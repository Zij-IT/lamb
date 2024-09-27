use std::collections::HashSet;

use super::{Constraint, RigidVar, Type};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TypeScheme {
    pub unbound: HashSet<RigidVar>,
    pub constraints: Vec<Constraint>,
    pub ty: Type,
}
