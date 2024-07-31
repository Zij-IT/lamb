use crate::name_res::Var;

use super::{unification::TypeError, FnType, Type, TypeScheme};

type RawType = lambc_parse::Type<Var>;
type RawNamed = lambc_parse::NamedType<Var>;
type RawFnType = lambc_parse::FnType<Var>;

#[derive(Clone, Default)]
pub struct TypeEnv {
    inner: im::HashMap<Var, Type>,
}

impl TypeEnv {
    pub fn add_type(&mut self, var: Var, ty: Type) {
        assert!(
            self.inner.insert(var, ty).is_none(),
            "A var had it's type redefined... this should not be possible"
        );
    }

    pub fn get_type(&self, var: Var) -> Result<Type, TypeError> {
        self.inner.get(&var).cloned().ok_or(TypeError::UnknownType)
    }
}

pub struct TypeParser<'a> {
    env: &'a mut TypeEnv,
}

impl<'a> TypeParser<'a> {
    pub fn new(env: &'a mut TypeEnv) -> TypeParser<'a> {
        Self { env }
    }

    pub fn parse_scheme(
        &mut self,
        raw: &RawType,
    ) -> Result<TypeScheme, TypeError> {
        todo!()
    }

    fn parse_type(&self, raw: &RawType) -> Result<Type, TypeError> {
        match raw {
            RawType::Fn(fntype) => {
                let lambc_parse::FnType { args, gens, ret_type, span } =
                    &**fntype;

                assert!(gens.is_none());

                let args = args
                    .into_iter()
                    .map(|a| self.parse_type(a))
                    .collect::<Result<_, _>>()?;

                let ret_type = ret_type
                    .as_ref()
                    .map_or(Ok(Type::NIL), |ret| self.parse_type(ret))?;

                Ok(Type::Fun(FnType { args, ret_type: Box::new(ret_type) }))
            }
            RawType::Named(named) => {
                let RawNamed { name, gens, span: _ } = &**named;
                assert!(gens.is_none());
                self.env.get_type(*name)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use lambc_parse::Span;

    use super::*;
    use pretty_assertions::assert_eq;

    const SPAN: Span = Span::new(0, 0);

    #[test]
    fn parses_simple_named_type() {
        let mut env = TypeEnv::default();
        env.add_type(Var::INT, Type::INT);
        let parser = TypeParser::new(&mut env);

        let typ = RawType::Named(Box::new(RawNamed {
            name: Var::INT,
            gens: None,
            span: SPAN,
        }));

        let int = parser.parse_type(&typ);
        assert_eq!(int, Ok(Type::INT));
    }

    #[test]
    fn parses_simple_fn_type() {
        let mut env = TypeEnv::default();
        env.add_type(Var::INT, Type::INT);

        let parser = TypeParser::new(&mut env);

        let raw_int = RawType::Named(Box::new(RawNamed {
            name: Var::INT,
            gens: None,
            span: SPAN,
        }));

        let raw_fn = RawType::Fn(Box::new(RawFnType {
            args: vec![raw_int.clone(), raw_int.clone()],
            ret_type: Some(raw_int),
            gens: None,
            span: SPAN,
        }));

        assert_eq!(
            parser.parse_type(&raw_fn),
            Ok(Type::fun(vec![Type::INT, Type::INT], Type::INT))
        );
    }

    #[test]
    fn parses_nested_simple_fn_type() {
        let mut env = TypeEnv::default();
        env.add_type(Var::INT, Type::INT);

        let parser = TypeParser::new(&mut env);

        let raw_int = RawType::Named(Box::new(RawNamed {
            name: Var::INT,
            gens: None,
            span: SPAN,
        }));

        let raw_fn = RawType::Fn(Box::new(RawFnType {
            args: vec![raw_int.clone()],
            ret_type: Some(raw_int.clone()),
            gens: None,
            span: SPAN,
        }));

        let nested_raw_fn = RawType::Fn(Box::new(RawFnType {
            args: vec![raw_fn, raw_int.clone()],
            ret_type: Some(raw_int),
            gens: None,
            span: SPAN,
        }));

        assert_eq!(
            parser.parse_type(&nested_raw_fn),
            Ok(Type::fun(
                vec![Type::fun(vec![Type::INT], Type::INT), Type::INT],
                Type::INT
            ))
        );
    }
}
