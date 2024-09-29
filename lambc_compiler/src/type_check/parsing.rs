use crate::name_res::Var;

use std::collections::HashSet;

use super::{Error, FnType, Result, RigidVar, Type, TypeScheme};

type RawType = lambc_parse::Type<Var>;
type RawNamed = lambc_parse::NamedType<Var>;
type RawFnType = lambc_parse::FnType<Var>;

pub trait ParserContext {
    fn get_type(&mut self, var: Var) -> Result<Type>;
    fn add_type(&mut self, var: Var, ty: Type);
    fn new_rigid_var(&mut self) -> RigidVar;
}

pub struct TypeParser<P> {
    ctx: P,
}

impl<P: ParserContext> TypeParser<P> {
    pub fn new(ctx: P) -> TypeParser<P> {
        Self { ctx }
    }

    pub fn parse_scheme(&mut self, raw: &RawType) -> Result<TypeScheme> {
        match raw {
            RawType::Fn(fntype) => {
                let mut unbound = HashSet::new();
                if let Some(gens) = fntype.gens.as_ref() {
                    for gen in &gens.params {
                        let rigid = self.ctx.new_rigid_var();
                        unbound.insert(rigid);
                        self.ctx.add_type(gen.id, Type::RigidVar(rigid));
                    }
                }

                Ok(TypeScheme {
                    unbound,
                    constraints: vec![],
                    ty: self.parse_fn(fntype, true)?,
                })
            }
            RawType::Named(named) => Ok(TypeScheme {
                unbound: Default::default(),
                constraints: vec![],
                ty: self.parse_named(named)?,
            }),
        }
    }

    fn parse_type(&mut self, raw: &RawType) -> Result<Type> {
        match raw {
            RawType::Fn(fntype) => self.parse_fn(fntype, false),
            RawType::Named(named) => self.parse_named(named),
        }
    }

    fn parse_fn(
        &mut self,
        fntype: &RawFnType,
        allow_generics: bool,
    ) -> Result<Type> {
        let lambc_parse::FnType { args, gens, ret_type, span: _ } = fntype;

        if gens.as_ref().is_some_and(|gens| !gens.params.is_empty())
            && !allow_generics
        {
            return Err(Error::NewTypeNotAllowed);
        }

        let args =
            args.iter().map(|a| self.parse_type(a)).collect::<Result<_>>()?;

        let ret_type = ret_type
            .as_ref()
            .map_or(Ok(Type::NIL), |ret| self.parse_type(ret))?;

        Ok(Type::Fun(FnType { args, ret_type: Box::new(ret_type) }))
    }

    fn parse_named(&mut self, named: &RawNamed) -> Result<Type> {
        let RawNamed { name, gens, span: _ } = named;
        let generic_count = gens.as_ref().map_or(0, |g| g.params.len());
        match (*name, generic_count) {
            (Var::LIST, 1) => {
                let first = &gens.as_ref().unwrap().params[0].id;
                let ty = self.parse_type(first)?;
                Ok(Type::List(Box::new(ty)))
            }
            (name, 0) => self.ctx.get_type(name),
            (_unknown, c) => {
                Err(Error::TypeParamCountMismatch { got: c, expected: 0 })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use lambc_parse::Span;

    use crate::type_check::TypeInference;

    use super::*;
    use pretty_assertions::assert_eq;

    const SPAN: Span = Span::new(0, 0);

    #[test]
    fn parses_simple_named_type() {
        let mut inf = TypeInference::new();
        inf.ctx.add_type(Var::INT, Type::INT);
        let mut parser = TypeParser::new(&mut inf.ctx);

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
        let mut inf = TypeInference::new();
        inf.ctx.add_type(Var::INT, Type::INT);
        let mut parser = TypeParser::new(&mut inf.ctx);

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
        let mut inf = TypeInference::new();
        inf.ctx.add_type(Var::INT, Type::INT);
        let mut parser = TypeParser::new(&mut inf.ctx);

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
