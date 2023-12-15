use backend::LambBackend;
use lamb_ast::{Assign, Atom, Block, Expr, Ident, If, Literal, Script, Statement};

mod backend;
mod js;

fn main() {
    let mut js = js::Js::new();
    js.compile_script(&Script {
        block: Block {
            stats: vec![
                Statement::Assign(Assign {
                    assignee: Ident("run".to_string()),
                    value: Expr::Atom(Atom::Literal(Literal::Num(2))),
                }),
                Statement::Assign(Assign {
                    assignee: Ident("run".to_string()),
                    value: Expr::Atom(Atom::Literal(Literal::Num(2))),
                }),
                Statement::Expr(Expr::Atom(Atom::Ident(Ident("run".to_string())))),
            ],
            value: None,
        },
    });

    println!("{}", js.into_output())
}
