use std::collections::HashMap;

use lamb_ast::{
    Assign, Atom, Binary, BinaryOp, Block, Case, Expr, FuncDef, Ident, If, Literal, Pattern,
    Script, Statement, UnaryOp,
};

use crate::backend::LambBackend;

// Lamb:
// {
//    x := 2;
//    x := 3;
//    {
//       x := 4;
//       assert(x = 4);
//    }
//    y := { x = 2; x + 3 };
//    assert(x = 3);
// }

// Js:
// ((function(){
//   const x0 := 2;
//   const x1 := 3;
//
//   (function(){
//     const x2 := 4;
//     assert(x2 === 4);
//     return;
//   })());
//
//   assert(x1 === 3);
//   return;
// })());

//  2 pages introduction
//  2 pages related works
// 11 pages of grundlagen
//  5 pages design information
// 10 pages about implementation
//  4 pages about fulfilling requirements
//  3 pages summary and conclusion

#[derive(Default)]
struct Environment {
    vars: HashMap<Ident, usize>,
    parent: Option<Box<Self>>,
}

impl Environment {
    pub fn add_ident(&mut self, id: Ident) {
        *self.vars.entry(id).or_insert(0) += 1;
    }

    pub fn resolve_ident<'b>(&self, id: &'b Ident) -> Ident {
        let idx = self.vars.get(&id).copied();
        match idx {
            Some(idx) => {
                let mut id = id.0.clone();
                id.push_str(idx.to_string().as_str());
                Ident(id)
            }
            None => match self.parent.as_ref() {
                Some(par) => par.resolve_ident(id),
                None => todo!("No ident defined with this name... {}", id.0.as_str()),
            },
        }
    }

    pub fn new_scope(&mut self) {
        let parent = std::mem::take(self);
        self.parent = Some(Box::new(parent));
    }

    pub fn end_scope(&mut self) {
        *self = *self
            .parent
            .take()
            .expect("Can't end a scope without a parent scope");
    }
}

pub struct Js {
    script: String,
    env: Environment,
}

impl Js {
    pub fn new() -> Self {
        Self {
            script: String::new(),
            env: Default::default(),
        }
    }

    pub fn into_output(self) -> String {
        self.script
    }

    pub fn push_ident(&mut self, id: &Ident) {
        let Ident(id) = self.env.resolve_ident(id);
        self.push_str(id.as_str());
    }

    fn push_str(&mut self, s: &str) {
        self.script.push_str(s);
    }

    fn compile_binary(&mut self, binary: &Binary) {
        let op = match binary.op {
            BinaryOp::Add => "__LambAdd",
            BinaryOp::Sub => "__LambSub",
            BinaryOp::Div => "__LambDiv",
            BinaryOp::Mul => "__LambMul",
            BinaryOp::Mod => "__LambMod",
            BinaryOp::Gt => "__LambGt",
            BinaryOp::Ge => "__LambGe",
            BinaryOp::Lt => "__LambLt",
            BinaryOp::Le => "__LambLe",
            BinaryOp::Eq => "__LambEq",
            BinaryOp::Ne => "__LambNe",
            BinaryOp::LogOr => "__LambLogOr",
            BinaryOp::LogAnd => "__LambLogAnd",
            BinaryOp::BinOr => "__LambBinOr",
            BinaryOp::BinAnd => "__LambBinAnd",
            BinaryOp::BinXor => "__LambBinXor",
            BinaryOp::RShift => "__LambRShift",
            BinaryOp::LShift => "__LambLShift",
            BinaryOp::LApply => "__LambLApply",
            BinaryOp::RApply => "__LambRApply",
            BinaryOp::LCompose => "__LambLCompose",
            BinaryOp::RCompose => "__LambRCompose",
        };

        self.push_str(op);
        self.push_str("(");
        self.compile_expr(binary.lhs.as_ref());
        self.push_str(",");
        self.compile_expr(binary.rhs.as_ref());
        self.push_str(")");
    }

    fn compile_unary(&mut self, unary: &lamb_ast::Unary) {
        let op = match unary.op {
            UnaryOp::NumNeg => "__LambNumNeg",
            UnaryOp::LogNot => "__LambLogNot",
            UnaryOp::BinNot => "__LambBinNot",
        };

        self.push_str(op);
        self.push_str("(");
        self.compile_expr(unary.rhs.as_ref());
        self.push_str(")");
    }

    fn compile_idx(&mut self, idx: &lamb_ast::Index) {
        self.push_str("__LambIndex(");
        self.compile_expr(idx.indexee.as_ref());
        self.push_str(",");
        self.compile_expr(idx.index.as_ref());
        self.push_str(")");
    }

    fn compile_funccall(&mut self, func: &lamb_ast::FuncCall) {
        self.push_str("__LambCall(");
        self.compile_expr(func.callee.as_ref());
        self.push_str(",");
        for arg in &func.args {
            self.compile_expr(arg);
            self.push_str(",");
        }
        self.push_str(")");
    }

    fn compile_funcdef(&mut self, fd: &FuncDef) {
        let FuncDef {
            args,
            body,
            // TODO: How to handle is_recursive?
            is_recursive,
        } = fd;
        self.push_str("(");
        for arg in args {
            self.push_str(arg.0.as_str());
            self.push_str(",");
        }
        self.push_str(") => (");
        self.compile_expr(body);
        self.push_str(")");
    }

    fn compile_atom(&mut self, atom: &lamb_ast::Atom) {
        match atom {
            lamb_ast::Atom::Literal(lit) => {
                let lit = match lit {
                    lamb_ast::Literal::Num(n) => n.to_string(),
                    lamb_ast::Literal::Str(s) => s.clone(),
                    lamb_ast::Literal::Char(c) => c.to_string(),
                    lamb_ast::Literal::Bool(b) => b.to_string(),
                    lamb_ast::Literal::Nil => "null".to_owned(),
                };

                self.push_str(lit.as_str());
            }
            lamb_ast::Atom::Ident(i) => self.push_ident(i),
            lamb_ast::Atom::Array(a) => {
                self.push_str("__LambMakeArray(");
                for i in a {
                    self.compile_expr(i);
                    self.push_str(", ");
                }
                self.push_str(")");
            }
        }
    }
}

impl<'a> LambBackend<'a> for Js {
    fn compile_script(&mut self, script: &'a Script) {
        let Script { block } = script;
        self.push_str("\"use strict\";");
        self.push_str("function __LambMain(){");
        self.compile_block(block);
        self.push_str(";");
        self.push_str("}");
        self.push_str("__LambMain();")
    }

    fn compile_block(&mut self, block: &'a Block) {
        self.env.new_scope();

        let Block { stats, value } = block;
        self.push_str("((function(){");

        for s in stats {
            self.compile_statement(s);
        }

        self.push_str("return ");
        if let Some(expr) = value.as_deref() {
            self.compile_expr(expr);
        }

        self.push_str(";})())");
        self.env.end_scope();
    }

    fn compile_statement(&mut self, stmt: &'a Statement) {
        match stmt {
            Statement::Assign(ass) => self.compile_assignment(ass),
            Statement::Expr(expr) => {
                self.compile_expr(expr);
                self.push_str(";");
            }
            Statement::Return(value) => {
                self.push_str("return ");
                if let Some(expr) = value.as_ref() {
                    self.compile_expr(expr);
                }
                self.push_str(";");
            }
        }
    }

    fn compile_assignment(&mut self, assign: &'a Assign) {
        let Assign { assignee, value } = assign;
        self.env.add_ident(assignee.clone());

        self.push_str("const ");
        self.push_ident(&assignee);
        self.push_str(" = ");
        self.compile_expr(value);
        self.push_str(";");
    }

    fn compile_expr(&mut self, expr: &'a Expr) {
        match expr {
            Expr::Binary(b) => self.compile_binary(b),
            Expr::Unary(u) => self.compile_unary(u),
            Expr::FuncCall(f) => self.compile_funccall(f),
            Expr::Index(i) => self.compile_idx(i),
            Expr::If(if_) => self.compile_if(if_),
            Expr::Case(case) => self.compile_case(case),
            Expr::FuncDef(fd) => self.compile_funcdef(fd),
            Expr::Block(b) => self.compile_block(b),
            Expr::Atom(atom) => self.compile_atom(atom),
            Expr::Error => unimplemented!("Attempt to compile Expr::Error"),
        }
    }

    fn compile_return(&mut self, ret: &'a Option<Expr>) {
        self.push_str("return ");
        if let Some(e) = ret {
            self.compile_expr(e);
        }
        self.push_str(";");
    }

    fn compile_case(&mut self, case: &'a Case) {
        todo!()
    }

    fn compile_if(&mut self, if_: &'a If) {
        // Important distinction between lamb "if" and JavaScript "if":
        // - Lamb "if" is an expression.
        //
        // For this reason, we compile it instead to ternary operator
        self.compile_expr(if_.cond.as_ref());
        self.push_str(" ? ");
        self.compile_block(if_.block.as_ref());
        self.push_str(" : (");
        for elif in &if_.elifs {
            self.compile_expr(&elif.cond);
            self.push_str(" ? ");
            self.compile_block(&elif.block);
            self.push_str(" : (");
        }

        match if_.els.as_deref() {
            Some(e) => self.compile_block(&e.block),
            None => self.compile_atom(&Atom::Literal(Literal::Nil)),
        }
        self.push_str(")".repeat(if_.elifs.len()).as_str());
        self.push_str(")");
    }

    fn compile_pattern(&mut self, pattern: &'a Pattern) {
        todo!()
    }
}
