pub trait LambBackend<'a>
where
    Self: Sized,
{
    fn compile_script(&mut self, script: &'a lamb_ast::Script);

    fn compile_block(&mut self, block: &'a lamb_ast::Block);

    fn compile_statement(&mut self, block: &'a lamb_ast::Statement);

    fn compile_assignment(&mut self, block: &'a lamb_ast::Assign);

    fn compile_expr(&mut self, block: &'a lamb_ast::Expr);

    fn compile_return(&mut self, ret: &'a Option<lamb_ast::Expr>);

    fn compile_case(&mut self, ret: &'a lamb_ast::Case);

    fn compile_if(&mut self, ret: &'a lamb_ast::If);

    fn compile_pattern(&mut self, ret: &'a lamb_ast::Pattern);
}
