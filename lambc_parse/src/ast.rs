//! Abstract Syntax Tree for Lamb
//!
//! This module contains the nodes that make up the Abstract-Syntax-Tree for Lamb
//! directly after parsing. In order to reduce duplication of ASTs throughout the
//! project, many of the nodes are generic over the identifier type, as well as
//! the type of the path variable.
use crate::{Span, TokKind};

/// Marks that an item contains a span. This is implemented on items which do
/// contain a span, but don't have a span member, such as the `enum` nodes.
pub trait Spanned {
    /// Returns the [`Span`] of the AST node.
    fn span(&self) -> Span;
}

impl Spanned for Ident {
    fn span(&self) -> Span {
        self.span
    }
}

/// The base that the string contents of the [`I64Lit`].
///
/// This is used because after parsing, the prefix of the literal is stripped
/// away so that later phases of compilation can calculate the value by using
/// this base and [`i64::from_str_radix`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum I64Base {
    /// Base 2
    Bin,
    /// Base 8
    Oct,
    /// Base 10
    Dec,
    /// Base 16
    Hex,
}

impl I64Base {
    /// Returns the number value for the base
    pub fn to_base(&self) -> u32 {
        match self {
            I64Base::Bin => 2,
            I64Base::Oct => 8,
            I64Base::Dec => 10,
            I64Base::Hex => 16,
        }
    }
}

/// A non-decimal numeric literal
///
/// # Breakdown
/// ```text
/// |--------| span
/// || base
/// 0xDEADBEEF
///   |------| values
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct I64Lit {
    /// The base, which was parsed from the suffix
    pub base: I64Base,
    /// The text from the input of the literal, without the prefix
    pub value: String,
    /// The span in the input this was parsed from
    pub span: Span,
}

/// Any decimal numeric literal
///
/// # Breakdown
/// ```text
/// |--------| span
/// 123.456789
/// |--------| value
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct F64Lit {
    /// The text from the input representing the literal
    pub value: String,
    /// The span in the input this was parsed from
    pub span: Span,
}

/// A boolean literal (`true`|`false`)
///
/// # Breakdown
/// ```text
/// |--| span
/// true
/// |--| value
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BoolLit {
    /// The value of the literal
    pub value: bool,
    /// The span in the input this was parsed from
    pub span: Span,
}

/// The `nil` literal
///
/// # Breakdown
/// ```text
/// |-| span
/// nil
/// |-| value
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NilLit {
    /// The span in the input this was parsed from
    pub span: Span,
}

/// A string literal
///
/// # Breakdown
/// ```text
///
/// |-----------| span
/// "hello world"
///  |---------| text
///
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StrLit {
    /// The text of the string, excluding quotes
    pub text: Option<StrText>,
    /// The span of the entire string, including quotes
    pub span: Span,
}

/// The text within a string literal
///
/// # Breakdown
/// ```text
///
///  |---------| span
/// "hello world"
///  |---------| text
///
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StrText {
    /// The text of the string, excluding quotes
    pub inner: String,
    /// The span of the entire string, excluding quotes
    pub span: Span,
}

/// A char literal
///
/// # Breakdown
///
/// ```text
///
/// |-----------| span
/// 'broken char'
///  |---------| text
///
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CharLit {
    /// The text of the char, excluding quotes
    pub text: Option<CharText>,
    /// The span of the entire char, including quotes
    pub span: Span,
}

/// The text within a char literal
///
/// # Breakdown
/// ```text
///
///  |---------| span
/// 'broken char'
///  |---------| text
///
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CharText {
    /// The text of the char, excluding quotes
    pub inner: String,
    /// The span of the entire char, excluding quotes
    pub span: Span,
}

/// An identifier
///
/// # Breakdown
/// ```text
///
/// |----------| span
/// a_long_ident
/// |----------| values
///
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident {
    /// The identifier as a string
    pub raw: String,
    /// The span of the identifier
    pub span: Span,
}

/// A list of [`Expr`]
///
/// # Breakdown
/// ```text
///
/// |----------| span
/// [1, 2, 3, 4]
///  |--------| values
///
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct List<IdKind> {
    /// The values contained within the list
    pub values: Vec<Expr<IdKind>>,
    /// The span of the entire list, including `[]`
    pub span: Span,
}

/// An [`Expr`] surrounded by parenthesis
///
/// # Breakdown
/// ```text
///
/// |------------| span
/// ([1, 2, 3, 4])
///  |----------| value
///
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Group<IdKind> {
    /// The inner expression
    pub value: Expr<IdKind>,
    /// The span of the entire expression, including `()`
    pub span: Span,
}

/// A function definition
///
/// # Breakdown
///
/// ```text
/// |------ span ------|
/// rec fn(a, b, c) -> a
/// |-|    |-----|     ^ body
/// |      ^ args
/// ^ recursive
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnDef<IdKind> {
    /// The parameters of the function
    pub args: Vec<IdKind>,
    /// The body of the function
    pub body: Expr<IdKind>,
    /// Whether the function is recursive
    pub recursive: bool,
    /// The span of the function, including the `fn` keyword
    pub span: Span,
}

/// A block expression
///
/// # Breakdown
///
///
/// ```text
/// |-------------------| span
/// { let x := 2; x * 2 }
///   |---------| |---|
///   |           ^ value
///   ^ statements    
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Block<IdKind> {
    /// The statements within the block
    pub statements: Vec<Statement<IdKind>>,
    /// The final expression within the block
    pub value: Option<Expr<IdKind>>,
    /// The span of the entire block, including `{}`
    pub span: Span,
}

/// A combination of an condition and a block which should be executed if the
/// condition is true.
///
/// # Breakdown
///
/// ```text
/// |----------| span
/// if 2 = 3 { }
///    |---| |-|
///    |     ^ block
///    ^ condition
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IfCond<IdKind> {
    /// The condition to be tested
    pub cond: Expr<IdKind>,
    /// The block which will be conditionally executed
    pub body: Block<IdKind>,
    /// The entire span of the condition, including the `if` keyword, and the
    /// `{}`
    pub span: Span,
}

/// The `else` block in an `if` expression.
///
/// # Breakdown
///
/// ```text
///                |--------| span
/// if false { 1 } else { 2 }
///                     |---| body
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Else<IdKind> {
    /// The block to be executed
    pub body: Block<IdKind>,
    /// The span of the entire else, including the `else` keyword
    pub span: Span,
}

/// An `if` expression
///
/// # Breakdown
///
/// ```text
/// |----------------------------------------| span
/// if false { 1 } elif false { 2 } else { 3 }
///    |---|       |--------------| |--------|
///    ^ cond      ^ elif           ^ els_
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct If<IdKind> {
    /// The initial if condition
    pub cond: IfCond<IdKind>,
    /// The optional elif branches
    pub elif: Vec<IfCond<IdKind>>,
    /// The optional else branch
    pub els_: Option<Else<IdKind>>,
    /// The span of the entire if, including the `if` keyword
    pub span: Span,
}

/// An `case` expression
///
/// # Breakdown
///
/// ```text
/// |--------------------------| span
/// case num { 1 -> 2, 2 -> 3, }
///     |--|   |-------------|
///     |      ^ arms
///     ^ scrutinee
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Case<IdKind> {
    /// The value that the patterns in the arm will be tested against
    pub scrutinee: Expr<IdKind>,
    /// The arms of the case expression
    pub arms: Vec<CaseArm<IdKind>>,
    /// The span of the entire case, including the `case` keyword
    pub span: Span,
}

/// An arm in a case expression, which details the pattern to match
/// and the expression to be evaluated if the pattern matches.
///
/// # Breakdown
///
/// ```text
///            |------------| span
/// case num { 1 -> { 1 + 1 } }
///            |    |-------|
///            |    ^ body
///            ^ pattern
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CaseArm<IdKind> {
    /// The pattern which the scrutinee of the case will be tested against
    pub pattern: Pattern<IdKind>,
    /// The expr to be executed if the pattern matches
    pub body: Expr<IdKind>,
    /// The span of the entire case arm
    pub span: Span,
}

/// One or more patterns which can be used for pattern matching
///
/// # Breakdown
///
/// ```text
///            |-------| span
/// case num { 1 | 2 | 3 -> { 1 + 1 } }
///            |-------| inner
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Pattern<IdKind> {
    /// The list of `|` separated patterns
    pub inner: Vec<InnerPattern<IdKind>>,
    /// The span of the pattern
    pub span: Span,
}
impl<IdKind> Pattern<IdKind> {
    /// Returns the list of all binding names which are introduced by this pattern
    // TODO: Get rid of this when introducing hir
    pub fn binding_names(&self) -> Vec<&IdKind> {
        self.inner.iter().flat_map(InnerPattern::binding_names).collect()
    }
}

/// A singular pattern which can be used for pattern matching, and matches if any
/// of the subpatterns match.
///
/// # Breakdown
///
/// ```text
/// case num { 1 | 2 | 3 -> { 1 + 1 } }
///            ^ InnerPattern::Literal
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InnerPattern<IdKind> {
    Literal(Box<LiteralPattern>),
    Array(Box<ArrayPattern<IdKind>>),
    Ident(Box<IdentPattern<IdKind>>),
    Rest(Box<RestPattern>),
}

impl<IdKind> InnerPattern<IdKind> {
    /// Returns the list of all binding names which are introduced by this pattern
    // TODO: Get rid of this when introducing hir
    pub fn binding_names(&self) -> Vec<&IdKind> {
        match self {
            InnerPattern::Literal(_) => vec![],
            InnerPattern::Rest(_) => vec![],
            InnerPattern::Array(arr) => {
                arr.patterns.iter().flat_map(Pattern::binding_names).collect()
            }
            InnerPattern::Ident(id) => {
                vec![&id.ident]
            }
        }
    }

    /// Returns the span of the pattern
    pub fn span(&self) -> Span {
        match self {
            InnerPattern::Literal(p) => p.span(),
            InnerPattern::Array(p) => p.span,
            InnerPattern::Ident(p) => p.span,
            InnerPattern::Rest(p) => p.span,
        }
    }
}

/// The rest pattern, which always matches and can be used to bind to the unlisted
/// elements.
///
/// # Breakdown
///
/// ```text
/// case arr { [1, 2, ..] -> { 1 + 1 } }
///                   || span
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RestPattern {
    /// The span of the pattern
    pub span: Span,
}

/// Literal patterns, which are matched if the scrutinee has the same value
/// as the literal.
///
/// # Breakdown
///
/// ```text
/// case arr { [1, -2, ..] -> { 1 + 1 } }
///             |  || LiteralPattern::I64Lit
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LiteralPattern {
    String(StrLit),
    Bool(BoolLit),
    Char(CharLit),
    I64(I64Lit),
    Nil(NilLit),
}

impl LiteralPattern {
    /// Returns the span of the pattern
    pub fn span(&self) -> Span {
        match self {
            Self::String(p) => p.span,
            Self::Bool(p) => p.span,
            Self::Char(p) => p.span,
            Self::I64(p) => p.span,
            Self::Nil(p) => p.span,
        }
    }
}

/// A pattern which binds a matching value to a name
///
/// # Breakdown
///
/// ```text
///             |------| span
/// case arr { [all @ ..] -> { 1 + 1 } }
///             |-|   ||
///             |     ^ bound
///             ^ ident
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdentPattern<IdKind> {
    /// The identifier to which the matching value will be bound
    pub ident: IdKind,
    /// The pattern which the value will be tested against
    pub bound: Option<Box<InnerPattern<IdKind>>>,
    /// The span of the entire pattern
    pub span: Span,
}

/// A pattern which matches to list-like values such as strings or lists
///
/// # Breakdown
///
/// ```text
///            |---------------| span
/// case arr { [head, rest @ ..] -> { 1 + 1 } }
///             |-------------| patterns
///             |--|  |-------|
///             |     ^ pattern for the rest of the elements
///             ^ pattern for element at index 0
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayPattern<IdKind> {
    /// The patterns on which the elements of the array will be tested against
    pub patterns: Vec<Pattern<IdKind>>,
    /// The span of the entire pattern
    pub span: Span,
}
impl<IdKind> ArrayPattern<IdKind> {
    /// Returns a tuple representing all patterns before a rest pattern, after
    /// a rest pattern, and the rest pattern itself. If there is no rest pattern
    /// the all of the elements are the `head`
    ///
    /// # Breakdown
    ///
    /// ```text
    ///  | head                  | tail
    ///  |----------|            |--|
    /// [head1, head2, mid @ .., last]
    ///                |......|
    ///                | rest
    /// ```
    pub fn as_parts(
        &self,
    ) -> (&[Pattern<IdKind>], &[Pattern<IdKind>], Option<&InnerPattern<IdKind>>)
    {
        let mut splits =
            self.patterns.split(|pat| match pat.inner.as_slice() {
                [InnerPattern::Rest(..)] => true,
                [InnerPattern::Ident(ip)] => {
                    matches!(ip.bound.as_deref(), Some(InnerPattern::Rest(..)))
                }
                _ => false,
            });

        let head = splits.next().unwrap_or_default();
        let (tail, rest) = if let Some(tail) = splits.next() {
            (tail, self.patterns[head.len()].inner.first())
        } else {
            ([].as_slice(), None)
        };

        (head, tail, rest)
    }

    /// Returns the `Span` of all of the patterns before the [`RestPattern`]
    /// If there are are no patterns before the [`RestPattern`], `None` is
    /// returned
    pub fn head_span(&self) -> Option<Span> {
        let head = self.as_parts().0;
        head.first()
            .zip(head.last())
            .map(|(first, last)| Span::connect(first.span, last.span))
    }

    /// Returns the `Span` of all of the patterns before the [`RestPattern`]
    /// If there are are no patterns before the [`RestPattern`], `None` is
    /// returned
    pub fn tail_span(&self) -> Option<Span> {
        let tail = self.as_parts().1;
        tail.first()
            .zip(tail.last())
            .map(|(first, last)| Span::connect(first.span, last.span))
    }
}

/// An index expression
///
/// # Breakdown
///
/// ```text
/// |-------------| span
/// my_array[index]
/// |------| |---|
/// |        ^ rhs
/// ^ lhs
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Index<IdKind> {
    /// The expression which will be indexed into
    pub lhs: Expr<IdKind>,
    /// The expression to be used as the index
    pub rhs: Expr<IdKind>,
    /// The span of the entire index expression, including `[]`
    pub span: Span,
}

/// A call expression
///
/// # Breakdown
///
/// ```text
/// |-----------------| span
/// my_func(arg0, arg1)
/// |-----| |--------|
/// |       ^ args
/// ^ callee
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call<IdKind> {
    /// The expression which will be called
    pub callee: Expr<IdKind>,
    /// The arguments to be passed to `callee`
    pub args: Vec<Expr<IdKind>>,
    /// The span of the expression, including the `()`
    pub span: Span,
}

/// A binary (or infix) expression
///
/// # Breakdown
///
/// ```text
/// |---------------| span
///         | op_span
/// 123_456 * 789_098
/// |-----| | |-----|
/// |       | ^ rhs
/// |       ^ op
/// ^ lhs
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Binary<IdKind> {
    /// The expression on the left hand side of the operator
    pub lhs: Expr<IdKind>,
    /// The infix operator
    pub op: BinaryOp,
    /// The expression on the right hand side of the operator
    pub rhs: Expr<IdKind>,
    /// The span of the entire expression
    pub span: Span,
    /// The span of the operator
    // Must be a subspan of `span`
    pub op_span: Span,
}

/// All infix operators
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOp {
    /// `<$`
    Appl,
    /// `$>`
    Appr,
    /// `<.`
    Cpsl,
    /// `.>`
    Cpsr,
    /// `&&`
    Land,
    /// `||`
    Lor,
    /// `=`
    Eq,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `<=`
    Le,
    /// `<`
    Lt,
    /// `|`
    Bor,
    /// `^`
    Bxor,
    /// `&`
    Band,
    /// `>>`
    Shr,
    /// `<<`
    Shl,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `*`
    Mul,
}

impl BinaryOp {
    /// Returns both the left and right binding power of each
    /// operator for Pratt-Parsing
    pub fn infix_bp(self) -> (u8, u8) {
        match self {
            BinaryOp::Appl => (1, 2),
            BinaryOp::Appr => (1, 2),
            BinaryOp::Cpsl => (2, 3),
            BinaryOp::Cpsr => (2, 3),
            BinaryOp::Land => (3, 4),
            BinaryOp::Lor => (4, 5),
            BinaryOp::Eq => (5, 6),
            BinaryOp::Ne => (5, 6),
            BinaryOp::Ge => (5, 6),
            BinaryOp::Gt => (5, 6),
            BinaryOp::Le => (5, 6),
            BinaryOp::Lt => (5, 6),
            BinaryOp::Bor => (6, 7),
            BinaryOp::Bxor => (7, 8),
            BinaryOp::Band => (8, 9),
            BinaryOp::Shr => (9, 10),
            BinaryOp::Shl => (9, 10),
            BinaryOp::Add => (10, 11),
            BinaryOp::Sub => (10, 11),
            BinaryOp::Div => (11, 12),
            BinaryOp::Mod => (11, 12),
            BinaryOp::Mul => (11, 12),
        }
    }
}

/// A unary expression
///
/// # Breakdown
///
/// ```text
/// |------| span
/// | op_span
/// -789_098
/// ||-----|
/// |^ rhs
/// ^ op
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Unary<IdKind> {
    /// The expression the operator is being applied to
    pub rhs: Expr<IdKind>,
    /// The operator to be applied
    pub op: UnaryOp,
    /// The span of the entire expression
    pub span: Span,
    /// The span of the operator
    // Must be a subspan of `span`
    pub op_span: Span,
}

/// All unary operators
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    /// -
    Nneg,
    /// !
    Lnot,
    /// ~
    Bneg,
}

impl UnaryOp {
    /// Returns the right binding power
    pub(crate) fn rbp(&self) -> u8 {
        match self {
            UnaryOp::Nneg | UnaryOp::Lnot | UnaryOp::Bneg => 11,
        }
    }
}

impl From<TokKind> for UnaryOp {
    fn from(value: TokKind) -> Self {
        match value {
            TokKind::Sub => UnaryOp::Nneg,
            TokKind::Bneg => UnaryOp::Bneg,
            TokKind::Lnot => UnaryOp::Lnot,
            _ => unimplemented!("Bad compiler."),
        }
    }
}

/// A return expression.
///
/// Unlike in many language, `return` in Lamb is an
/// expression and not a statement to allow for it
/// being used in expression contexts.
///
/// # Breakdown
///
/// ```text
/// |----------| span
/// return 1 + 2
///        |---| value
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Return<IdKind> {
    /// The optional expression to be returned
    pub value: Option<Expr<IdKind>>,
    /// The span of the expression, including the `return` keyword
    pub span: Span,
}

/// A path expression
///
/// This type of expression is created by using `::` for accessing the contents
/// of a module.
///
/// # Breakdown
///
/// ```text
/// |---------------------------| span
/// module_name::sub_module::item
/// |---------|  |--------------|
/// ^ head       ^ tail
/// ```
// TODO:
// Since this is supposed to be something that is done at compiled time, there
// is probably a better way to represent this. Why is `head` separated from the
// rest of the items?
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Path {
    /// The initial variable to access
    pub head: Ident,
    /// The rest of the path elements
    pub tail: Vec<Ident>,
    /// The span of the entire path expression
    pub span: Span,
}

/// All possible expressions found within Lamb
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr<IdKind> {
    /// A variable reference
    Ident(IdKind),
    /// A char literal
    Char(CharLit),
    /// A string literal
    String(StrLit),
    /// A boolean literal
    Bool(BoolLit),
    /// A nil literal
    Nil(NilLit),
    /// A int literal
    I64(I64Lit),
    /// A double literal
    F64(F64Lit),
    /// A list literal
    List(List<IdKind>),
    /// A group expression
    Group(Box<Group<IdKind>>),
    /// A function definition
    FnDef(Box<FnDef<IdKind>>),
    /// A block
    Block(Box<Block<IdKind>>),
    /// An `if` expression
    If(Box<If<IdKind>>),
    /// A `case` expression
    Case(Box<Case<IdKind>>),
    /// An index expression
    Index(Box<Index<IdKind>>),
    /// A call expression
    Call(Box<Call<IdKind>>),
    /// A unary expression
    Unary(Box<Unary<IdKind>>),
    /// A binary expression
    Binary(Box<Binary<IdKind>>),
    /// A return expression
    Return(Box<Return<IdKind>>),
    /// A path expression
    Path(Box<Path>),
}

impl<IdKind> Expr<IdKind> {
    /// Returns whether an expression is directly recursive, meaning that
    /// it is a function definition that is marked as recursive.
    pub fn is_recursive(&self) -> bool {
        matches!(self, Self::FnDef(f) if f.recursive)
    }
}

impl<IdKind: Spanned> Expr<IdKind> {
    /// Returns the span of an expression
    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(e) => e.span(),
            Expr::Char(e) => e.span,
            Expr::String(e) => e.span,
            Expr::Bool(e) => e.span,
            Expr::Nil(e) => e.span,
            Expr::I64(e) => e.span,
            Expr::F64(e) => e.span,
            Expr::List(e) => e.span,
            Expr::Group(e) => e.span,
            Expr::FnDef(e) => e.span,
            Expr::Block(e) => e.span,
            Expr::If(e) => e.span,
            Expr::Case(e) => e.span,
            Expr::Index(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::Unary(e) => e.span,
            Expr::Binary(e) => e.span,
            Expr::Return(e) => e.span,
            Expr::Path(e) => e.span,
        }
    }

    /// Returns whether an expression ends with a block, such as an `if` or
    /// or `case` expression.
    pub fn ends_with_block(&self) -> bool {
        match self {
            Expr::Ident(_)
            | Expr::Char(_)
            | Expr::String(_)
            | Expr::Bool(_)
            | Expr::Nil(_)
            | Expr::I64(_)
            | Expr::F64(_)
            | Expr::List(_)
            | Expr::Index(_)
            | Expr::Call(_)
            | Expr::Path(_)
            | Expr::Group(_) => false,
            Expr::Return(r) => {
                r.value.as_ref().map_or(false, |e| e.ends_with_block())
            }
            Expr::Unary(u) => u.rhs.ends_with_block(),
            Expr::Binary(b) => b.rhs.ends_with_block(),
            Expr::FnDef(f) => f.body.ends_with_block(),
            Expr::Block(_) | Expr::If(_) | Expr::Case(_) => true,
        }
    }
}

/// A variable definition
///
/// This node is used both for item definitions, as well as local variable
/// definitions. The distinction is soley based on the parent node.
///
/// # Breakdown
///
/// ```text
///
/// |------------------------| span
/// my_var : string = "hello";
/// |----|   |----|   |-----|
/// |        |        ^ value
/// |        ^ typ
/// ^ ident
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Define<IdKind> {
    /// The name of the item or local variable being defined
    pub ident: IdKind,
    /// The expected type of the `value`
    pub typ: Option<Type<IdKind>>,
    /// The expression whose value will be assigned to `ident`
    pub value: Expr<IdKind>,
    /// The span of the entire definition
    pub span: Span,
}

/// A type found in the source code
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<IdKind> {
    /// Represents all function types
    Fn(Box<FnType<IdKind>>),
    /// Represents all other types. This is here
    /// because functions have named types,
    Named(Box<NamedType<IdKind>>),
}

impl<IdKind> Type<IdKind> {
    /// Returns the span of the type
    pub fn span(&self) -> Span {
        match self {
            Type::Fn(fun) => fun.span,
            Type::Named(nt) => nt.span,
        }
    }
}

/// A function type
///
/// # Breakdown
///
/// ```text
///
/// |--------------------------------------| span
/// fn[T, U](list[T], fn(T) -> U) -> list[U]
///   |----| |-----------------|     |-----|
///   |      |                       ^ ret_type
///   |      ^ args
///   ^ gens
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnType<IdKind> {
    /// The types of the arguments
    pub args: Vec<Type<IdKind>>,
    /// The generics of the function type
    pub gens: Option<SimpleGenerics<IdKind>>,
    /// The return type of the function
    pub ret_type: Option<Type<IdKind>>,
    /// The span of the function, including the `fn` keyword
    pub span: Span,
}

/// A non-function type
///
/// # Breakdown
///
/// ```text
///
/// |----------------------| span
/// dict[list[string], int]
/// |--| |---------------|
/// |    ^ gens
/// ^ name
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NamedType<IdKind> {
    /// The name of the type constructor
    pub name: IdKind,
    /// The name of the generic arguments
    pub gens: Option<Generics<IdKind>>,
    /// The span of the type
    pub span: Span,
}

/// A group of generics which bring new types into scope.
///
/// This is currently only found on [`FnType`], and is used to make a function
/// generic.
///
/// # Breakdown
///
/// ```text
///   |----| span
/// fn[A, B](A) -> A
///    |--| params
///
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SimpleGenerics<IdKind> {
    /// The name of the types being created and brought into scope
    pub params: Vec<SimpleGeneric<IdKind>>,
    /// The span of the list of [`SimpleGeneric`], including the `[]`
    pub span: Span,
}

/// A type being used within a [`SimpleGenerics`].
///
/// # Breakdown
///
/// ```text
///    | span
/// fn[A, B](A) -> A
///    | id
///
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SimpleGeneric<IdKind> {
    /// The name of the generic being brought into scope
    pub id: IdKind,
    /// The span of `id`
    pub span: Span,
}

/// A group of generics which may *not* bring new types into scope.
///
/// Curently only [`NamedType`] are capable of having generics which are not
/// brought into scope. This is most commonly seen on the `list[t]` type.
///
/// # Breakdown
///
/// ```text
/// |---------------| span
/// list[fn() -> int]
///      |---------| params
///
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Generics<IdKind> {
    /// The name of the types being created and brought into scope
    pub params: Vec<Generic<IdKind>>,
    /// The span of the list of [`Generic`], including `[]`
    pub span: Span,
}

/// A generic type used within a [`Generics`]
///
/// Curently only [`NamedType`] are capable of having generics which are not
/// brought into scope. This is most commonly seen on the `list[t]` type.
///
/// # Breakdown
///
/// ```text
///      |---------| span
/// list[fn() -> int]
///      |---------| id
///
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Generic<IdKind> {
    /// The type over which a type constructor is generic
    pub id: Type<IdKind>,
    /// The span of `id`
    pub span: Span,
}

/// An expression followed by a semicolon
///
/// # Breakdown
///
/// ```text
/// |---------------------| span
/// println("Hello world");
/// |--------------------| expr
///
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExprStatement<IdKind> {
    /// The expression whose value will be discarded after evaluation
    pub expr: Expr<IdKind>,
    /// The span of the expression and the following semicolon
    pub span: Span,
}

/// A definition, or an expression, which ends in a semicolon
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement<IdKind> {
    Define(Define<IdKind>),
    Expr(ExprStatement<IdKind>),
}

/// A top-level piece of code that is determined at compile time.
/// For now this consists of top-level variable definitions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item<IdKind> {
    Def(Define<IdKind>),
}

/// An [`Item`] that is being imported via name.
///
/// # Breakdown
///
/// ```text
///                           |-------------| span
/// from "./math.lb" import { add as addition, };
///                           |-|    |------|
///                           |      ^ alias
///                           ^ item
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportItem<IdKind> {
    /// The name of the item being imported
    pub item: IdKind,
    /// An alias by which the item should be referred to
    pub alias: Option<IdKind>,
    /// The span of the entire item, including the alias if present
    pub span: Span,
}

/// A group of [`Item`] that is being imported from a file
///
/// # Breakdown
///
/// ```text
/// |-----------------------------------------------------| span
///      |---------| path_span
/// from "./math.lb" import * as math { add as addition, };
///      |---------|        |    |--|   |-------------|
///      |                  |    |      ^ items
///      |                  |    ^ name
///      ^ file             ^ star
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Import<IdKind, PathKind> {
    /// The file from which `items` are being imported
    pub file: PathKind,
    /// A name for the glob import
    pub name: Option<IdKind>,
    /// The list of [`ImportItem`] being imported
    pub items: Vec<ImportItem<IdKind>>,
    /// Whether or not the import is a glob import
    pub star: bool,
    /// The span of the entire import
    pub span: Span,
    /// The span of the path being imported from
    pub path_span: Span,
}

/// An [`Item`] that is being exported via name.
///
/// # Breakdown
///
/// ```text
///          |-------------| span
/// export { add as addition, };
///          |-|    |------|
///          |      ^ alias
///          ^ item
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExportItem<IdKind> {
    /// The name of the [`Item`] being exported
    pub item: IdKind,
    /// The alias by which this must be referred to when being imported
    pub alias: Option<IdKind>,
    /// The span of the item name, and the alias if present
    pub span: Span,
}

/// A group of [`Item`] that is being exported
///
/// # Breakdown
///
/// ```text
/// |---------------------------------------------| span
/// export { add as addition, sub as subtraction };
///          |---------------------------------| items
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Export<IdKind> {
    /// A list of the [`ExportItem`] being exported
    pub items: Vec<ExportItem<IdKind>>,
    /// The span of the entire export
    pub span: Span,
}

/// The entirety of a Lamb module
///
/// # Breakdown
///
/// ```text
/// export { numId };
///
/// from "math.lb" import { add, sub };
///
/// def numId
///   : fn(int) -> int
///   = fn(a) -> add(sub(a, 1), 1);
///
/// ```
///
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Module<IdKind, PathKind> {
    /// The items exported by the module
    pub exports: Vec<Export<IdKind>>,
    /// The imports within the module
    pub imports: Vec<Import<IdKind, PathKind>>,
    /// The top-level [`Item`] defined within the module
    pub items: Vec<Item<IdKind>>,
    /// The path of the module itself
    pub path: PathKind,
    /// The span of the entire module contents
    pub span: Span,
}
