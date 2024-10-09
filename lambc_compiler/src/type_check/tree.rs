//! Abstract Syntax Tree for Lamb
//!
//! This module contains the nodes that make up the Abstract-Syntax-Tree for Lamb
//! directly after parsing. In order to reduce duplication of ASTs throughout the
//! project, many of the nodes are generic over the identifier type, as well as
//! the type of the path variable.
use super::TypedVar;
use crate::{name_res::Var, PathRef};
use lambc_parse::Span;

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
pub struct Module {
    /// The items exported by the module
    pub exports: Vec<Export>,
    /// The imports within the module
    pub imports: Vec<Import>,
    /// The top-level [`Item`] defined within the module
    pub items: Vec<Item>,
    /// The path of the module itself
    pub path: PathRef,
    /// The span of the entire module contents
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
pub struct Import {
    /// The file from which `items` are being imported
    pub file: PathRef,
    /// A name for the glob import
    pub name: Option<Var>,
    /// The list of [`ImportItem`] being imported
    pub items: Vec<ImportItem>,
    /// Whether or not the import is a glob import
    pub star: bool,
    /// The span of the entire import
    pub span: Span,
    /// The span of the path being imported from
    pub path_span: Span,
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
pub struct ImportItem {
    /// The name of the item being imported
    pub item: Var,
    /// An alias by which the item should be referred to
    pub alias: Option<Var>,
    /// The span of the entire item, including the alias if present
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
pub struct Export {
    /// A list of the [`ExportItem`] being exported
    pub items: Vec<ExportItem>,
    /// The span of the entire export
    pub span: Span,
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
pub struct ExportItem {
    /// The name of the [`Item`] being exported
    pub item: Var,
    /// The alias by which this must be referred to when being imported
    pub alias: Option<Var>,
    /// The span of the item name, and the alias if present
    pub span: Span,
}

/// A top-level piece of code that is determined at compile time.
/// For now this consists of top-level variable definitions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item {
    Def(Define),
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
pub struct Define {
    /// The name of the item or local variable being defined
    pub ident: TypedVar,
    /// The expected type of the `value`
    pub typ: Option<super::Type>,
    /// The expression whose value will be assigned to `ident`
    pub value: Expr,
    /// The span of the entire definition
    pub span: Span,
}

/// All possible expressions found within Lamb
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    /// A variable reference
    Ident(TypedVar),
    /// A char literal
    Usv(UsvLit),
    /// A string literal
    String(StrLit),
    /// A boolean literal
    Bool(BoolLit),
    /// A nil literal
    Nil(NilLit),
    /// A int literal
    Int(IntLit),
    /// A double literal
    Double(DoubleLit),
    /// A list literal
    List(Box<List>),
    /// A group expression
    Group(Box<Group>),
    /// A function definition
    FnDef(Box<FnDef>),
    /// A block
    Block(Box<Block>),
    /// An `if` expression
    If(Box<If>),
    /// A `case` expression
    Case(Box<Case>),
    /// An index expression
    Index(Box<Index>),
    /// A call expression
    Call(Box<Call>),
    /// A unary expression
    Unary(Box<Unary>),
    /// A binary expression
    Binary(Box<Binary>),
    /// A return expression
    Return(Box<Return>),
}

/// The base that the string contents of the [`I64Lit`].
///
/// This is used because after parsing, the prefix of the literal is stripped
/// away so that later phases of compilation can calculate the value by using
/// this base and [`i64::from_str_radix`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IntBase {
    /// Base 2
    Bin,
    /// Base 8
    Oct,
    /// Base 10
    Dec,
    /// Base 16
    Hex,
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
pub struct IntLit {
    /// The base, which was parsed from the suffix
    pub base: IntBase,
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
pub struct DoubleLit {
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
pub struct UsvLit {
    /// The text of the char, excluding quotes
    pub text: Option<UsvText>,
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
pub struct UsvText {
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
pub struct List {
    /// The values contained within the list
    pub values: Vec<Expr>,
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
pub struct Group {
    /// The inner expression
    pub value: Expr,
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
pub struct FnDef {
    /// The parameters of the function
    pub args: Vec<TypedVar>,
    /// The body of the function
    pub body: Expr,
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
pub struct Block {
    /// The statements within the block
    pub statements: Vec<Stmt>,
    /// The final expression within the block
    pub value: Option<Expr>,
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
pub struct IfCond {
    /// The condition to be tested
    pub cond: Expr,
    /// The block which will be conditionally executed
    pub body: Block,
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
pub struct Else {
    /// The block to be executed
    pub body: Block,
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
pub struct If {
    /// The initial if condition
    pub cond: IfCond,
    /// The optional elif branches
    pub elif: Vec<IfCond>,
    /// The optional else branch
    pub els_: Option<Else>,
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
pub struct Case {
    /// The value that the patterns in the arm will be tested against
    pub scrutinee: Expr,
    /// The arms of the case expression
    pub arms: Vec<CaseArm>,
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
pub struct CaseArm {
    /// The pattern which the scrutinee of the case will be tested against
    pub pattern: Pattern,
    /// The expr to be executed if the pattern matches
    pub body: Expr,
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
pub struct Pattern {
    /// The list of `|` separated patterns
    pub inner: Vec<InnerPattern>,
    /// The span of the pattern
    pub span: Span,
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
pub enum InnerPattern {
    Literal(Box<LiteralPattern>),
    Array(Box<ArrayPattern>),
    Ident(Box<IdentPattern>),
    Rest(Box<RestPattern>),
}

impl InnerPattern {
    pub fn is_rest_pattern(&self) -> bool {
        match self {
            InnerPattern::Ident(i) => {
                i.bound.as_ref().map_or(false, |b| b.is_rest_pattern())
            }
            InnerPattern::Rest(_) => true,
            InnerPattern::Literal(_) => false,
            InnerPattern::Array(_) => false,
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
    Usv(UsvLit),
    Int(IntLit),
    Nil(NilLit),
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
pub struct IdentPattern {
    /// The identifier to which the matching value will be bound
    pub ident: TypedVar,
    /// The pattern which the value will be tested against
    pub bound: Option<Box<InnerPattern>>,
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
pub struct ArrayPattern {
    /// The patterns on which the elements of the array will be tested against
    pub patterns: Vec<Pattern>,
    /// The span of the entire pattern
    pub span: Span,
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
pub struct Index {
    /// The expression which will be indexed into
    pub lhs: Expr,
    /// The expression to be used as the index
    pub rhs: Expr,
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
pub struct Call {
    /// The expression which will be called
    pub callee: Expr,
    /// The arguments to be passed to `callee`
    pub args: Vec<Expr>,
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
pub struct Binary {
    /// The expression on the left hand side of the operator
    pub lhs: Expr,
    /// The infix operator
    pub op: BinaryOp,
    /// The expression on the right hand side of the operator
    pub rhs: Expr,
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
pub struct Unary {
    /// The expression the operator is being applied to
    pub rhs: Expr,
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
pub struct Return {
    /// The optional expression to be returned
    pub value: Option<Expr>,
    /// The span of the expression, including the `return` keyword
    pub span: Span,
}

/// A definition, or an expression, which ends in a semicolon
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Stmt {
    Def(Define),
    Expr(ExprStmt),
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
pub struct ExprStmt {
    /// The expression whose value will be discarded after evaluation
    pub expr: Expr,
    /// The span of the expression and the following semicolon
    pub span: Span,
}
