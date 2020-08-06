use common::ident::{IdentifierID, UnresolvedType, UnresolvedVariable};
use common::loc::Loc;

#[derive(Debug)]
pub struct Module {
    pub top_levels: Vec<TopLevel>,
}

#[derive(Debug)]
pub struct TopLevel {
    pub kind: TopLevelKind,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum TopLevelKind {
    VarDecl(VarDecl),
    FuncDecl(FuncDecl),
}

///////////////////////////////////////////////////////////////////////////////
//
// Statements
//
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Block(Vec<Stmt>),
    VarDecl(VarDecl),
    FuncDecl(FuncDecl),
    If { cond: Box<Expr>, conseq: Box<Expr> },
    While { cond: Box<Expr>, body: Box<Expr> },
    Return(Box<Expr>),
    Break,
    Continue,
}

#[derive(Debug)]
pub struct VarDecl {
    pub lvalue: Box<Pat>,
    pub rvalue: Box<Expr>,
    pub annot: Option<Box<Type>>,
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: IdentifierID,
    pub params: Vec<(IdentifierID, Box<Type>)>,
    pub body: Box<Stmt>,
    pub return_annot: Option<Box<Type>>,
}

///////////////////////////////////////////////////////////////////////////////
//
// Expressions
//
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum ExprKind {
    UnitLiteral,
    BoolLiteral(bool),
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    TupleLiteral(Vec<Expr>),
    Variable(UnresolvedVariable),
    UnaryOp {
        op: UnaryOp,
        node: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    ParenthesizedGroup(Box<Expr>),
    If {
        cond: Box<Expr>,
        conseq: Box<Expr>,
        altern: Box<Expr>,
    },
    Application {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    Assignment {
        var: UnresolvedVariable,
        expr: Box<Expr>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    LogicalNot,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
    Remainder,
    LogicalAnd,
    LogicalOr,
    Equals,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

///////////////////////////////////////////////////////////////////////////////
//
// Patterns
//
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct Pat {
    pub kind: PatKind,
    pub loc: Loc,
}

#[derive(Debug, Clone)]
pub enum PatKind {
    Variable(IdentifierID),
    Tuple(Vec<Pat>),
}

///////////////////////////////////////////////////////////////////////////////
//
// Types
//
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum TypeKind {
    Variable(UnresolvedType),
    Unit,
    Bool,
    Int,
    Float,
    String,
    Function(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
}
