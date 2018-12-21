use common::ident::{IdentifierID, UnresolvedType, UnresolvedVariable};
use common::span::Span;

#[derive(Debug)]
pub struct AstExpr {
    pub node: AstExprType,
    pub span: Span,
}

#[derive(Debug)]
pub enum AstExprType {
    UnitLiteral,
    BoolLiteral(bool),
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    Variable(UnresolvedVariable),
    UnaryOp {
        op: UnaryOp,
        node: Box<AstExpr>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<AstExpr>,
        right: Box<AstExpr>,
    },
    ParenthesizedGroup(Box<AstExpr>),
    Block(Vec<AstStmt>),
    If {
        cond: Box<AstExpr>,
        conseq: Box<AstExpr>,
        altern: Box<AstExpr>,
    },
    Application {
        func: Box<AstExpr>,
        args: Vec<AstExpr>,
    },
    Assignment {
        var: UnresolvedVariable,
        expr: Box<AstExpr>,
    },
    Return(Box<AstExpr>),
    Break,
    Continue,
}

#[derive(Debug)]
pub enum AstStmt {
    Expr {
        expr: Box<AstExpr>,
    },
    VariableDefinition {
        lvalue: Box<AstPat>,
        rvalue: Box<AstExpr>,
        annot: Option<Box<AstType>>,
        span: Span,
    },
    FunctionDefinition {
        name: IdentifierID,
        params: Vec<(IdentifierID, Box<AstType>)>,
        body: Box<AstExpr>,
        return_annot: Option<Box<AstType>>,
        span: Span,
    },
    If {
        cond: Box<AstExpr>,
        conseq: Box<AstExpr>,
        span: Span,
    },
    While {
        cond: Box<AstExpr>,
        body: Box<AstExpr>,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum AstPat {
    Variable { var: IdentifierID, span: Span },
}

#[derive(Debug)]
pub struct AstType {
    pub ty: AstTypeType,
    pub span: Span,
}

#[derive(Debug)]
pub enum AstTypeType {
    Variable(UnresolvedType),
    Unit,
    Bool,
    Int,
    Float,
    String,
    Function(Vec<AstType>, Box<AstType>),
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
