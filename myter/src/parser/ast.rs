use common::ident::{UnresolvedVariable, VariableID};
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
}

#[derive(Debug)]
pub enum AstStmt {
    Expr {
        expr: Box<AstExpr>,
    },
    VariableDefinition {
        lvalue: Box<AstPat>,
        rvalue: Box<AstExpr>,
        span: Span,
    },
    FunctionDefinition {
        name: VariableID,
        params: Vec<VariableID>,
        body: Box<AstExpr>,
        span: Span,
    },
    If {
        cond: Box<AstExpr>,
        conseq: Box<AstExpr>,
        span: Span,
    },
}

#[derive(Debug)]
pub enum AstPat {
    Variable { var: VariableID, span: Span },
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    LogicalNot,
}

#[derive(Debug)]
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
