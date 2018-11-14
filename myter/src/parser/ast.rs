use common::ident::{UnresolvedVariable, VariableID};
use common::span::Span;

#[derive(Debug)]
pub enum AstExpr {
    UnitLiteral {
        span: Span,
    },
    BoolLiteral {
        bool: bool,
        span: Span,
    },
    StringLiteral {
        string: String,
        span: Span,
    },
    IntLiteral {
        num: i64,
        span: Span,
    },
    FloatLiteral {
        num: f64,
        span: Span,
    },
    Variable {
        var: UnresolvedVariable,
        span: Span,
    },
    UnaryOp {
        op: UnaryOp,
        node: Box<AstExpr>,
        span: Span,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<AstExpr>,
        right: Box<AstExpr>,
        span: Span,
    },
    ParenthesizedGroup {
        node: Box<AstExpr>,
        span: Span,
    },
    Block {
        nodes: Vec<AstStmt>,
        span: Span,
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
}

impl AstExpr {
    pub fn span(&self) -> &Span {
        match self {
            AstExpr::UnitLiteral { span } => span,
            AstExpr::BoolLiteral { span, .. } => span,
            AstExpr::StringLiteral { span, .. } => span,
            AstExpr::IntLiteral { span, .. } => span,
            AstExpr::FloatLiteral { span, .. } => span,
            AstExpr::Variable { span, .. } => span,
            AstExpr::UnaryOp { span, .. } => span,
            AstExpr::BinaryOp { span, .. } => span,
            AstExpr::ParenthesizedGroup { span, .. } => span,
            AstExpr::Block { span, .. } => span,
        }
    }
}

impl AstStmt {
    pub fn span(&self) -> &Span {
        match self {
            AstStmt::Expr { expr } => expr.span(),
            AstStmt::VariableDefinition { span, .. } => span,
        }
    }
}
