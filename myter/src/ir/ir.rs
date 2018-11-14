use common::ident::VariableID;
use common::span::Span;

pub enum IrExpr {
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
        var: VariableID,
        span: Span,
    },
    Add {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    Subtract {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    Multiply {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    Divide {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    Exponentiate {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    Remainder {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    ParenthesizedGroup {
        node: Box<IrExpr>,
        span: Span,
    },
    UnaryPlus {
        node: Box<IrExpr>,
        span: Span,
    },
    UnaryMinus {
        node: Box<IrExpr>,
        span: Span,
    },
    LogicalNot {
        node: Box<IrExpr>,
        span: Span,
    },
    LogicalAnd {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    LogicalOr {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    Block {
        nodes: Vec<IrStmt>,
        span: Span,
    },
}

pub enum IrStmt {
    Expr {
        expr: Box<IrExpr>,
    },
    VariableDefinition {
        lvalue: Box<IrPat>,
        rvalue: Box<IrExpr>,
        span: Span,
    },
}

pub enum IrPat {
    Variable { var: VariableID, span: Span },
}
