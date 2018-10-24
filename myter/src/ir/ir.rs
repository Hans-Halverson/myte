use common::span::Span;

pub enum Ir {
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
    Add {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
    Subtract {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
    Multiply {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
    Divide {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
    Exponentiate {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
    Remainder {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
    ParenthesizedGroup {
        node: Box<Ir>,
        span: Span,
    },
    UnaryPlus {
        node: Box<Ir>,
        span: Span,
    },
    UnaryMinus {
        node: Box<Ir>,
        span: Span,
    },
    LogicalNot {
        node: Box<Ir>,
        span: Span,
    },
    LogicalAnd {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
    LogicalOr {
        left: Box<Ir>,
        right: Box<Ir>,
        span: Span,
    },
}
