use common::span::Span;

pub enum Ir {
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
}
