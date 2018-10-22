use common::span::Span;

pub enum Ast {
    IntLiteral {
        num: i64,
        span: Span,
    },
    FloatLiteral {
        num: f64,
        span: Span,
    },
    Add {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
    Subtract {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
    Multiply {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
    Divide {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
    Exponentiate {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
    Remainder {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
    ParenthesizedGroup {
        node: Box<Ast>,
        span: Span,
    },
    UnaryPlus {
        node: Box<Ast>,
        span: Span,
    },
    UnaryMinus {
        node: Box<Ast>,
        span: Span,
    },
}

impl Ast {
    pub fn span(&self) -> &Span {
        match self {
            Ast::IntLiteral { span, .. } => span,
            Ast::FloatLiteral { span, .. } => span,
            Ast::Add { span, .. } => span,
            Ast::Subtract { span, .. } => span,
            Ast::Multiply { span, .. } => span,
            Ast::Divide { span, .. } => span,
            Ast::Exponentiate { span, .. } => span,
            Ast::Remainder { span, .. } => span,
            Ast::ParenthesizedGroup { span, .. } => span,
            Ast::UnaryPlus { span, .. } => span,
            Ast::UnaryMinus { span, .. } => span,
        }
    }
}
