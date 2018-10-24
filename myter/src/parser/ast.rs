use common::span::Span;

pub enum Ast {
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
    LogicalNot {
        node: Box<Ast>,
        span: Span,
    },
    LogicalAnd {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
    LogicalOr {
        left: Box<Ast>,
        right: Box<Ast>,
        span: Span,
    },
}

impl Ast {
    pub fn span(&self) -> &Span {
        match self {
            Ast::UnitLiteral { span } => span,
            Ast::BoolLiteral { span, .. } => span,
            Ast::StringLiteral { span, .. } => span,
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
            Ast::LogicalNot { span, .. } => span,
            Ast::LogicalAnd { span, .. } => span,
            Ast::LogicalOr { span, .. } => span,
        }
    }
}
