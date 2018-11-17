use common::ident::VariableID;
use common::span::Span;

#[derive(Clone)]
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
    Equals {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    NotEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    LessThan {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    LessThanOrEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    GreaterThan {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    GreaterThanOrEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        span: Span,
    },
    Block {
        nodes: Vec<IrStmt>,
        span: Span,
    },
    If {
        cond: Box<IrExpr>,
        conseq: Box<IrExpr>,
        altern: Box<IrExpr>,
        span: Span,
    },
    Application {
        func: Box<IrExpr>,
        args: Vec<IrExpr>,
        span: Span,
    },
    Assignment {
        var: VariableID,
        expr: Box<IrExpr>,
        span: Span,
    },
}

#[derive(Clone)]
pub enum IrStmt {
    Expr {
        expr: Box<IrExpr>,
    },
    VariableDefinition {
        lvalue: Box<IrPat>,
        rvalue: Box<IrExpr>,
        span: Span,
    },
    FunctionDefinition {
        name: VariableID,
        params: Vec<VariableID>,
        body: Box<IrExpr>,
        span: Span,
    },
    If {
        cond: Box<IrExpr>,
        conseq: Box<IrExpr>,
        span: Span,
    },
}

#[derive(Clone)]
pub enum IrPat {
    Variable { var: VariableID, span: Span },
}

impl IrExpr {
    pub fn span(&self) -> &Span {
        match self {
            IrExpr::UnitLiteral { span } => span,
            IrExpr::BoolLiteral { span, .. } => span,
            IrExpr::StringLiteral { span, .. } => span,
            IrExpr::IntLiteral { span, .. } => span,
            IrExpr::FloatLiteral { span, .. } => span,
            IrExpr::Variable { span, .. } => span,
            IrExpr::UnaryPlus { span, .. } => span,
            IrExpr::UnaryMinus { span, .. } => span,
            IrExpr::LogicalNot { span, .. } => span,
            IrExpr::Add { span, .. } => span,
            IrExpr::Subtract { span, .. } => span,
            IrExpr::Multiply { span, .. } => span,
            IrExpr::Divide { span, .. } => span,
            IrExpr::Exponentiate { span, .. } => span,
            IrExpr::Remainder { span, .. } => span,
            IrExpr::LogicalAnd { span, .. } => span,
            IrExpr::LogicalOr { span, .. } => span,
            IrExpr::Equals { span, .. } => span,
            IrExpr::NotEqual { span, .. } => span,
            IrExpr::LessThan { span, .. } => span,
            IrExpr::LessThanOrEqual { span, .. } => span,
            IrExpr::GreaterThan { span, .. } => span,
            IrExpr::GreaterThanOrEqual { span, .. } => span,
            IrExpr::ParenthesizedGroup { span, .. } => span,
            IrExpr::Block { span, .. } => span,
            IrExpr::If { span, .. } => span,
            IrExpr::Application { span, .. } => span,
            IrExpr::Assignment { span, .. } => span,
        }
    }
}
