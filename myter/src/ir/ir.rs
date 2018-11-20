use common::ident::VariableID;
use common::span::Span;

#[derive(Clone)]
pub struct IrExpr {
    pub node: IrExprType,
    pub span: Span,
}

#[derive(Clone)]
pub enum IrExprType {
    UnitLiteral,
    BoolLiteral(bool),
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    Variable(VariableID),
    Add {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Subtract {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Multiply {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Divide {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Exponentiate {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Remainder {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    ParenthesizedGroup(Box<IrExpr>),
    UnaryPlus(Box<IrExpr>),
    UnaryMinus(Box<IrExpr>),
    LogicalNot(Box<IrExpr>),
    LogicalAnd {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    LogicalOr {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Equals {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    NotEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    LessThan {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    LessThanOrEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    GreaterThan {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    GreaterThanOrEqual {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Block(Vec<IrStmt>),
    If {
        cond: Box<IrExpr>,
        conseq: Box<IrExpr>,
        altern: Box<IrExpr>,
    },
    Application {
        func: Box<IrExpr>,
        args: Vec<IrExpr>,
    },
    Assignment {
        var: VariableID,
        expr: Box<IrExpr>,
    },
}

#[derive(Clone)]
pub struct IrStmt {
    pub node: IrStmtType,
    pub span: Span,
}

#[derive(Clone)]
pub enum IrStmtType {
    Expr(Box<IrExpr>),
    VariableDefinition {
        lvalue: Box<IrPat>,
        rvalue: Box<IrExpr>,
    },
    FunctionDefinition {
        name: VariableID,
        params: Vec<VariableID>,
        body: Box<IrExpr>,
    },
    If {
        cond: Box<IrExpr>,
        conseq: Box<IrExpr>,
    },
}

#[derive(Clone)]
pub enum IrPat {
    Variable { var: VariableID, span: Span },
}
