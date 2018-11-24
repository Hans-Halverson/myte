use common::ident::IdentifierID;
use common::span::Span;

pub type IrID = u32;

#[derive(Clone)]
pub struct IrContext {
    next_ir_id: IrID,
}

impl IrContext {
    pub fn new() -> IrContext {
        IrContext { next_ir_id: 0 }
    }

    pub fn new_ir_id(&mut self) -> IrID {
        let id = self.next_ir_id;
        self.next_ir_id += 1;
        id
    }
}

#[derive(Clone)]
pub struct IrExpr {
    pub id: IrID,
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
    Variable(IdentifierID),
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
        var: IdentifierID,
        expr: Box<IrExpr>,
    },
}

#[derive(Clone)]
pub struct IrStmt {
    pub id: IrID,
    pub node: IrStmtType,
    pub span: Span,
}

#[derive(Clone)]
pub enum IrStmtType {
    Expr(Box<IrExpr>),
    VariableDefinition {
        lvalue: Box<IrPat>,
        rvalue: Box<IrExpr>,
        has_annot: bool,
    },
    FunctionDefinition {
        name: IdentifierID,
        params: Vec<IdentifierID>,
        body: Box<IrExpr>,
    },
    If {
        cond: Box<IrExpr>,
        conseq: Box<IrExpr>,
    },
}

#[derive(Clone)]
pub enum IrPat {
    Variable {
        id: IrID,
        var: IdentifierID,
        span: Span,
    },
}
