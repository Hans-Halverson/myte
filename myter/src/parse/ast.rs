use common::ident::{IdentifierID, UnresolvedType, UnresolvedVariable};
use common::loc::Loc;

#[derive(Debug)]
pub struct AstExpr {
    pub node: AstExprType,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum AstExprType {
    UnitLiteral,
    BoolLiteral(bool),
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    TupleLiteral(Vec<AstExpr>),
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
    Return(Box<AstExpr>),
    Break,
    Continue,
}

#[derive(Debug)]
pub enum AstStmt {
    Expr {
        expr: Box<AstExpr>,
    },
    VariableDefinition {
        lvalue: Box<AstPat>,
        rvalue: Box<AstExpr>,
        annot: Option<Box<AstType>>,
        loc: Loc,
    },
    FunctionDefinition {
        name: IdentifierID,
        params: Vec<(IdentifierID, Box<AstType>)>,
        body: Box<AstExpr>,
        return_annot: Option<Box<AstType>>,
        loc: Loc,
    },
    If {
        cond: Box<AstExpr>,
        conseq: Box<AstExpr>,
        loc: Loc,
    },
    While {
        cond: Box<AstExpr>,
        body: Box<AstExpr>,
        loc: Loc,
    },
}

#[derive(Debug, Clone)]
pub enum AstPat {
    Variable { var: IdentifierID, loc: Loc },
    Tuple { elements: Vec<AstPat>, loc: Loc },
}

#[derive(Debug)]
pub struct AstType {
    pub ty: AstTypeType,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum AstTypeType {
    Variable(UnresolvedType),
    Unit,
    Bool,
    Int,
    Float,
    String,
    Function(Vec<AstType>, Box<AstType>),
    Tuple(Vec<AstType>),
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    LogicalNot,
}

#[derive(Clone, Copy, Debug)]
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
