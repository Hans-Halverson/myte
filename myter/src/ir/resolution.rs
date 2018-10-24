use common::error::MyteResult;
use ir::ir::Ir;
use parser::ast::Ast;

pub fn resolve_ast(ast: Ast) -> MyteResult<Ir> {
    match ast {
        Ast::UnitLiteral { span } => Ok(Ir::UnitLiteral { span }),
        Ast::BoolLiteral { bool, span } => Ok(Ir::BoolLiteral { bool, span }),
        Ast::StringLiteral { string, span } => Ok(Ir::StringLiteral { string, span }),
        Ast::IntLiteral { num, span } => Ok(Ir::IntLiteral { num, span }),
        Ast::FloatLiteral { num, span } => Ok(Ir::FloatLiteral { num, span }),
        Ast::Add { left, right, span } => Ok(Ir::Add {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
        Ast::Subtract { left, right, span } => Ok(Ir::Subtract {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
        Ast::Multiply { left, right, span } => Ok(Ir::Multiply {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
        Ast::Divide { left, right, span } => Ok(Ir::Divide {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
        Ast::Exponentiate { left, right, span } => Ok(Ir::Exponentiate {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
        Ast::Remainder { left, right, span } => Ok(Ir::Remainder {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
        Ast::ParenthesizedGroup { node, span } => Ok(Ir::ParenthesizedGroup {
            node: Box::new(resolve_ast(*node)?),
            span,
        }),
        Ast::UnaryPlus { node, span } => Ok(Ir::UnaryPlus {
            node: Box::new(resolve_ast(*node)?),
            span,
        }),
        Ast::UnaryMinus { node, span } => Ok(Ir::UnaryMinus {
            node: Box::new(resolve_ast(*node)?),
            span,
        }),
        Ast::LogicalNot { node, span } => Ok(Ir::LogicalNot {
            node: Box::new(resolve_ast(*node)?),
            span,
        }),
        Ast::LogicalAnd { left, right, span } => Ok(Ir::LogicalAnd {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
        Ast::LogicalOr { left, right, span } => Ok(Ir::LogicalOr {
            left: Box::new(resolve_ast(*left)?),
            right: Box::new(resolve_ast(*right)?),
            span,
        }),
    }
}
