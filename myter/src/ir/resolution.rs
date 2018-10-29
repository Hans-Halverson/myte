use common::error::ErrorContext;
use common::span::Span;
use ir::ir::Ir;
use parser::ast::{AstExpr, BinaryOp, UnaryOp};

struct Resolver<'a> {
    error_context: &'a mut ErrorContext,
}

impl<'a> Resolver<'a> {
    pub fn new(error_context: &'a mut ErrorContext) -> Resolver<'a> {
        Resolver { error_context }
    }

    fn resolve_ast(&mut self, ast: AstExpr) -> Option<Ir> {
        match ast {
            AstExpr::UnitLiteral { span } => Some(Ir::UnitLiteral { span }),
            AstExpr::BoolLiteral { bool, span } => Some(Ir::BoolLiteral { bool, span }),
            AstExpr::StringLiteral { string, span } => Some(Ir::StringLiteral { string, span }),
            AstExpr::IntLiteral { num, span } => Some(Ir::IntLiteral { num, span }),
            AstExpr::FloatLiteral { num, span } => Some(Ir::FloatLiteral { num, span }),
            AstExpr::UnaryOp { node, op, span } => self.resolve_unary_op(*node, op, span),
            AstExpr::BinaryOp {
                left,
                right,
                op,
                span,
            } => self.resolve_binary_op(*left, *right, op, span),
            AstExpr::ParenthesizedGroup { node, span } => Some(Ir::ParenthesizedGroup {
                node: Box::new(self.resolve_ast(*node)?),
                span,
            }),
            AstExpr::Block { nodes, span } => self.resolve_block(nodes, span),
        }
    }

    fn resolve_binary_op(
        &mut self,
        left: AstExpr,
        right: AstExpr,
        op: BinaryOp,
        span: Span,
    ) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);

        let left = Box::new(left_ir?);
        let right = Box::new(right_ir?);

        match op {
            BinaryOp::Add => Some(Ir::Add { left, right, span }),
            BinaryOp::Subtract => Some(Ir::Subtract { left, right, span }),
            BinaryOp::Multiply => Some(Ir::Multiply { left, right, span }),
            BinaryOp::Divide => Some(Ir::Divide { left, right, span }),
            BinaryOp::Exponentiate => Some(Ir::Exponentiate { left, right, span }),
            BinaryOp::Remainder => Some(Ir::Remainder { left, right, span }),
            BinaryOp::LogicalAnd => Some(Ir::LogicalAnd { left, right, span }),
            BinaryOp::LogicalOr => Some(Ir::LogicalOr { left, right, span }),
        }
    }

    fn resolve_unary_op(&mut self, node: AstExpr, op: UnaryOp, span: Span) -> Option<Ir> {
        let node_ir = self.resolve_ast(node);
        let node = Box::new(node_ir?);
        match op {
            UnaryOp::Plus => Some(Ir::UnaryPlus { node, span }),
            UnaryOp::Minus => Some(Ir::UnaryMinus { node, span }),
            UnaryOp::LogicalNot => Some(Ir::LogicalNot { node, span }),
        }
    }

    fn resolve_block(&mut self, nodes: Vec<AstExpr>, span: Span) -> Option<Ir> {
        let ir_nodes = nodes
            .into_iter()
            .map(|node| self.resolve_ast(node))
            .collect::<Vec<Option<Ir>>>();
        if ir_nodes.iter().any(|node| node.is_none()) {
            return None;
        }

        Some(Ir::Block {
            nodes: ir_nodes.into_iter().flatten().collect::<Vec<Ir>>(),
            span,
        })
    }
}

pub fn resolve(ast: AstExpr, error_context: &mut ErrorContext) -> Option<Ir> {
    let mut resolver = Resolver::new(error_context);
    resolver.resolve_ast(ast)
}
