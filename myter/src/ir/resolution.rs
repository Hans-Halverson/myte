use common::error::ErrorContext;
use common::span::Span;
use ir::ir::Ir;
use parser::ast::Ast;

struct Resolver<'a> {
    error_context: &'a mut ErrorContext,
}

impl<'a> Resolver<'a> {
    pub fn new(error_context: &'a mut ErrorContext) -> Resolver<'a> {
        Resolver { error_context }
    }

    fn resolve_ast(&mut self, ast: Ast) -> Option<Ir> {
        match ast {
            Ast::UnitLiteral { span } => Some(Ir::UnitLiteral { span }),
            Ast::BoolLiteral { bool, span } => Some(Ir::BoolLiteral { bool, span }),
            Ast::StringLiteral { string, span } => Some(Ir::StringLiteral { string, span }),
            Ast::IntLiteral { num, span } => Some(Ir::IntLiteral { num, span }),
            Ast::FloatLiteral { num, span } => Some(Ir::FloatLiteral { num, span }),
            Ast::Add { left, right, span } => self.resolve_add(*left, *right, span),
            Ast::Subtract { left, right, span } => self.resolve_subtract(*left, *right, span),
            Ast::Multiply { left, right, span } => self.resolve_multiply(*left, *right, span),
            Ast::Divide { left, right, span } => self.resolve_divide(*left, *right, span),
            Ast::Exponentiate { left, right, span } => {
                self.resolve_exponentiate(*left, *right, span)
            }
            Ast::Remainder { left, right, span } => self.resolve_remainder(*left, *right, span),
            Ast::ParenthesizedGroup { node, span } => Some(Ir::ParenthesizedGroup {
                node: Box::new(self.resolve_ast(*node)?),
                span,
            }),
            Ast::UnaryPlus { node, span } => Some(Ir::UnaryPlus {
                node: Box::new(self.resolve_ast(*node)?),
                span,
            }),
            Ast::UnaryMinus { node, span } => Some(Ir::UnaryMinus {
                node: Box::new(self.resolve_ast(*node)?),
                span,
            }),
            Ast::LogicalNot { node, span } => Some(Ir::LogicalNot {
                node: Box::new(self.resolve_ast(*node)?),
                span,
            }),
            Ast::LogicalAnd { left, right, span } => self.resolve_logical_and(*left, *right, span),
            Ast::LogicalOr { left, right, span } => self.resolve_logical_or(*left, *right, span),
            Ast::Block { nodes, span } => self.resolve_block(nodes, span),
        }
    }

    fn resolve_add(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::Add {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_subtract(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::Subtract {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_multiply(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::Multiply {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_divide(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::Divide {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_exponentiate(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::Exponentiate {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_remainder(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::Remainder {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_logical_and(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::LogicalAnd {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_logical_or(&mut self, left: Ast, right: Ast, span: Span) -> Option<Ir> {
        let left_ir = self.resolve_ast(left);
        let right_ir = self.resolve_ast(right);
        Some(Ir::LogicalOr {
            left: Box::new(left_ir?),
            right: Box::new(right_ir?),
            span,
        })
    }

    fn resolve_block(&mut self, nodes: Vec<Ast>, span: Span) -> Option<Ir> {
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

pub fn resolve(ast: Ast, error_context: &mut ErrorContext) -> Option<Ir> {
    let mut resolver = Resolver::new(error_context);
    resolver.resolve_ast(ast)
}
