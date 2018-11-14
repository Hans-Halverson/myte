use common::error::{ErrorContext, MyteError, MyteErrorType};
use common::ident::{SymbolTable, UnresolvedVariable};
use common::span::Span;
use ir::ir::{IrExpr, IrPat, IrStmt};
use parser::ast::{AstExpr, AstPat, AstStmt, BinaryOp, UnaryOp};

struct Resolver<'s, 'e> {
    symbol_table: &'s mut SymbolTable,
    error_context: &'e mut ErrorContext,
}

impl<'s, 'e> Resolver<'s, 'e> {
    pub fn new(
        symbol_table: &'s mut SymbolTable,
        error_context: &'e mut ErrorContext,
    ) -> Resolver<'s, 'e> {
        Resolver {
            symbol_table,
            error_context,
        }
    }

    fn resolve_expr(&mut self, expr: AstExpr) -> Option<IrExpr> {
        match expr {
            AstExpr::UnitLiteral { span } => Some(IrExpr::UnitLiteral { span }),
            AstExpr::BoolLiteral { bool, span } => Some(IrExpr::BoolLiteral { bool, span }),
            AstExpr::StringLiteral { string, span } => Some(IrExpr::StringLiteral { string, span }),
            AstExpr::IntLiteral { num, span } => Some(IrExpr::IntLiteral { num, span }),
            AstExpr::FloatLiteral { num, span } => Some(IrExpr::FloatLiteral { num, span }),
            AstExpr::Variable { var, span } => self.resolve_variable(var, span),
            AstExpr::UnaryOp { node, op, span } => self.resolve_unary_op(*node, op, span),
            AstExpr::BinaryOp {
                left,
                right,
                op,
                span,
            } => self.resolve_binary_op(*left, *right, op, span),
            AstExpr::ParenthesizedGroup { node, span } => Some(IrExpr::ParenthesizedGroup {
                node: Box::new(self.resolve_expr(*node)?),
                span,
            }),
            AstExpr::Block { nodes, span } => self.resolve_block(nodes, span),
        }
    }

    fn resolve_stmt(&mut self, stmt: AstStmt) -> Option<IrStmt> {
        match stmt {
            AstStmt::Expr { expr } => Some(IrStmt::Expr {
                expr: Box::new(self.resolve_expr(*expr)?),
            }),
            AstStmt::VariableDefinition {
                lvalue,
                rvalue,
                span,
            } => self.resolve_variable_definition(*lvalue, *rvalue, span),
        }
    }

    fn resolve_pat(&mut self, pat: AstPat) -> Option<IrPat> {
        match pat {
            AstPat::Variable { var, span } => Some(IrPat::Variable { var, span }),
        }
    }

    fn resolve_variable(&mut self, var: UnresolvedVariable, span: Span) -> Option<IrExpr> {
        let var = match self.symbol_table.resolve_variable(&var) {
            Some(var_id) => var_id,
            None => {
                self.error_context.add_error(MyteError::new(
                    format!("Unkown variable {}", var.name),
                    &span,
                    MyteErrorType::Resolve,
                ));
                return None;
            }
        };

        Some(IrExpr::Variable { var, span })
    }

    fn resolve_binary_op(
        &mut self,
        left: AstExpr,
        right: AstExpr,
        op: BinaryOp,
        span: Span,
    ) -> Option<IrExpr> {
        let left_ir = self.resolve_expr(left);
        let right_ir = self.resolve_expr(right);

        let left = Box::new(left_ir?);
        let right = Box::new(right_ir?);

        match op {
            BinaryOp::Add => Some(IrExpr::Add { left, right, span }),
            BinaryOp::Subtract => Some(IrExpr::Subtract { left, right, span }),
            BinaryOp::Multiply => Some(IrExpr::Multiply { left, right, span }),
            BinaryOp::Divide => Some(IrExpr::Divide { left, right, span }),
            BinaryOp::Exponentiate => Some(IrExpr::Exponentiate { left, right, span }),
            BinaryOp::Remainder => Some(IrExpr::Remainder { left, right, span }),
            BinaryOp::LogicalAnd => Some(IrExpr::LogicalAnd { left, right, span }),
            BinaryOp::LogicalOr => Some(IrExpr::LogicalOr { left, right, span }),
        }
    }

    fn resolve_unary_op(&mut self, node: AstExpr, op: UnaryOp, span: Span) -> Option<IrExpr> {
        let node_ir = self.resolve_expr(node);
        let node = Box::new(node_ir?);
        match op {
            UnaryOp::Plus => Some(IrExpr::UnaryPlus { node, span }),
            UnaryOp::Minus => Some(IrExpr::UnaryMinus { node, span }),
            UnaryOp::LogicalNot => Some(IrExpr::LogicalNot { node, span }),
        }
    }

    fn resolve_block(&mut self, nodes: Vec<AstStmt>, span: Span) -> Option<IrExpr> {
        let ir_nodes = nodes
            .into_iter()
            .map(|node| self.resolve_stmt(node))
            .collect::<Vec<Option<IrStmt>>>();
        if ir_nodes.iter().any(|node| node.is_none()) {
            return None;
        }

        Some(IrExpr::Block {
            nodes: ir_nodes.into_iter().flatten().collect::<Vec<IrStmt>>(),
            span,
        })
    }

    fn resolve_variable_definition(
        &mut self,
        lvalue: AstPat,
        rvalue: AstExpr,
        span: Span,
    ) -> Option<IrStmt> {
        let lvalue = self.resolve_pat(lvalue)?;
        let rvalue = self.resolve_expr(rvalue)?;

        Some(IrStmt::VariableDefinition {
            lvalue: Box::new(lvalue),
            rvalue: Box::new(rvalue),
            span,
        })
    }
}

pub fn resolve(
    stmt: AstStmt,
    symbol_table: &mut SymbolTable,
    error_context: &mut ErrorContext,
) -> Option<IrStmt> {
    let mut resolver = Resolver::new(symbol_table, error_context);
    resolver.resolve_stmt(stmt)
}
