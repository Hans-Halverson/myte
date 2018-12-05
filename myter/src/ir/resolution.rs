use common::context::Context;
use common::error::{MyteError, MyteErrorType};
use common::ident::{IdentifierID, UnresolvedType, UnresolvedVariable};
use common::span::Span;
use ir::ir::{IrExpr, IrExprType, IrID, IrPat, IrPatType, IrStmt, IrStmtType};
use parser::ast::{AstExpr, AstExprType, AstPat, AstStmt, AstType, AstTypeType, BinaryOp, UnaryOp};
use types::infer::InferType;

struct Resolver<'ctx> {
    ctx: &'ctx mut Context,
}

impl<'ctx> Resolver<'ctx> {
    pub fn new(ctx: &mut Context) -> Resolver {
        Resolver { ctx }
    }

    fn new_id(&mut self) -> IrID {
        let Context {
            ref mut ir_ctx,
            ref mut infer_ctx,
            ..
        } = self.ctx;

        let ir_id = ir_ctx.new_ir_id();
        let infer_var_id = infer_ctx.new_infer_var_id();

        infer_ctx.ir_to_type.insert(ir_id, infer_var_id);

        ir_id
    }

    fn resolve_expr(&mut self, expr: AstExpr) -> Option<IrExpr> {
        let span = expr.span;
        match expr.node {
            AstExprType::UnitLiteral => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::UnitLiteral,
            }),
            AstExprType::BoolLiteral(bool) => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::BoolLiteral(bool),
            }),
            AstExprType::StringLiteral(string) => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::StringLiteral(string),
            }),
            AstExprType::IntLiteral(num) => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::IntLiteral(num),
            }),
            AstExprType::FloatLiteral(num) => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::FloatLiteral(num),
            }),
            AstExprType::Variable(var) => self.resolve_variable(var, span),
            AstExprType::UnaryOp { node, op } => self.resolve_unary_op(*node, op, span),
            AstExprType::BinaryOp { left, right, op } => {
                self.resolve_binary_op(*left, *right, op, span)
            }
            AstExprType::ParenthesizedGroup(node) => Some(self.resolve_expr(*node)?),
            AstExprType::Block(nodes) => self.resolve_block(nodes, span),
            AstExprType::If {
                cond,
                conseq,
                altern,
            } => self.resolve_if_expr(*cond, *conseq, *altern, span),
            AstExprType::Application { func, args } => self.resolve_application(*func, args, span),
            AstExprType::Assignment { var, expr } => self.resolve_assignment(var, *expr, span),
        }
    }

    fn resolve_stmt(&mut self, stmt: AstStmt) -> Option<IrStmt> {
        match stmt {
            AstStmt::Expr { expr } => Some(IrStmt {
                id: self.new_id(),
                span: expr.span,
                node: IrStmtType::Expr(Box::new(self.resolve_expr(*expr)?)),
            }),
            AstStmt::VariableDefinition {
                lvalue,
                rvalue,
                annot,
                span,
            } => self.resolve_variable_definition(*lvalue, *rvalue, annot, span),
            AstStmt::FunctionDefinition {
                name,
                params,
                body,
                return_annot,
                span,
            } => self.resolve_function_definition(name, params, *body, return_annot, span),
            AstStmt::If { cond, conseq, span } => self.resolve_if_stmt(*cond, *conseq, span),
        }
    }

    fn resolve_pat(&mut self, pat: AstPat) -> Option<IrPat> {
        match pat {
            AstPat::Variable { var, span } => Some(IrPat {
                span,
                id: self.new_id(),
                pat: IrPatType::Variable(var),
            }),
        }
    }

    fn resolve_type(&mut self, ty: AstType) -> Option<InferType> {
        match ty.ty {
            AstTypeType::Unit => Some(InferType::Unit),
            AstTypeType::Bool => Some(InferType::Bool),
            AstTypeType::Int => Some(InferType::Int),
            AstTypeType::Float => Some(InferType::Float),
            AstTypeType::String => Some(InferType::String),
            AstTypeType::Variable(var) => self.resolve_variable_type(var, ty.span),
            AstTypeType::Function(arg_tys, ret_ty) => self.resolve_function_type(arg_tys, *ret_ty),
        }
    }

    fn resolve_variable(&mut self, var: UnresolvedVariable, span: Span) -> Option<IrExpr> {
        let var = match self.ctx.symbol_table.resolve_variable(&var) {
            Some(var_id) => var_id,
            None => {
                self.ctx.error_ctx.add_error(MyteError::new(
                    format!("Unknown variable {}", var.name),
                    &span,
                    MyteErrorType::Resolve,
                ));
                return None;
            }
        };

        Some(IrExpr {
            span,
            id: self.new_id(),
            node: IrExprType::Variable(var),
        })
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
            BinaryOp::Add => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::Add { left, right },
            }),
            BinaryOp::Subtract => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::Subtract { left, right },
            }),
            BinaryOp::Multiply => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::Multiply { left, right },
            }),
            BinaryOp::Divide => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::Divide { left, right },
            }),
            BinaryOp::Exponentiate => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::Exponentiate { left, right },
            }),
            BinaryOp::Remainder => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::Remainder { left, right },
            }),
            BinaryOp::LogicalAnd => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::LogicalAnd { left, right },
            }),
            BinaryOp::LogicalOr => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::LogicalOr { left, right },
            }),
            BinaryOp::Equals => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::Equals { left, right },
            }),
            BinaryOp::NotEqual => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::NotEqual { left, right },
            }),
            BinaryOp::LessThan => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::LessThan { left, right },
            }),
            BinaryOp::LessThanOrEqual => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::LessThanOrEqual { left, right },
            }),
            BinaryOp::GreaterThan => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::GreaterThan { left, right },
            }),
            BinaryOp::GreaterThanOrEqual => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::GreaterThanOrEqual { left, right },
            }),
        }
    }

    fn resolve_unary_op(&mut self, node: AstExpr, op: UnaryOp, span: Span) -> Option<IrExpr> {
        let node_ir = self.resolve_expr(node);
        let node = Box::new(node_ir?);
        match op {
            UnaryOp::Plus => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::UnaryPlus(node),
            }),
            UnaryOp::Minus => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::UnaryMinus(node),
            }),
            UnaryOp::LogicalNot => Some(IrExpr {
                span,
                id: self.new_id(),
                node: IrExprType::LogicalNot(node),
            }),
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

        Some(IrExpr {
            span,
            id: self.new_id(),
            node: IrExprType::Block(ir_nodes.into_iter().flatten().collect::<Vec<IrStmt>>()),
        })
    }

    fn resolve_if_expr(
        &mut self,
        cond: AstExpr,
        conseq: AstExpr,
        altern: AstExpr,
        span: Span,
    ) -> Option<IrExpr> {
        Some(IrExpr {
            span,
            id: self.new_id(),
            node: IrExprType::If {
                cond: Box::new(self.resolve_expr(cond)?),
                conseq: Box::new(self.resolve_expr(conseq)?),
                altern: Box::new(self.resolve_expr(altern)?),
            },
        })
    }

    fn resolve_application(
        &mut self,
        func: AstExpr,
        args: Vec<AstExpr>,
        span: Span,
    ) -> Option<IrExpr> {
        let ir_args = args
            .into_iter()
            .map(|arg| self.resolve_expr(arg))
            .collect::<Vec<Option<IrExpr>>>();
        if ir_args.iter().any(|arg| arg.is_none()) {
            return None;
        }

        Some(IrExpr {
            span,
            id: self.new_id(),
            node: IrExprType::Application {
                func: Box::new(self.resolve_expr(func)?),
                args: ir_args.into_iter().flatten().collect::<Vec<IrExpr>>(),
            },
        })
    }

    fn resolve_assignment(
        &mut self,
        var: UnresolvedVariable,
        expr: AstExpr,
        span: Span,
    ) -> Option<IrExpr> {
        let var = match self.ctx.symbol_table.resolve_variable(&var) {
            Some(var_id) => var_id,
            None => {
                self.ctx.error_ctx.add_error(MyteError::new(
                    format!("Unknown variable {}", var.name),
                    &span,
                    MyteErrorType::Resolve,
                ));
                return None;
            }
        };

        Some(IrExpr {
            span,
            id: self.new_id(),
            node: IrExprType::Assignment {
                var,
                expr: Box::new(self.resolve_expr(expr)?),
            },
        })
    }

    fn resolve_variable_definition(
        &mut self,
        lvalue: AstPat,
        rvalue: AstExpr,
        annot: Option<Box<AstType>>,
        span: Span,
    ) -> Option<IrStmt> {
        let lvalue = self.resolve_pat(lvalue)?;
        let rvalue = self.resolve_expr(rvalue)?;
        let has_annot = annot.is_some();

        let ty = match annot {
            None => InferType::InferVariable(self.ctx.infer_ctx.new_infer_var_id()),
            Some(annot) => self.resolve_type(*annot)?,
        };

        self.match_annotated_pattern(&lvalue, &ty);

        Some(IrStmt {
            span,
            id: self.new_id(),
            node: IrStmtType::VariableDefinition {
                lvalue: Box::new(lvalue),
                rvalue: Box::new(rvalue),
                has_annot,
            },
        })
    }

    fn match_annotated_pattern(&mut self, pat: &IrPat, ty: &InferType) {
        let IrPat {
            pat: IrPatType::Variable(var),
            ..
        } = pat;
        self.ctx.infer_ctx.ident_to_type.insert(*var, ty.clone());
    }

    fn resolve_function_definition(
        &mut self,
        name: IdentifierID,
        params: Vec<(IdentifierID, Box<AstType>)>,
        body: AstExpr,
        return_annot: Option<Box<AstType>>,
        span: Span,
    ) -> Option<IrStmt> {
        let body = self.resolve_expr(body)?;

        let param_ids = params.iter().map(|(param_id, _)| *param_id).collect::<Vec<IdentifierID>>();
        let param_opt_tys = params
            .into_iter()
            .map(|(_, param_ty)| self.resolve_type(*param_ty))
            .collect::<Vec<Option<InferType>>>();
        let return_ty = match return_annot {
            Some(annot) => self.resolve_type(*annot),
            None => Some(InferType::Unit),
        };

        if param_opt_tys
            .iter()
            .any(|node| node.is_none() || return_ty.is_none())
        {
            return None;
        }

        let param_tys = param_opt_tys.into_iter().flatten().collect::<Vec<InferType>>();
        for (param_id, param_ty) in param_ids.iter().zip(&param_tys) {
            self.ctx.infer_ctx.ident_to_type.insert(*param_id, param_ty.clone());
        }

        let func_ty = InferType::Function(
            param_tys,
            Box::new(return_ty.unwrap()),
        );

        self.ctx.infer_ctx.ident_to_type.insert(name, func_ty);

        Some(IrStmt {
            span,
            id: self.new_id(),
            node: IrStmtType::FunctionDefinition {
                name,
                params: param_ids,
                body: Box::new(body),
            },
        })
    }

    fn resolve_if_stmt(&mut self, cond: AstExpr, conseq: AstExpr, span: Span) -> Option<IrStmt> {
        Some(IrStmt {
            span,
            id: self.new_id(),
            node: IrStmtType::If {
                cond: Box::new(self.resolve_expr(cond)?),
                conseq: Box::new(self.resolve_expr(conseq)?),
            },
        })
    }

    fn resolve_variable_type(&mut self, var: UnresolvedType, span: Span) -> Option<InferType> {
        let var = match self.ctx.symbol_table.resolve_type(&var) {
            Some(var_id) => var_id,
            None => {
                self.ctx.error_ctx.add_error(MyteError::new(
                    format!("Unknown type {}", var.name),
                    &span,
                    MyteErrorType::Resolve,
                ));
                return None;
            }
        };

        if self.ctx.infer_ctx.type_ident_to_type.contains_key(&var) {
            Some(self.ctx.infer_ctx.type_ident_to_type[&var].clone())
        } else {
            let param_ty = InferType::ParamVariable(self.ctx.infer_ctx.new_param_var_id());
            self.ctx
                .infer_ctx
                .type_ident_to_type
                .insert(var, param_ty.clone());
            Some(param_ty)
        }
    }

    fn resolve_function_type(
        &mut self,
        arg_tys: Vec<AstType>,
        ret_ty: AstType,
    ) -> Option<InferType> {
        let arg_tys = arg_tys
            .into_iter()
            .map(|ty| self.resolve_type(ty))
            .collect::<Vec<Option<InferType>>>();
        if arg_tys.iter().any(|ty| ty.is_none()) {
            return None;
        }

        let ret_ty = self.resolve_type(ret_ty)?;

        Some(InferType::Function(
            arg_tys.into_iter().flatten().collect::<Vec<InferType>>(),
            Box::new(ret_ty),
        ))
    }
}

pub fn resolve_repl_line(stmt: AstStmt, ctx: &mut Context) -> Option<IrStmt> {
    let mut resolver = Resolver::new(ctx);
    resolver.resolve_stmt(stmt)
}

pub fn resolve_file(stmts: Vec<AstStmt>, ctx: &mut Context) -> Vec<IrStmt> {
    let mut resolver = Resolver::new(ctx);
    stmts
        .into_iter()
        .filter_map(|stmt| resolver.resolve_stmt(stmt))
        .collect()
}
