use common::context::Context;
use common::error::{MyteError, MyteErrorType};
use common::ident::{IdentifierID, UnresolvedType, UnresolvedVariable};
use common::loc::Loc;
use ir::nodes::{IrExpr, IrExprType, IrID, IrPat, IrPatType, IrStmt, IrStmtType};
use parse::ast::{
    BinaryOp, Expr, ExprKind, FuncDecl, Module, Pat, PatKind, Stmt, StmtKind, TopLevel,
    TopLevelKind, Type, TypeKind, UnaryOp, VarDecl,
};
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

    fn resolve_expr(&mut self, expr: Expr) -> Option<IrExpr> {
        let loc = expr.loc;
        match expr.kind {
            ExprKind::UnitLiteral => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::UnitLiteral,
            }),
            ExprKind::BoolLiteral(bool) => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::BoolLiteral(bool),
            }),
            ExprKind::StringLiteral(string) => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::StringLiteral(string),
            }),
            ExprKind::IntLiteral(num) => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::IntLiteral(num),
            }),
            ExprKind::FloatLiteral(num) => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::FloatLiteral(num),
            }),
            ExprKind::TupleLiteral(elements) => self.resolve_tuple_literal(elements, loc),
            ExprKind::Variable(var) => self.resolve_variable(&var, loc),
            ExprKind::UnaryOp { node, op } => self.resolve_unary_op(*node, op, loc),
            ExprKind::BinaryOp { left, right, op } => {
                self.resolve_binary_op(*left, *right, op, loc)
            }
            ExprKind::ParenthesizedGroup(node) => Some(self.resolve_expr(*node)?),
            ExprKind::If {
                cond,
                conseq,
                altern,
            } => self.resolve_if_expr(*cond, *conseq, *altern, loc),
            ExprKind::Application { func, args } => self.resolve_application(*func, args, loc),
            ExprKind::Assignment { var, expr } => self.resolve_assignment(&var, *expr, loc),
        }
    }

    fn resolve_top_level(&mut self, top_level: TopLevel) -> Option<IrStmt> {
        match top_level.kind {
            TopLevelKind::VarDecl(VarDecl {
                lvalue,
                rvalue,
                annot,
            }) => self.resolve_variable_definition(*lvalue, *rvalue, annot, top_level.loc),
            TopLevelKind::FuncDecl(FuncDecl {
                name,
                params,
                body,
                return_annot,
            }) => {
                self.resolve_function_definition(name, params, *body, return_annot, top_level.loc)
            }
        }
    }

    fn resolve_stmt(&mut self, stmt: Stmt) -> Option<IrStmt> {
        match stmt.kind {
            StmtKind::Expr(expr) => Some(IrStmt {
                id: self.new_id(),
                loc: stmt.loc,
                node: IrStmtType::Expr(Box::new(self.resolve_expr(*expr)?)),
            }),
            StmtKind::VarDecl(VarDecl {
                lvalue,
                rvalue,
                annot,
            }) => self.resolve_variable_definition(*lvalue, *rvalue, annot, stmt.loc),
            StmtKind::FuncDecl(FuncDecl {
                name,
                params,
                body,
                return_annot,
            }) => self.resolve_function_definition(name, params, *body, return_annot, stmt.loc),
            StmtKind::Block(nodes) => self.resolve_block(nodes, stmt.loc),
            StmtKind::If { cond, conseq } => self.resolve_if_stmt(*cond, *conseq, stmt.loc),
            StmtKind::While { cond, body } => self.resolve_while_stmt(*cond, *body, stmt.loc),
            StmtKind::Return(expr) => Some(IrStmt {
                id: self.new_id(),
                loc: stmt.loc,
                node: IrStmtType::Return(Box::new(self.resolve_expr(*expr)?)),
            }),
            StmtKind::Break => Some(IrStmt {
                id: self.new_id(),
                loc: stmt.loc,
                node: IrStmtType::Break,
            }),
            StmtKind::Continue => Some(IrStmt {
                id: self.new_id(),
                loc: stmt.loc,
                node: IrStmtType::Continue,
            }),
        }
    }

    fn resolve_pat(&mut self, pat: Pat) -> Option<IrPat> {
        match pat.kind {
            PatKind::Variable(var) => Some(IrPat {
                loc: pat.loc,
                id: self.new_id(),
                pat: IrPatType::Variable(var),
            }),
            PatKind::Tuple(elements) => {
                let ir_elements = elements
                    .into_iter()
                    .map(|element| self.resolve_pat(element))
                    .collect::<Vec<Option<IrPat>>>();
                if ir_elements.iter().any(|element| element.is_none()) {
                    return None;
                }

                Some(IrPat {
                    loc: pat.loc,
                    id: self.new_id(),
                    pat: IrPatType::Tuple(ir_elements.into_iter().flatten().collect()),
                })
            }
        }
    }

    fn resolve_type(&mut self, ty: Type) -> Option<InferType> {
        match ty.kind {
            TypeKind::Unit => Some(InferType::Unit),
            TypeKind::Bool => Some(InferType::Bool),
            TypeKind::Int => Some(InferType::Int),
            TypeKind::Float => Some(InferType::Float),
            TypeKind::String => Some(InferType::String),
            TypeKind::Variable(var) => self.resolve_variable_type(&var, ty.loc),
            TypeKind::Function(arg_tys, ret_ty) => self.resolve_function_type(arg_tys, *ret_ty),
            TypeKind::Tuple(element_tys) => self.resolve_tuple_type(element_tys),
        }
    }

    fn resolve_variable(&mut self, var: &UnresolvedVariable, loc: Loc) -> Option<IrExpr> {
        let var = match self.ctx.symbol_table.resolve_variable(&var) {
            Some(var_id) => var_id,
            None => {
                self.ctx.error_ctx.add_error(MyteError::new(
                    format!("Unknown variable {}", var.name),
                    &loc,
                    MyteErrorType::Resolve,
                ));
                return None;
            }
        };

        Some(IrExpr {
            loc,
            id: self.new_id(),
            node: IrExprType::Variable(var),
        })
    }

    fn resolve_tuple_literal(&mut self, elements: Vec<Expr>, loc: Loc) -> Option<IrExpr> {
        let ir_elements = elements
            .into_iter()
            .map(|element| self.resolve_expr(element))
            .collect::<Vec<Option<IrExpr>>>();
        if ir_elements.iter().any(|element| element.is_none()) {
            return None;
        }

        Some(IrExpr {
            loc,
            id: self.new_id(),
            node: IrExprType::TupleLiteral(ir_elements.into_iter().flatten().collect()),
        })
    }

    fn resolve_binary_op(
        &mut self,
        left: Expr,
        right: Expr,
        op: BinaryOp,
        loc: Loc,
    ) -> Option<IrExpr> {
        let left_ir = self.resolve_expr(left);
        let right_ir = self.resolve_expr(right);

        let left = Box::new(left_ir?);
        let right = Box::new(right_ir?);

        match op {
            BinaryOp::Add => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::Add { left, right },
            }),
            BinaryOp::Subtract => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::Subtract { left, right },
            }),
            BinaryOp::Multiply => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::Multiply { left, right },
            }),
            BinaryOp::Divide => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::Divide { left, right },
            }),
            BinaryOp::Exponentiate => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::Exponentiate { left, right },
            }),
            BinaryOp::Remainder => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::Remainder { left, right },
            }),
            BinaryOp::LogicalAnd => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::LogicalAnd { left, right },
            }),
            BinaryOp::LogicalOr => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::LogicalOr { left, right },
            }),
            BinaryOp::Equals => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::Equals { left, right },
            }),
            BinaryOp::NotEqual => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::NotEqual { left, right },
            }),
            BinaryOp::LessThan => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::LessThan { left, right },
            }),
            BinaryOp::LessThanOrEqual => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::LessThanOrEqual { left, right },
            }),
            BinaryOp::GreaterThan => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::GreaterThan { left, right },
            }),
            BinaryOp::GreaterThanOrEqual => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::GreaterThanOrEqual { left, right },
            }),
        }
    }

    fn resolve_unary_op(&mut self, node: Expr, op: UnaryOp, loc: Loc) -> Option<IrExpr> {
        let node_ir = self.resolve_expr(node);
        let node = Box::new(node_ir?);
        match op {
            UnaryOp::Plus => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::UnaryPlus(node),
            }),
            UnaryOp::Minus => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::UnaryMinus(node),
            }),
            UnaryOp::LogicalNot => Some(IrExpr {
                loc,
                id: self.new_id(),
                node: IrExprType::LogicalNot(node),
            }),
        }
    }

    fn resolve_block(&mut self, nodes: Vec<Stmt>, loc: Loc) -> Option<IrStmt> {
        let ir_nodes = nodes
            .into_iter()
            .map(|node| self.resolve_stmt(node))
            .collect::<Vec<Option<IrStmt>>>();
        if ir_nodes.iter().any(|node| node.is_none()) {
            return None;
        }

        Some(IrStmt {
            loc,
            id: self.new_id(),
            node: IrStmtType::Block(ir_nodes.into_iter().flatten().collect()),
        })
    }

    fn resolve_if_expr(
        &mut self,
        cond: Expr,
        conseq: Expr,
        altern: Expr,
        loc: Loc,
    ) -> Option<IrExpr> {
        Some(IrExpr {
            loc,
            id: self.new_id(),
            node: IrExprType::If {
                cond: Box::new(self.resolve_expr(cond)?),
                conseq: Box::new(self.resolve_expr(conseq)?),
                altern: Box::new(self.resolve_expr(altern)?),
            },
        })
    }

    fn resolve_application(&mut self, func: Expr, args: Vec<Expr>, loc: Loc) -> Option<IrExpr> {
        let ir_args = args
            .into_iter()
            .map(|arg| self.resolve_expr(arg))
            .collect::<Vec<Option<IrExpr>>>();
        if ir_args.iter().any(|arg| arg.is_none()) {
            return None;
        }

        Some(IrExpr {
            loc,
            id: self.new_id(),
            node: IrExprType::Application {
                func: Box::new(self.resolve_expr(func)?),
                args: ir_args.into_iter().flatten().collect::<Vec<IrExpr>>(),
            },
        })
    }

    fn resolve_assignment(
        &mut self,
        var: &UnresolvedVariable,
        expr: Expr,
        loc: Loc,
    ) -> Option<IrExpr> {
        let var = match self.ctx.symbol_table.resolve_variable(&var) {
            Some(var_id) => var_id,
            None => {
                self.ctx.error_ctx.add_error(MyteError::new(
                    format!("Unknown variable {}", var.name),
                    &loc,
                    MyteErrorType::Resolve,
                ));
                return None;
            }
        };

        Some(IrExpr {
            loc,
            id: self.new_id(),
            node: IrExprType::Assignment {
                var,
                expr: Box::new(self.resolve_expr(expr)?),
            },
        })
    }

    fn resolve_variable_definition(
        &mut self,
        lvalue: Pat,
        rvalue: Expr,
        annot: Option<Box<Type>>,
        loc: Loc,
    ) -> Option<IrStmt> {
        let lvalue = self.resolve_pat(lvalue)?;
        let rvalue = self.resolve_expr(rvalue)?;
        let has_annot = annot.is_some();

        match annot {
            None => {
                self.match_unannotated_pattern(&lvalue);
            }
            Some(annot) => {
                let ty = self.resolve_type(*annot)?;
                self.match_annotated_pattern(&lvalue, &ty);
            }
        }

        Some(IrStmt {
            loc,
            id: self.new_id(),
            node: IrStmtType::VariableDefinition {
                lvalue: Box::new(lvalue),
                rvalue: Box::new(rvalue),
                has_annot,
            },
        })
    }

    fn match_unannotated_pattern(&mut self, pat: &IrPat) {
        match &pat.pat {
            IrPatType::Variable(var) => {
                let ty = InferType::InferVariable(self.ctx.infer_ctx.new_infer_var_id());
                self.ctx.infer_ctx.ident_to_type.insert(*var, ty);
            }
            IrPatType::Tuple(ref pats) => {
                for pat in pats {
                    self.match_unannotated_pattern(pat);
                }
            }
        }
    }

    fn match_annotated_pattern(&mut self, pat: &IrPat, ty: &InferType) {
        match (&pat.pat, ty) {
            (IrPatType::Variable(var), _) => {
                self.ctx.infer_ctx.ident_to_type.insert(*var, ty.clone());
            }
            (IrPatType::Tuple(ref pats), InferType::Tuple(ref types))
                if pats.len() == types.len() =>
            {
                for (pat, ty) in pats.iter().zip(types) {
                    self.match_annotated_pattern(pat, ty)
                }
            }
            _ => {
                let formatted_types = InferType::format_types(&[ty.clone()]);
                self.ctx.error_ctx.add_error(MyteError::new(
                    format!(
                        "Annotation does not match pattern. Pattern expected to match {}",
                        formatted_types[0]
                    ),
                    &pat.loc,
                    MyteErrorType::Resolve,
                ));
            }
        }
    }

    fn resolve_function_definition(
        &mut self,
        name: IdentifierID,
        params: Vec<(IdentifierID, Box<Type>)>,
        body: Stmt,
        return_annot: Option<Box<Type>>,
        loc: Loc,
    ) -> Option<IrStmt> {
        let param_ids = params
            .iter()
            .map(|(param_id, _)| *param_id)
            .collect::<Vec<IdentifierID>>();
        let param_opt_tys = params
            .into_iter()
            .map(|(_, param_ty)| self.resolve_type(*param_ty))
            .collect::<Vec<Option<InferType>>>();
        let return_ty = match return_annot {
            Some(annot) => self.resolve_type(*annot),
            None => Some(InferType::Unit),
        };

        let body = self.resolve_stmt(body)?;

        if param_opt_tys
            .iter()
            .any(|node| node.is_none() || return_ty.is_none())
        {
            return None;
        }

        let param_tys = param_opt_tys
            .into_iter()
            .flatten()
            .collect::<Vec<InferType>>();
        for (param_id, param_ty) in param_ids.iter().zip(&param_tys) {
            self.ctx
                .infer_ctx
                .ident_to_type
                .insert(*param_id, param_ty.clone());
        }

        let func_ty = InferType::Function(param_tys, Box::new(return_ty.unwrap()));

        self.ctx.infer_ctx.ident_to_type.insert(name, func_ty);

        Some(IrStmt {
            loc,
            id: self.new_id(),
            node: IrStmtType::FunctionDefinition {
                name,
                params: param_ids,
                body: Box::new(body),
            },
        })
    }

    fn resolve_if_stmt(&mut self, cond: Expr, conseq: Expr, loc: Loc) -> Option<IrStmt> {
        Some(IrStmt {
            loc,
            id: self.new_id(),
            node: IrStmtType::If {
                cond: Box::new(self.resolve_expr(cond)?),
                conseq: Box::new(self.resolve_expr(conseq)?),
            },
        })
    }

    fn resolve_while_stmt(&mut self, cond: Expr, body: Expr, loc: Loc) -> Option<IrStmt> {
        Some(IrStmt {
            loc,
            id: self.new_id(),
            node: IrStmtType::While {
                cond: Box::new(self.resolve_expr(cond)?),
                body: Box::new(self.resolve_expr(body)?),
            },
        })
    }

    fn resolve_variable_type(&mut self, var: &UnresolvedType, loc: Loc) -> Option<InferType> {
        let var = match self.ctx.symbol_table.resolve_type(var) {
            Some(var_id) => var_id,
            None => {
                self.ctx.error_ctx.add_error(MyteError::new(
                    format!("Unknown type {}", var.name),
                    &loc,
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

    fn resolve_function_type(&mut self, arg_tys: Vec<Type>, ret_ty: Type) -> Option<InferType> {
        let arg_tys = arg_tys
            .into_iter()
            .map(|ty| self.resolve_type(ty))
            .collect::<Vec<Option<InferType>>>();
        if arg_tys.iter().any(|ty| ty.is_none()) {
            return None;
        }

        let ret_ty = self.resolve_type(ret_ty)?;

        Some(InferType::Function(
            arg_tys.into_iter().flatten().collect(),
            Box::new(ret_ty),
        ))
    }

    fn resolve_tuple_type(&mut self, element_tys: Vec<Type>) -> Option<InferType> {
        let element_tys = element_tys
            .into_iter()
            .map(|ty| self.resolve_type(ty))
            .collect::<Vec<Option<InferType>>>();
        if element_tys.iter().any(|ty| ty.is_none()) {
            return None;
        }

        Some(InferType::Tuple(
            element_tys.into_iter().flatten().collect(),
        ))
    }
}

pub fn resolve_repl_line(stmt: Stmt, ctx: &mut Context) -> Option<IrStmt> {
    let mut resolver = Resolver::new(ctx);
    resolver.resolve_stmt(stmt)
}

pub fn resolve_file(module: Module, ctx: &mut Context) -> Vec<IrStmt> {
    let mut resolver = Resolver::new(ctx);
    module
        .top_levels
        .into_iter()
        .filter_map(|top_level| resolver.resolve_top_level(top_level))
        .collect()
}
