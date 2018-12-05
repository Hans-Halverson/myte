use std::collections::HashMap;

use common::context::Context;
use common::error::{MyteError, MyteErrorType};
use common::ident::IdentifierID;
use common::span::Span;
use ir::ir::{IrExpr, IrExprType, IrPat, IrPatType, IrStmt, IrStmtType};
use types::infer::{InferType, InferTypeVariable, InferVarID};

struct TypeChecker<'ctx> {
    ctx: &'ctx mut Context,
}

impl<'ctx> TypeChecker<'ctx> {
    pub fn new(ctx: &mut Context) -> TypeChecker {
        TypeChecker { ctx }
    }

    fn unify(&mut self, ty1: &InferType, ty2: &InferType) -> bool {
        self.ctx.infer_ctx.graph.unify(ty1, ty2)
    }

    fn rep(&mut self, ty: &InferType) -> InferType {
        self.ctx.infer_ctx.graph.rep(ty)
    }

    fn expr_var(&self, expr: &IrExpr) -> InferType {
        InferType::InferVariable(self.ctx.infer_ctx.ir_to_type[&expr.id])
    }

    fn stmt_var(&self, stmt: &IrStmt) -> InferType {
        InferType::InferVariable(self.ctx.infer_ctx.ir_to_type[&stmt.id])
    }

    fn pat_var(&self, pat: &IrPat) -> InferType {
        InferType::InferVariable(self.ctx.infer_ctx.ir_to_type[&pat.id])
    }

    pub fn check_expr(&mut self, expr: &IrExpr) {
        let IrExpr { node, span, .. } = expr;
        let ty = self.expr_var(expr);
        match node {
            IrExprType::UnitLiteral => self.check_literal(&ty, span, "unit", &InferType::Unit),
            IrExprType::BoolLiteral(_) => self.check_literal(&ty, span, "bool", &InferType::Bool),
            IrExprType::IntLiteral(_) => self.check_literal(&ty, span, "int", &InferType::Int),
            IrExprType::FloatLiteral(_) => {
                self.check_literal(&ty, span, "float", &InferType::Float)
            }
            IrExprType::StringLiteral(_) => {
                self.check_literal(&ty, span, "string", &InferType::String)
            }
            IrExprType::Variable(var) => self.check_variable(var, &ty, span),
            IrExprType::Add { left, right } => {
                self.check_binary_math_expr(left, right, &ty, span, "add")
            }
            IrExprType::Subtract { left, right } => {
                self.check_binary_math_expr(left, right, &ty, span, "subtract")
            }
            IrExprType::Multiply { left, right } => {
                self.check_binary_math_expr(left, right, &ty, span, "multiply")
            }
            IrExprType::Divide { left, right } => {
                self.check_binary_math_expr(left, right, &ty, span, "divide")
            }
            IrExprType::Exponentiate { left, right } => {
                self.check_binary_math_expr(left, right, &ty, span, "exponentiate")
            }
            IrExprType::Remainder { left, right } => {
                self.check_binary_math_expr(left, right, &ty, span, "remainder")
            }
            IrExprType::UnaryPlus(node) => {
                self.check_unary_math_expr(node, &ty, span, "unary plus")
            }
            IrExprType::UnaryMinus(node) => {
                self.check_unary_math_expr(node, &ty, span, "unary minus")
            }
            IrExprType::LogicalNot(node) => self.check_logical_not(node, &ty, span),
            IrExprType::LogicalAnd { left, right } => {
                self.check_binary_logical_expr(left, right, &ty, span, "logical and")
            }
            IrExprType::LogicalOr { left, right } => {
                self.check_binary_logical_expr(left, right, &ty, span, "logical or")
            }
            IrExprType::Equals { left, right }
            | IrExprType::NotEqual { left, right }
            | IrExprType::LessThan { left, right }
            | IrExprType::LessThanOrEqual { left, right }
            | IrExprType::GreaterThan { left, right }
            | IrExprType::GreaterThanOrEqual { left, right } => {
                self.check_comparison(left, right, &ty, span)
            }
            IrExprType::Block(nodes) => self.check_block(nodes, &ty, span),
            IrExprType::If {
                cond,
                conseq,
                altern,
            } => self.check_if_expr(cond, conseq, altern, &ty, span),
            IrExprType::Application { func, args } => self.check_application(func, args, &ty, span),
            IrExprType::Assignment { var, expr } => self.check_assignment(var, expr, &ty, span),
        }
    }

    pub fn check_stmt(&mut self, stmt: &IrStmt) {
        let IrStmt { node, span, .. } = stmt;
        let ty = self.stmt_var(stmt);
        match node {
            IrStmtType::Expr(expr) => self.check_stmt_expr(expr, &ty, span),
            IrStmtType::If { cond, conseq } => self.check_if_stmt(cond, conseq, &ty, span),
            IrStmtType::VariableDefinition { lvalue, rvalue, .. } => {
                self.check_variable_definition(lvalue, rvalue, &ty, span)
            }
            IrStmtType::FunctionDefinition { name, body, .. } => self.check_function_definition(name, body, &ty, span),
        }
    }

    pub fn check_pat(&mut self, pattern: &IrPat) {
        let IrPat { pat, span, .. } = pattern;
        let ty = self.pat_var(pattern);
        match pat {
            IrPatType::Variable(var) => self.check_variable_pat(var, &ty, span),
        }
    }

    fn check_literal(&mut self, ty: &InferType, span: &Span, name: &str, expected: &InferType) {
        if !self.unify(ty, expected) {
            add_formatted_type_error!(
                self,
                "Expected {} literal to have type {}, but found {}",
                name,
                expected,
                ty,
                span
            );
        }
    }

    fn check_variable(&mut self, var: &IdentifierID, ty: &InferType, span: &Span) {
        let var_ty = self.ctx.infer_ctx.ident_to_type[var].clone();
        let name = self.ctx.symbol_table.get_ident(*var).name.clone();

        let rep_ty = self.rep(&var_ty);
        let refreshed_rep_ty = self.refresh(&rep_ty);

        if !self.unify(ty, &refreshed_rep_ty) {
            add_formatted_type_error!(
                self,
                "Expected {} to have type {}, but found {}",
                name,
                ty,
                &refreshed_rep_ty,
                span
            );
        }
    }

    fn check_binary_math_expr(
        &mut self,
        left: &IrExpr,
        right: &IrExpr,
        ty: &InferType,
        span: &Span,
        name: &str,
    ) {
        self.check_expr(left);
        self.check_expr(right);

        let left_ty = self.expr_var(left);
        let right_ty = self.expr_var(right);

        if !self.unify(&left_ty, &right_ty) {
            add_formatted_type_error!(
                self,
                "Expected both arguments to {} to have same type, found {} and {}",
                name,
                &left_ty,
                &right_ty,
                span
            );
        }

        if !self.unify(ty, &left_ty) {
            add_formatted_type_error!(
                self,
                "Expected {} expression to have type {}, but found {}",
                name,
                ty,
                &left_ty,
                span
            );
        }
    }

    fn check_unary_math_expr(&mut self, node: &IrExpr, ty: &InferType, span: &Span, name: &str) {
        self.check_expr(node);

        let node_ty = self.expr_var(node);

        if !self.unify(ty, &node_ty) {
            add_formatted_type_error!(
                self,
                "Expected {} expression to have type {}, but found {}",
                name,
                ty,
                &node_ty,
                span
            );
        }
    }

    fn check_logical_not(&mut self, node: &IrExpr, ty: &InferType, span: &Span) {
        self.check_expr(node);

        let node_ty = self.expr_var(node);

        if !self.unify(&node_ty, &InferType::Bool) {
            add_type_error!(
                self,
                "Expected argument to logical not to have type {}, but found {}",
                &InferType::Bool,
                &node_ty,
                span
            );
        }

        if !self.unify(ty, &InferType::Bool) {
            add_type_error!(
                self,
                "Expected logical not expression to have type {}, but found {}",
                &InferType::Bool,
                ty,
                span
            );
        }
    }

    fn check_binary_logical_expr(
        &mut self,
        left: &IrExpr,
        right: &IrExpr,
        ty: &InferType,
        span: &Span,
        name: &str,
    ) {
        self.check_expr(left);
        self.check_expr(right);

        let left_ty = self.expr_var(left);
        let right_ty = self.expr_var(right);

        if !self.unify(&left_ty, &InferType::Bool) || !self.unify(&right_ty, &InferType::Bool) {
            add_formatted_type_error!(
                self,
                "Expected both arguments to {} to be bools, but found {} and {}",
                name,
                &left_ty,
                &right_ty,
                span
            );
        }

        if !self.unify(ty, &InferType::Bool) {
            add_formatted_type_error!(
                self,
                "Expected {} expression to have type {}, but found {}",
                name,
                &InferType::Bool,
                ty,
                span
            );
        }
    }

    fn check_comparison(&mut self, left: &IrExpr, right: &IrExpr, ty: &InferType, span: &Span) {
        self.check_expr(left);
        self.check_expr(right);

        let left_ty = self.expr_var(left);
        let right_ty = self.expr_var(right);

        if !self.unify(&left_ty, &right_ty) {
            add_type_error!(
                self,
                "Expected both arguments to comparison to have same type, found {} and {}",
                &left_ty,
                &right_ty,
                span
            );
        }

        if !self.unify(ty, &InferType::Bool) {
            add_type_error!(
                self,
                "Expected comparison expression to have type {}, but found {}",
                &InferType::Bool,
                ty,
                span
            );
        }
    }

    fn check_block(&mut self, nodes: &[IrStmt], ty: &InferType, span: &Span) {
        for node in nodes {
            self.check_stmt(node);
        }

        let eval_ty = match nodes.last() {
            Some(node) => self.stmt_var(node),
            None => InferType::Unit,
        };

        if !self.unify(ty, &eval_ty) {
            add_type_error!(
                self,
                "Expected block to have type {}, but found {}",
                ty,
                &eval_ty,
                span
            );
        }
    }

    fn check_if_expr(
        &mut self,
        cond: &IrExpr,
        conseq: &IrExpr,
        altern: &IrExpr,
        ty: &InferType,
        span: &Span,
    ) {
        self.check_expr(cond);
        self.check_expr(conseq);
        self.check_expr(altern);

        let cond_ty = self.expr_var(cond);
        let conseq_ty = self.expr_var(conseq);
        let altern_ty = self.expr_var(altern);

        if !self.unify(&cond_ty, &InferType::Bool) {
            add_type_error!(
                self,
                "Expected condition of if expression to have type {}, but found {}",
                &InferType::Bool,
                &cond_ty,
                span
            );
        }

        if !self.unify(&conseq_ty, &altern_ty) {
            add_type_error!(
                self,
                "Expected both branches of if expression to have same type, found {} and {}",
                &conseq_ty,
                &altern_ty,
                span
            );
        }

        if !self.unify(&conseq_ty, ty) {
            add_type_error!(
                self,
                "Expected if expression to have type {}, but found {}",
                ty,
                &conseq_ty,
                span
            );
        }
    }

    fn check_application(&mut self, func: &IrExpr, args: &[IrExpr], ty: &InferType, span: &Span) {
        self.check_expr(func);
        for arg in args {
            self.check_expr(arg);
        }

        let func_ty = self.expr_var(func);
        let arg_tys = args.iter().map(|arg| self.expr_var(arg)).collect();

        let expected_func_ty = InferType::Function(arg_tys, Box::new(ty.clone()));

        if !self.unify(&expected_func_ty, &func_ty) {
            add_type_error!(
                self,
                "Function inferred to have type {}, but used as if it had type {}",
                &func_ty,
                &expected_func_ty,
                span
            );
        }
    }

    fn check_assignment(&mut self, var: &IdentifierID, expr: &IrExpr, ty: &InferType, span: &Span) {
        self.check_expr(expr);

        let expr_ty = self.expr_var(expr);
        let var_ty = self.ctx.infer_ctx.ident_to_type[var].clone();
        let name = self.ctx.symbol_table.get_ident(*var).name.clone();

        if !self.unify(&var_ty, &expr_ty) {
            add_formatted_type_error!(
                self,
                "{} inferred to have type {}, but assigned {}",
                name,
                &var_ty,
                &expr_ty,
                span
            );
        }

        if !self.unify(ty, &expr_ty) {
            add_type_error!(
                self,
                "Expected assignment expression to have type {}, but found {}",
                ty,
                &expr_ty,
                span
            );
        }
    }

    fn check_stmt_expr(&mut self, expr: &IrExpr, ty: &InferType, span: &Span) {
        self.check_expr(expr);

        let expr_ty = self.expr_var(expr);

        if !self.unify(ty, &expr_ty) {
            add_type_error!(
                self,
                "Expression expected to have type {}, but found {}",
                ty,
                &expr_ty,
                span
            );
        }
    }

    fn check_if_stmt(&mut self, cond: &IrExpr, conseq: &IrExpr, ty: &InferType, span: &Span) {
        self.check_expr(cond);
        self.check_expr(conseq);

        let cond_ty = self.expr_var(cond);

        if !self.unify(&cond_ty, &InferType::Bool) {
            add_type_error!(
                self,
                "Expected condition of if statement to have type {}, but found {}",
                &InferType::Bool,
                &cond_ty,
                span
            );
        }

        if !self.unify(ty, &InferType::Unit) {
            add_type_error!(
                self,
                "Expected if statement to have type {}, but found {}",
                &InferType::Unit,
                ty,
                span
            );
        }
    }

    fn check_variable_definition(
        &mut self,
        lvalue: &IrPat,
        rvalue: &IrExpr,
        ty: &InferType,
        span: &Span,
    ) {
        self.check_pat(lvalue);
        self.check_expr(rvalue);

        let lvalue_ty = self.pat_var(lvalue);
        let rvalue_ty = self.expr_var(rvalue);

        if !self.unify(&lvalue_ty, &rvalue_ty) {
            add_type_error!(
                self,
                "Pattern has type {}, but assigned {}",
                &lvalue_ty,
                &rvalue_ty,
                span
            );
        }

        if !self.unify(ty, &InferType::Unit) {
            add_type_error!(
                self,
                "Expected definition to have type {}, but found {}",
                &InferType::Unit,
                ty,
                span
            );
        }
    }

    fn check_function_definition(
        &mut self,
        var: &IdentifierID,
        body: &IrExpr,
        ty: &InferType,
        span: &Span,
    ) {
        self.check_expr(body);

        let body_ty = self.expr_var(body);

        let name = self.ctx.symbol_table.get_ident(*var).name.clone();
        let func_ty = self.ctx.infer_ctx.ident_to_type[var].clone();
        let ret_ty = if let InferType::Function(_, ret) = func_ty {
            ret
        } else {
            return;
        };

        if !self.unify(&ret_ty, &body_ty) {
            add_formatted_type_error!(
                self,
                "Function {} has return type {}, but found {}",
                name,
                &ret_ty,
                &body_ty,
                span
            );
        }

        if !self.unify(ty, &InferType::Unit) {
            add_type_error!(
                self,
                "Expected definition to have type {}, but found {}",
                &InferType::Unit,
                ty,
                span
            );
        }
    }

    fn check_variable_pat(&mut self, var: &IdentifierID, ty: &InferType, span: &Span) {
        let var_ty = self.ctx.infer_ctx.ident_to_type[var].clone();
        let name = self.ctx.symbol_table.get_ident(*var).name.clone();

        if !self.unify(ty, &var_ty) {
            add_formatted_type_error!(
                self,
                "Expected {} to have type {}, but found {}",
                name,
                ty,
                &var_ty,
                span
            );
        }
    }

    fn refresh(&mut self, ty: &InferType) -> InferType {
        let refreshed_vars = ty
            .get_vars()
            .into_iter()
            .map(|var| (var, self.ctx.infer_ctx.new_infer_var_id()))
            .collect();
        self.refresh_with_vars(ty, &refreshed_vars)
    }

    fn refresh_with_vars(
        &self,
        ty: &InferType,
        vars: &HashMap<InferTypeVariable, InferVarID>,
    ) -> InferType {
        match ty {
            InferType::InferVariable(var) => {
                InferType::InferVariable(vars[&InferTypeVariable::Infer(*var)])
            }
            InferType::ParamVariable(var) => {
                InferType::InferVariable(vars[&InferTypeVariable::Param(*var)])
            }
            InferType::Unit
            | InferType::Bool
            | InferType::Int
            | InferType::Float
            | InferType::String => ty.clone(),
            InferType::Function(args, ret) => InferType::Function(
                args.iter()
                    .map(|arg| self.refresh_with_vars(arg, vars))
                    .collect(),
                Box::new(self.refresh_with_vars(ret, vars)),
            ),
        }
    }
}

pub fn type_check_repl_line(ir: &IrStmt, ctx: &mut Context) {
    let mut type_checker = TypeChecker::new(ctx);
    type_checker.check_stmt(ir);
}

pub fn type_check_files(irs: &[IrStmt], ctx: &mut Context) {
    let mut type_checker = TypeChecker::new(ctx);
    for ir in irs {
        type_checker.check_stmt(ir);
    }
}
