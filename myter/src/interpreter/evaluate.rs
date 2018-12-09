use common::context::Context;
use common::error::{self, mkerr, MyteErrorType, MyteResult};
use common::span::Span;
use interpreter::env::Environment;
use interpreter::value::Value;
use ir::nodes::{IrExpr, IrExprType, IrPat, IrPatType, IrStmt, IrStmtType};

struct Evaluator<'ctx> {
    ctx: &'ctx mut Context,
}

impl<'ctx> Evaluator<'ctx> {
    fn new(ctx: &mut Context) -> Evaluator {
        Evaluator { ctx }
    }

    fn evaluate_expr(&mut self, ir: &IrExpr, env: &mut Environment) -> MyteResult<Value> {
        let IrExpr { span, node, .. } = ir;
        match *node {
            IrExprType::UnitLiteral => Ok(Value::Unit),
            IrExprType::BoolLiteral(bool) => Ok(Value::Bool(bool)),
            IrExprType::StringLiteral(ref string) => Ok(Value::String(string.clone())),
            IrExprType::IntLiteral(num) => Ok(Value::Int(num)),
            IrExprType::FloatLiteral(num) => Ok(Value::Float(num)),
            IrExprType::Variable(var) => Ok(env.lookup(var)),
            IrExprType::Add {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left + right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Add expects numbers of same type".to_string(),
                    &span,
                ),
            },
            IrExprType::Subtract {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left - right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left - right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Subtract expects numbers of same type".to_string(),
                    &span,
                ),
            },
            IrExprType::Multiply {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left * right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Multiply expects numbers of same type".to_string(),
                    &span,
                ),
            },
            IrExprType::Divide {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left / right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left / right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Divide expects numbers of same type".to_string(),
                    &span,
                ),
            },
            IrExprType::Exponentiate {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Int(left), Value::Int(right)) => {
                    Ok(Value::Int(i64::pow(left, right as u32)))
                }
                (Value::Float(left), Value::Float(right)) => {
                    Ok(Value::Float(f64::powf(left, right)))
                }
                _ => mk_eval_err(
                    "PRE-TYPES: Exponentiate expects numbers of same type".to_string(),
                    &span,
                ),
            },
            IrExprType::Remainder {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left % right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Float(left % right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Remainder expects numbers of same type".to_string(),
                    &span,
                ),
            },
            IrExprType::UnaryPlus(ref node) => match self.evaluate_expr(node, env)? {
                Value::Int(num) => Ok(Value::Int(num)),
                Value::Float(num) => Ok(Value::Float(num)),
                _ => mk_eval_err("PRE-TYPES: Unary plus expects number".to_string(), &span),
            },
            IrExprType::UnaryMinus(ref node) => match self.evaluate_expr(node, env)? {
                Value::Int(num) => Ok(Value::Int(-num)),
                Value::Float(num) => Ok(Value::Float(-num)),
                _ => mk_eval_err("PRE-TYPES: Unary minus expects number".to_string(), &span),
            },
            IrExprType::LogicalNot(ref node) => match self.evaluate_expr(node, env)? {
                Value::Bool(bool) => Ok(Value::Bool(!bool)),
                _ => mk_eval_err("PRE-TYPES: Logical not expects bool".to_string(), &span),
            },
            IrExprType::LogicalAnd {
                ref left,
                ref right,
            } => match self.evaluate_expr(left, env)? {
                Value::Bool(false) => Ok(Value::Bool(false)),
                Value::Bool(true) => match self.evaluate_expr(right, env)? {
                    Value::Bool(bool) => Ok(Value::Bool(bool)),
                    _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
                },
                _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
            },
            IrExprType::LogicalOr {
                ref left,
                ref right,
            } => match self.evaluate_expr(left, env)? {
                Value::Bool(true) => Ok(Value::Bool(true)),
                Value::Bool(false) => match self.evaluate_expr(right, env)? {
                    Value::Bool(bool) => Ok(Value::Bool(bool)),
                    _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
                },
                _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
            },
            IrExprType::Equals {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Unit, Value::Unit) => Ok(Value::Bool(true)),
                (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left == right)),
                (Value::String(left), Value::String(right)) => Ok(Value::Bool(left == right)),
                (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left == right)),
                (Value::Float(left), Value::Float(right)) => {
                    Ok(Value::Bool((left - right).abs() < std::f64::EPSILON))
                }
                (Value::Closure { .. }, Value::Closure { .. }) => Ok(Value::Bool(false)),
                _ => mk_eval_err(
                    "PRE-TYPES: Comparison expects values of same type".to_string(),
                    span,
                ),
            },
            IrExprType::NotEqual {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::Unit, Value::Unit) => Ok(Value::Bool(false)),
                (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left != right)),
                (Value::String(left), Value::String(right)) => Ok(Value::Bool(left != right)),
                (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left != right)),
                (Value::Float(left), Value::Float(right)) => {
                    Ok(Value::Bool((left - right).abs() >= std::f64::EPSILON))
                }
                (Value::Closure { .. }, Value::Closure { .. }) => Ok(Value::Bool(true)),
                _ => mk_eval_err(
                    "PRE-TYPES: Comparison expects values of same type".to_string(),
                    span,
                ),
            },
            IrExprType::LessThan {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::String(left), Value::String(right)) => Ok(Value::Bool(left < right)),
                (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left < right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Bool(left < right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                    span,
                ),
            },
            IrExprType::LessThanOrEqual {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::String(left), Value::String(right)) => Ok(Value::Bool(left <= right)),
                (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left <= right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Bool(left <= right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                    span,
                ),
            },
            IrExprType::GreaterThan {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::String(left), Value::String(right)) => Ok(Value::Bool(left > right)),
                (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left > right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Bool(left > right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                    span,
                ),
            },
            IrExprType::GreaterThanOrEqual {
                ref left,
                ref right,
            } => match (
                self.evaluate_expr(left, env)?,
                self.evaluate_expr(right, env)?,
            ) {
                (Value::String(left), Value::String(right)) => Ok(Value::Bool(left >= right)),
                (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left >= right)),
                (Value::Float(left), Value::Float(right)) => Ok(Value::Bool(left >= right)),
                _ => mk_eval_err(
                    "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                    span,
                ),
            },
            IrExprType::Block(ref nodes) => {
                env.enter_scope();

                let mut value = Value::Unit;
                for node in nodes {
                    value = self.evaluate_stmt(&node, env)?;
                }

                env.exit_scope();

                Ok(value)
            }
            IrExprType::If {
                ref cond,
                ref conseq,
                ref altern,
            } => match self.evaluate_expr(cond, env)? {
                Value::Bool(true) => self.evaluate_expr(conseq, env),
                Value::Bool(false) => self.evaluate_expr(altern, env),
                _ => mk_eval_err(
                    "PRE-TYPES: Condition of if expression must be a bool".to_string(),
                    &cond.span,
                ),
            },
            IrExprType::Application { ref func, ref args } => {
                match self.evaluate_expr(func, env)? {
                    Value::Closure {
                        ref params,
                        ref body,
                        ..
                    } => {
                        if params.len() != args.len() {
                            return mk_eval_err(
                                "PRE-TYPES: Incorrect number of arguments in application"
                                    .to_string(),
                                &span,
                            );
                        }

                        env.enter_scope();

                        for (param, arg) in params.iter().zip(args) {
                            let arg_value = self.evaluate_expr(arg, env)?;
                            env.extend(*param, &arg_value);
                        }

                        let return_value = self.evaluate_expr(body, env)?;

                        env.exit_scope();

                        Ok(return_value)
                    }
                    _ => mk_eval_err(
                        "PRE-TYPES: Left side of application must be a closure".to_string(),
                        &span,
                    ),
                }
            }
            IrExprType::Assignment { var, ref expr } => {
                let value = self.evaluate_expr(expr, env)?;
                env.reassign(var, &value);
                Ok(value)
            }
        }
    }

    fn evaluate_stmt(&mut self, ir: &IrStmt, env: &mut Environment) -> MyteResult<Value> {
        match (*ir).node {
            IrStmtType::Expr(ref expr) => self.evaluate_expr(expr, env),
            IrStmtType::VariableDefinition {
                ref lvalue,
                ref rvalue,
                ..
            } => {
                let val = self.evaluate_expr(rvalue, env)?;
                bind_variables(lvalue, &val, env);
                Ok(Value::Unit)
            }
            IrStmtType::FunctionDefinition {
                name,
                ref params,
                ref body,
            } => {
                let ir_ty = &self.ctx.infer_ctx.ident_to_type[&name];
                let rep_ty = self.ctx.infer_ctx.graph.rep(ir_ty);
                env.extend(
                    name,
                    &Value::Closure {
                        params: params.clone(),
                        body: body.clone(),
                        ty: rep_ty,
                    },
                );
                Ok(Value::Unit)
            }
            IrStmtType::If {
                ref cond,
                ref conseq,
            } => match self.evaluate_expr(cond, env)? {
                Value::Bool(true) => {
                    self.evaluate_expr(conseq, env)?;
                    Ok(Value::Unit)
                }
                Value::Bool(false) => Ok(Value::Unit),
                _ => mk_eval_err(
                    "PRE-TYPES: Condition of if statement must be a bool".to_string(),
                    &cond.span,
                ),
            },
        }
    }

    fn apply_main(&mut self, env: &mut Environment) -> MyteResult<Option<Value>> {
        let main_id = match self.ctx.symbol_table.get_main_id() {
            Some(id) => id,
            None => {
                error::print_err_string("No main function defined");
                return Ok(None);
            }
        };

        match env.lookup(main_id) {
            Value::Closure {
                ref body,
                ref params,
                ..
            } => {
                if !params.is_empty() {
                    return mk_eval_err(
                        "Main takes no arguments".to_string(),
                        &self.ctx.symbol_table.get_ident(main_id).span,
                    );
                }

                env.enter_scope();
                let return_value = self.evaluate_expr(body, env)?;
                env.exit_scope();

                Ok(Some(return_value))
            }
            _ => mk_eval_err(
                "Main must be a function".to_string(),
                &self.ctx.symbol_table.get_ident(main_id).span,
            ),
        }
    }
}

fn bind_variables(pat: &IrPat, val: &Value, env: &mut Environment) {
    match pat.pat {
        IrPatType::Variable(var) => env.extend(var, val),
    }
}

pub fn evaluate_repl_line(
    ir: &IrStmt,
    env: &mut Environment,
    ctx: &mut Context,
) -> MyteResult<Value> {
    let mut evaluator = Evaluator::new(ctx);
    evaluator.evaluate_stmt(&ir, env)
}

pub fn evaluate_files(
    irs: Vec<IrStmt>,
    env: &mut Environment,
    ctx: &mut Context,
) -> MyteResult<Option<Value>> {
    let mut evaluator = Evaluator::new(ctx);
    for ir in irs {
        evaluator.evaluate_stmt(&ir, env)?;
    }

    Ok(evaluator.apply_main(env)?)
}

fn mk_eval_err<T>(error: String, span: &Span) -> MyteResult<T> {
    mkerr(error, span, MyteErrorType::Evaluate)
}
