use common::error::{mkerr, MyteErrorType, MyteResult};
use common::span::Span;
use interpreter::env::Environment;
use interpreter::value::Value;
use ir::ir::{IrExpr, IrPat, IrStmt};

fn evaluate_expr(ir: IrExpr, env: &mut Environment) -> MyteResult<Value> {
    match ir {
        IrExpr::UnitLiteral { .. } => Ok(Value::Unit),
        IrExpr::BoolLiteral { bool, .. } => Ok(Value::Bool { bool }),
        IrExpr::StringLiteral { string, .. } => Ok(Value::String { string }),
        IrExpr::IntLiteral { num, .. } => Ok(Value::Int { num }),
        IrExpr::FloatLiteral { num, .. } => Ok(Value::Float { num }),
        IrExpr::Variable { var, .. } => Ok(env.lookup(var)),
        IrExpr::Add { left, right, span } => {
            match (evaluate_expr(*left, env)?, evaluate_expr(*right, env)?) {
                (Value::Int { num: left }, Value::Int { num: right }) => {
                    Ok(Value::Int { num: left + right })
                }
                (Value::Float { num: left }, Value::Float { num: right }) => {
                    Ok(Value::Float { num: left + right })
                }
                _ => mk_eval_err(
                    "PRE-TYPES: Add expects numbers of same type".to_string(),
                    &span,
                ),
            }
        }
        IrExpr::Subtract { left, right, span } => {
            match (evaluate_expr(*left, env)?, evaluate_expr(*right, env)?) {
                (Value::Int { num: left }, Value::Int { num: right }) => {
                    Ok(Value::Int { num: left - right })
                }
                (Value::Float { num: left }, Value::Float { num: right }) => {
                    Ok(Value::Float { num: left - right })
                }
                _ => mk_eval_err(
                    "PRE-TYPES: Subtract expects numbers of same type".to_string(),
                    &span,
                ),
            }
        }
        IrExpr::Multiply { left, right, span } => {
            match (evaluate_expr(*left, env)?, evaluate_expr(*right, env)?) {
                (Value::Int { num: left }, Value::Int { num: right }) => {
                    Ok(Value::Int { num: left * right })
                }
                (Value::Float { num: left }, Value::Float { num: right }) => {
                    Ok(Value::Float { num: left * right })
                }
                _ => mk_eval_err(
                    "PRE-TYPES: Multiply expects numbers of same type".to_string(),
                    &span,
                ),
            }
        }
        IrExpr::Divide { left, right, span } => {
            match (evaluate_expr(*left, env)?, evaluate_expr(*right, env)?) {
                (Value::Int { num: left }, Value::Int { num: right }) => {
                    Ok(Value::Int { num: left / right })
                }
                (Value::Float { num: left }, Value::Float { num: right }) => {
                    Ok(Value::Float { num: left / right })
                }
                _ => mk_eval_err(
                    "PRE-TYPES: Divide expects numbers of same type".to_string(),
                    &span,
                ),
            }
        }
        IrExpr::Exponentiate { left, right, span } => {
            match (evaluate_expr(*left, env)?, evaluate_expr(*right, env)?) {
                (Value::Int { num: left }, Value::Int { num: right }) => Ok(Value::Int {
                    num: i64::pow(left, right as u32),
                }),
                (Value::Float { num: left }, Value::Float { num: right }) => Ok(Value::Float {
                    num: f64::powf(left, right),
                }),
                _ => mk_eval_err(
                    "PRE-TYPES: Exponentiate expects numbers of same type".to_string(),
                    &span,
                ),
            }
        }
        IrExpr::Remainder { left, right, span } => {
            match (evaluate_expr(*left, env)?, evaluate_expr(*right, env)?) {
                (Value::Int { num: left }, Value::Int { num: right }) => {
                    Ok(Value::Int { num: left % right })
                }
                (Value::Float { num: left }, Value::Float { num: right }) => {
                    Ok(Value::Float { num: left % right })
                }
                _ => mk_eval_err(
                    "PRE-TYPES: Remainder expects numbers of same type".to_string(),
                    &span,
                ),
            }
        }
        IrExpr::ParenthesizedGroup { node, .. } => evaluate_expr(*node, env),
        IrExpr::UnaryPlus { node, span } => match evaluate_expr(*node, env)? {
            Value::Int { num } => Ok(Value::Int { num }),
            Value::Float { num } => Ok(Value::Float { num }),
            _ => mk_eval_err("PRE-TYPES: Unary plus expects number".to_string(), &span),
        },
        IrExpr::UnaryMinus { node, span } => match evaluate_expr(*node, env)? {
            Value::Int { num } => Ok(Value::Int { num: -num }),
            Value::Float { num } => Ok(Value::Float { num: -num }),
            _ => mk_eval_err("PRE-TYPES: Unary minus expects number".to_string(), &span),
        },
        IrExpr::LogicalNot { node, span } => match evaluate_expr(*node, env)? {
            Value::Bool { bool } => Ok(Value::Bool { bool: !bool }),
            _ => mk_eval_err("PRE-TYPES: Logical not expects bool".to_string(), &span),
        },
        IrExpr::LogicalAnd { left, right, span } => match evaluate_expr(*left, env)? {
            Value::Bool { bool: false } => Ok(Value::Bool { bool: false }),
            Value::Bool { bool: true } => match evaluate_expr(*right, env)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
        },
        IrExpr::LogicalOr { left, right, span } => match evaluate_expr(*left, env)? {
            Value::Bool { bool: true } => Ok(Value::Bool { bool: true }),
            Value::Bool { bool: false } => match evaluate_expr(*right, env)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
        },
        IrExpr::Block { nodes, .. } => {
            let mut value = Value::Unit;
            for node in nodes {
                value = evaluate_stmt(node, env)?;
            }

            Ok(value)
        }
    }
}

fn evaluate_stmt(ir: IrStmt, env: &mut Environment) -> MyteResult<Value> {
    match ir {
        IrStmt::Expr { expr, .. } => evaluate_expr(*expr, env),
        IrStmt::VariableDefinition { lvalue, rvalue, .. } => {
            let val = evaluate_expr(*rvalue, env)?;
            bind_variables(*lvalue, val, env);
            Ok(Value::Unit)
        }
    }
}

fn bind_variables(pat: IrPat, expr: Value, env: &mut Environment) {
    match pat {
        IrPat::Variable { var, .. } => env.extend(var, expr),
    }
}

pub fn evaluate(ir: IrStmt, env: &mut Environment) -> MyteResult<Value> {
    evaluate_stmt(ir, env)
}

fn mk_eval_err<T>(error: String, span: &Span) -> MyteResult<T> {
    mkerr(error, span, MyteErrorType::Evaluate)
}
