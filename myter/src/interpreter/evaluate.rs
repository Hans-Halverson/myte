use common::error::{mkerr, MyteErrorType, MyteResult};
use common::span::Span;
use interpreter::env::Environment;
use interpreter::value::Value;
use ir::ir::{IrExpr, IrPat, IrStmt};

fn evaluate_expr(ir: &IrExpr, env: &mut Environment) -> MyteResult<Value> {
    match *ir {
        IrExpr::UnitLiteral { .. } => Ok(Value::Unit),
        IrExpr::BoolLiteral { bool, .. } => Ok(Value::Bool { bool }),
        IrExpr::StringLiteral { ref string, .. } => Ok(Value::String {
            string: string.clone(),
        }),
        IrExpr::IntLiteral { num, .. } => Ok(Value::Int { num }),
        IrExpr::FloatLiteral { num, .. } => Ok(Value::Float { num }),
        IrExpr::Variable { var, .. } => Ok(env.lookup(var)),
        IrExpr::Add {
            ref left,
            ref right,
            span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
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
        },
        IrExpr::Subtract {
            ref left,
            ref right,
            span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
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
        },
        IrExpr::Multiply {
            ref left,
            ref right,
            span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
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
        },
        IrExpr::Divide {
            ref left,
            ref right,
            span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
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
        },
        IrExpr::Exponentiate {
            ref left,
            ref right,
            span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
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
        },
        IrExpr::Remainder {
            ref left,
            ref right,
            span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
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
        },
        IrExpr::ParenthesizedGroup { ref node, .. } => evaluate_expr(node.as_ref(), env),
        IrExpr::UnaryPlus { ref node, span } => match evaluate_expr(node.as_ref(), env)? {
            Value::Int { num } => Ok(Value::Int { num }),
            Value::Float { num } => Ok(Value::Float { num }),
            _ => mk_eval_err("PRE-TYPES: Unary plus expects number".to_string(), &span),
        },
        IrExpr::UnaryMinus { ref node, span } => match evaluate_expr(node.as_ref(), env)? {
            Value::Int { num } => Ok(Value::Int { num: -num }),
            Value::Float { num } => Ok(Value::Float { num: -num }),
            _ => mk_eval_err("PRE-TYPES: Unary minus expects number".to_string(), &span),
        },
        IrExpr::LogicalNot { ref node, span } => match evaluate_expr(node.as_ref(), env)? {
            Value::Bool { bool } => Ok(Value::Bool { bool: !bool }),
            _ => mk_eval_err("PRE-TYPES: Logical not expects bool".to_string(), &span),
        },
        IrExpr::LogicalAnd {
            ref left,
            ref right,
            span,
        } => match evaluate_expr(left.as_ref(), env)? {
            Value::Bool { bool: false } => Ok(Value::Bool { bool: false }),
            Value::Bool { bool: true } => match evaluate_expr(right.as_ref(), env)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
        },
        IrExpr::LogicalOr {
            ref left,
            ref right,
            span,
        } => match evaluate_expr(left.as_ref(), env)? {
            Value::Bool { bool: true } => Ok(Value::Bool { bool: true }),
            Value::Bool { bool: false } => match evaluate_expr(right.as_ref(), env)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
        },
        IrExpr::Equals {
            ref left,
            ref right,
            ref span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
            (Value::Unit, Value::Unit) => Ok(Value::Bool { bool: true }),
            (Value::Bool { bool: left }, Value::Bool { bool: right }) => Ok(Value::Bool {
                bool: left == right,
            }),
            (Value::String { string: left }, Value::String { string: right }) => Ok(Value::Bool {
                bool: left == right,
            }),
            (Value::Int { num: left }, Value::Int { num: right }) => Ok(Value::Bool {
                bool: left == right,
            }),
            (Value::Float { num: left }, Value::Float { num: right }) => Ok(Value::Bool {
                bool: left == right,
            }),
            (Value::Closure { .. }, Value::Closure { .. }) => Ok(Value::Bool { bool: false }),
            _ => mk_eval_err(
                "PRE-TYPES: Comparison expects values of same type".to_string(),
                span,
            ),
        },
        IrExpr::NotEqual {
            ref left,
            ref right,
            ref span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
            (Value::Unit, Value::Unit) => Ok(Value::Bool { bool: false }),
            (Value::Bool { bool: left }, Value::Bool { bool: right }) => Ok(Value::Bool {
                bool: left != right,
            }),
            (Value::String { string: left }, Value::String { string: right }) => Ok(Value::Bool {
                bool: left != right,
            }),
            (Value::Int { num: left }, Value::Int { num: right }) => Ok(Value::Bool {
                bool: left != right,
            }),
            (Value::Float { num: left }, Value::Float { num: right }) => Ok(Value::Bool {
                bool: left != right,
            }),
            (Value::Closure { .. }, Value::Closure { .. }) => Ok(Value::Bool { bool: true }),
            _ => mk_eval_err(
                "PRE-TYPES: Comparison expects values of same type".to_string(),
                span,
            ),
        },
        IrExpr::LessThan {
            ref left,
            ref right,
            ref span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
            (Value::String { string: left }, Value::String { string: right }) => {
                Ok(Value::Bool { bool: left < right })
            }
            (Value::Int { num: left }, Value::Int { num: right }) => {
                Ok(Value::Bool { bool: left < right })
            }
            (Value::Float { num: left }, Value::Float { num: right }) => {
                Ok(Value::Bool { bool: left < right })
            }
            _ => mk_eval_err(
                "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                span,
            ),
        },
        IrExpr::LessThanOrEqual {
            ref left,
            ref right,
            ref span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
            (Value::String { string: left }, Value::String { string: right }) => Ok(Value::Bool {
                bool: left <= right,
            }),
            (Value::Int { num: left }, Value::Int { num: right }) => Ok(Value::Bool {
                bool: left <= right,
            }),
            (Value::Float { num: left }, Value::Float { num: right }) => Ok(Value::Bool {
                bool: left <= right,
            }),
            _ => mk_eval_err(
                "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                span,
            ),
        },
        IrExpr::GreaterThan {
            ref left,
            ref right,
            ref span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
            (Value::String { string: left }, Value::String { string: right }) => {
                Ok(Value::Bool { bool: left > right })
            }
            (Value::Int { num: left }, Value::Int { num: right }) => {
                Ok(Value::Bool { bool: left > right })
            }
            (Value::Float { num: left }, Value::Float { num: right }) => {
                Ok(Value::Bool { bool: left > right })
            }
            _ => mk_eval_err(
                "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                span,
            ),
        },
        IrExpr::GreaterThanOrEqual {
            ref left,
            ref right,
            ref span,
        } => match (
            evaluate_expr(left.as_ref(), env)?,
            evaluate_expr(right.as_ref(), env)?,
        ) {
            (Value::String { string: left }, Value::String { string: right }) => Ok(Value::Bool {
                bool: left >= right,
            }),
            (Value::Int { num: left }, Value::Int { num: right }) => Ok(Value::Bool {
                bool: left >= right,
            }),
            (Value::Float { num: left }, Value::Float { num: right }) => Ok(Value::Bool {
                bool: left >= right,
            }),
            _ => mk_eval_err(
                "PRE-TYPES: Comparison expects comparable values of same type".to_string(),
                span,
            ),
        },
        IrExpr::Block { ref nodes, .. } => {
            env.enter_scope();

            let mut value = Value::Unit;
            for node in nodes {
                value = evaluate_stmt(&node, env)?;
            }

            env.exit_scope();

            Ok(value)
        }
        IrExpr::If {
            ref cond,
            ref conseq,
            ref altern,
            ..
        } => match evaluate_expr(cond.as_ref(), env)? {
            Value::Bool { bool: true } => evaluate_expr(conseq.as_ref(), env),
            Value::Bool { bool: false } => evaluate_expr(altern.as_ref(), env),
            _ => mk_eval_err(
                "PRE-TYPES: Condition of if expression must be a bool".to_string(),
                &cond.span(),
            ),
        },
        IrExpr::Application {
            ref func,
            ref args,
            span,
        } => match evaluate_expr(func.as_ref(), env)? {
            Value::Closure { params, body } => {
                if params.len() != args.len() {
                    return mk_eval_err(
                        "PRE-TYPES: Incorrect number of arguments in application".to_string(),
                        &span,
                    );
                }

                env.enter_scope();

                for (param, arg) in params.iter().zip(args) {
                    let arg_value = evaluate_expr(arg, env)?;
                    env.extend(*param, arg_value);
                }

                let return_value = evaluate_expr(body.as_ref(), env)?;

                env.exit_scope();

                Ok(return_value)
            }
            _ => mk_eval_err(
                "PRE-TYPES: Left side of application must be a closure".to_string(),
                &span,
            ),
        },
    }
}

fn evaluate_stmt(ir: &IrStmt, env: &mut Environment) -> MyteResult<Value> {
    match *ir {
        IrStmt::Expr { ref expr, .. } => evaluate_expr(expr.as_ref(), env),
        IrStmt::VariableDefinition {
            ref lvalue,
            ref rvalue,
            ..
        } => {
            let val = evaluate_expr(rvalue.as_ref(), env)?;
            bind_variables((*lvalue).as_ref(), val, env);
            Ok(Value::Unit)
        }
        IrStmt::FunctionDefinition {
            name,
            ref params,
            ref body,
            ..
        } => {
            env.extend(
                name,
                Value::Closure {
                    params: params.clone(),
                    body: body.clone(),
                },
            );
            Ok(Value::Unit)
        }
        IrStmt::If {
            ref cond,
            ref conseq,
            ..
        } => match evaluate_expr(cond.as_ref(), env)? {
            Value::Bool { bool: true } => {
                evaluate_expr(conseq.as_ref(), env)?;
                Ok(Value::Unit)
            }
            Value::Bool { bool: false } => Ok(Value::Unit),
            _ => mk_eval_err(
                "PRE-TYPES: Condition of if statement must be a bool".to_string(),
                &cond.span(),
            ),
        },
    }
}

fn bind_variables(pat: &IrPat, val: Value, env: &mut Environment) {
    match *pat {
        IrPat::Variable { var, .. } => env.extend(var, val),
    }
}

pub fn evaluate(ir: IrStmt, env: &mut Environment) -> MyteResult<Value> {
    evaluate_stmt(&ir, env)
}

fn mk_eval_err<T>(error: String, span: &Span) -> MyteResult<T> {
    mkerr(error, span, MyteErrorType::Evaluate)
}
