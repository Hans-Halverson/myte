use common::error::{mkerr, MyteErrorType, MyteResult};
use common::ident::{SymbolTable, VariableID};
use common::span::Span;
use interpreter::env::Environment;
use interpreter::value::Value;
use ir::ir::{IrExpr, IrExprType, IrPat, IrStmt, IrStmtType};

fn evaluate_expr(ir: &IrExpr, env: &mut Environment) -> MyteResult<Value> {
    let IrExpr { span, node } = ir;
    match *node {
        IrExprType::UnitLiteral => Ok(Value::Unit),
        IrExprType::BoolLiteral(bool) => Ok(Value::Bool { bool }),
        IrExprType::StringLiteral(ref string) => Ok(Value::String {
            string: string.clone(),
        }),
        IrExprType::IntLiteral(num) => Ok(Value::Int { num }),
        IrExprType::FloatLiteral(num) => Ok(Value::Float { num }),
        IrExprType::Variable(var) => Ok(env.lookup(var)),
        IrExprType::Add {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::Subtract {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::Multiply {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::Divide {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::Exponentiate {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::Remainder {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::ParenthesizedGroup(ref node) => evaluate_expr(node, env),
        IrExprType::UnaryPlus(ref node) => match evaluate_expr(node, env)? {
            Value::Int { num } => Ok(Value::Int { num }),
            Value::Float { num } => Ok(Value::Float { num }),
            _ => mk_eval_err("PRE-TYPES: Unary plus expects number".to_string(), &span),
        },
        IrExprType::UnaryMinus(ref node) => match evaluate_expr(node, env)? {
            Value::Int { num } => Ok(Value::Int { num: -num }),
            Value::Float { num } => Ok(Value::Float { num: -num }),
            _ => mk_eval_err("PRE-TYPES: Unary minus expects number".to_string(), &span),
        },
        IrExprType::LogicalNot(ref node) => match evaluate_expr(node, env)? {
            Value::Bool { bool } => Ok(Value::Bool { bool: !bool }),
            _ => mk_eval_err("PRE-TYPES: Logical not expects bool".to_string(), &span),
        },
        IrExprType::LogicalAnd {
            ref left,
            ref right,
        } => match evaluate_expr(left, env)? {
            Value::Bool { bool: false } => Ok(Value::Bool { bool: false }),
            Value::Bool { bool: true } => match evaluate_expr(right, env)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
        },
        IrExprType::LogicalOr {
            ref left,
            ref right,
        } => match evaluate_expr(left, env)? {
            Value::Bool { bool: true } => Ok(Value::Bool { bool: true }),
            Value::Bool { bool: false } => match evaluate_expr(right, env)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
        },
        IrExprType::Equals {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::NotEqual {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::LessThan {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::LessThanOrEqual {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::GreaterThan {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::GreaterThanOrEqual {
            ref left,
            ref right,
        } => match (evaluate_expr(left, env)?, evaluate_expr(right, env)?) {
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
        IrExprType::Block(ref nodes) => {
            env.enter_scope();

            let mut value = Value::Unit;
            for node in nodes {
                value = evaluate_stmt(&node, env)?;
            }

            env.exit_scope();

            Ok(value)
        }
        IrExprType::If {
            ref cond,
            ref conseq,
            ref altern,
        } => match evaluate_expr(cond, env)? {
            Value::Bool { bool: true } => evaluate_expr(conseq, env),
            Value::Bool { bool: false } => evaluate_expr(altern, env),
            _ => mk_eval_err(
                "PRE-TYPES: Condition of if expression must be a bool".to_string(),
                &cond.span,
            ),
        },
        IrExprType::Application { ref func, ref args } => match evaluate_expr(func, env)? {
            Value::Closure {
                ref params,
                ref body,
            } => {
                if params.len() != args.len() {
                    return mk_eval_err(
                        "PRE-TYPES: Incorrect number of arguments in application".to_string(),
                        &span,
                    );
                }

                env.enter_scope();

                for (param, arg) in params.iter().zip(args) {
                    let arg_value = evaluate_expr(arg, env)?;
                    env.extend(*param, &arg_value);
                }

                let return_value = evaluate_expr(body, env)?;

                env.exit_scope();

                Ok(return_value)
            }
            _ => mk_eval_err(
                "PRE-TYPES: Left side of application must be a closure".to_string(),
                &span,
            ),
        },
        IrExprType::Assignment { var, ref expr } => {
            let value = evaluate_expr(expr, env)?;
            env.reassign(var, &value);
            Ok(value)
        }
    }
}

fn evaluate_stmt(ir: &IrStmt, env: &mut Environment) -> MyteResult<Value> {
    match (*ir).node {
        IrStmtType::Expr(ref expr) => evaluate_expr(expr, env),
        IrStmtType::VariableDefinition {
            ref lvalue,
            ref rvalue,
        } => {
            let val = evaluate_expr(rvalue, env)?;
            bind_variables(lvalue, &val, env);
            Ok(Value::Unit)
        }
        IrStmtType::FunctionDefinition {
            name,
            ref params,
            ref body,
        } => {
            env.extend(
                name,
                &Value::Closure {
                    params: params.clone(),
                    body: body.clone(),
                },
            );
            Ok(Value::Unit)
        }
        IrStmtType::If {
            ref cond,
            ref conseq,
        } => match evaluate_expr(cond, env)? {
            Value::Bool { bool: true } => {
                evaluate_expr(conseq, env)?;
                Ok(Value::Unit)
            }
            Value::Bool { bool: false } => Ok(Value::Unit),
            _ => mk_eval_err(
                "PRE-TYPES: Condition of if statement must be a bool".to_string(),
                &cond.span,
            ),
        },
    }
}

fn bind_variables(pat: &IrPat, val: &Value, env: &mut Environment) {
    match *pat {
        IrPat::Variable { var, .. } => env.extend(var, val),
    }
}

pub fn apply_main(
    main_id: VariableID,
    env: &mut Environment,
    symbol_table: &SymbolTable,
) -> MyteResult<Value> {
    match env.lookup(main_id) {
        Value::Closure {
            ref body,
            ref params,
        } => {
            if params.len() != 0 {
                return mk_eval_err(
                    "Main takes no arguments".to_string(),
                    &symbol_table.get_ident(main_id).span,
                );
            }

            env.enter_scope();
            let return_value = evaluate_expr(body, env)?;
            env.exit_scope();

            Ok(return_value)
        }
        _ => mk_eval_err(
            "Main must be a function".to_string(),
            &symbol_table.get_ident(main_id).span,
        ),
    }
}

pub fn evaluate_repl_line(ir: IrStmt, env: &mut Environment) -> MyteResult<Value> {
    evaluate_stmt(&ir, env)
}

pub fn evaluate_files(irs: Vec<IrStmt>, env: &mut Environment) -> MyteResult<()> {
    for ir in irs {
        evaluate_stmt(&ir, env)?;
    }

    Ok(())
}

fn mk_eval_err<T>(error: String, span: &Span) -> MyteResult<T> {
    mkerr(error, span, MyteErrorType::Evaluate)
}
