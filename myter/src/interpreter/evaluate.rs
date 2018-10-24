use common::error::{mkerr, MyteErrorType, MyteResult};
use common::span::Span;
use interpreter::value::Value;
use ir::ir::Ir;

pub fn evaluate(ir: Ir) -> MyteResult<Value> {
    match ir {
        Ir::UnitLiteral { .. } => Ok(Value::Unit),
        Ir::BoolLiteral { bool, .. } => Ok(Value::Bool { bool }),
        Ir::StringLiteral { string, .. } => Ok(Value::String { string }),
        Ir::IntLiteral { num, .. } => Ok(Value::Int { num }),
        Ir::FloatLiteral { num, .. } => Ok(Value::Float { num }),
        Ir::Add { left, right, span } => match (evaluate(*left)?, evaluate(*right)?) {
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
        Ir::Subtract { left, right, span } => match (evaluate(*left)?, evaluate(*right)?) {
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
        Ir::Multiply { left, right, span } => match (evaluate(*left)?, evaluate(*right)?) {
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
        Ir::Divide { left, right, span } => match (evaluate(*left)?, evaluate(*right)?) {
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
        Ir::Exponentiate { left, right, span } => match (evaluate(*left)?, evaluate(*right)?) {
            (Value::Int { num: left }, Value::Int { num: right }) => {
                Ok(Value::Int { num: left + right })
            }
            (Value::Float { num: left }, Value::Float { num: right }) => {
                Ok(Value::Float { num: left + right })
            }
            _ => mk_eval_err(
                "PRE-TYPES: Exponentiate expects numbers of same type".to_string(),
                &span,
            ),
        },
        Ir::Remainder { left, right, span } => match (evaluate(*left)?, evaluate(*right)?) {
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
        Ir::ParenthesizedGroup { node, .. } => evaluate(*node),
        Ir::UnaryPlus { node, span } => match evaluate(*node)? {
            Value::Int { num } => Ok(Value::Int { num }),
            Value::Float { num } => Ok(Value::Float { num }),
            _ => mk_eval_err("PRE-TYPES: Unary plus expects number".to_string(), &span),
        },
        Ir::UnaryMinus { node, span } => match evaluate(*node)? {
            Value::Int { num } => Ok(Value::Int { num: -num }),
            Value::Float { num } => Ok(Value::Float { num: -num }),
            _ => mk_eval_err("PRE-TYPES: Unary minus expects number".to_string(), &span),
        },
        Ir::LogicalNot { node, span } => match evaluate(*node)? {
            Value::Bool { bool } => Ok(Value::Bool { bool: !bool }),
            _ => mk_eval_err("PRE-TYPES: Logical not expects bool".to_string(), &span),
        },
        Ir::LogicalAnd { left, right, span } => match evaluate(*left)? {
            Value::Bool { bool: false } => Ok(Value::Bool { bool: false }),
            Value::Bool { bool: true } => match evaluate(*right)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical and expects bools".to_string(), &span),
        },
        Ir::LogicalOr { left, right, span } => match evaluate(*left)? {
            Value::Bool { bool: true } => Ok(Value::Bool { bool: true }),
            Value::Bool { bool: false } => match evaluate(*right)? {
                Value::Bool { bool } => Ok(Value::Bool { bool }),
                _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
            },
            _ => mk_eval_err("PRE-TYPES: Logical or expects bools".to_string(), &span),
        },
    }
}

fn mk_eval_err<T>(error: String, span: &Span) -> MyteResult<T> {
    mkerr(error, span, MyteErrorType::Evaluate)
}
