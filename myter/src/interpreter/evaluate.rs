use common::error::{mkerr, MyteResult};
use interpreter::value::Value;
use ir::ir::Ir;

pub fn evaluate(ir: Ir) -> MyteResult<Value> {
    match ir {
        Ir::IntLiteral { num, .. } => Ok(Value::Int { num }),
        Ir::FloatLiteral { num, .. } => Ok(Value::Float { num }),
        Ir::Add { left, right, span } => match (evaluate(*left)?, evaluate(*right)?) {
            (Value::Int { num: left }, Value::Int { num: right }) => {
                Ok(Value::Int { num: left + right })
            }
            (Value::Float { num: left }, Value::Float { num: right }) => {
                Ok(Value::Float { num: left + right })
            }
            _ => mkerr(
                "PRE-TYPES: Add given numbers of different types".to_string(),
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
            _ => mkerr(
                "PRE-TYPES: Subtract given numbers of different types".to_string(),
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
            _ => mkerr(
                "PRE-TYPES: Multiply given numbers of different types".to_string(),
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
            _ => mkerr(
                "PRE-TYPES: Divide given numbers of different types".to_string(),
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
            _ => mkerr(
                "PRE-TYPES: Exponentiate given numbers of different types".to_string(),
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
            _ => mkerr(
                "PRE-TYPES: Remainder given numbers of different types".to_string(),
                &span,
            ),
        },
        Ir::ParenthesizedGroup { node, .. } => evaluate(*node),
        Ir::UnaryPlus { node, .. } => evaluate(*node),
        Ir::UnaryMinus { node, .. } => match evaluate(*node)? {
            Value::Int { num } => Ok(Value::Int { num: -num }),
            Value::Float { num } => Ok(Value::Float { num: -num }),
        },
    }
}
