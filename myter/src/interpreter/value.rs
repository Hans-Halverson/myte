use common::ident::IdentifierID;
use ir::ir::IrExpr;
use types::infer::InferType;

#[derive(Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    String(String),
    Int(i64),
    Float(f64),
    Closure {
        params: Vec<IdentifierID>,
        body: Box<IrExpr>,
        ty: InferType,
    },
}

impl Value {
    pub fn ty(&self) -> InferType {
        match self {
            Value::Unit => InferType::Unit,
            Value::Bool(_) => InferType::Bool,
            Value::String(_) => InferType::String,
            Value::Int(_) => InferType::Int,
            Value::Float(_) => InferType::Float,
            Value::Closure { ty, .. } => ty.clone(),
        }
    }
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Bool(bool) => bool.to_string(),
            Value::String(string) => string.to_string(),
            Value::Int(num) => num.to_string(),
            Value::Float(num) => num.to_string(),
            Value::Closure { .. } => "<function>".to_string(),
        }
    }
}
