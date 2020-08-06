use common::ident::IdentifierID;
use ir::nodes::IrStmt;
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
        body: Box<IrStmt>,
        ty: InferType,
    },
    Tuple(Vec<Value>),
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
            Value::Tuple(elements) => {
                InferType::Tuple(elements.iter().map(|val| val.ty()).collect())
            }
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
            Value::Tuple(elements) => {
                let elements_format = elements
                    .iter()
                    .map(|element| element.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("({})", elements_format)
            }
        }
    }
}
