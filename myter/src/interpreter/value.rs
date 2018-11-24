use common::ident::IdentifierID;
use ir::ir::IrExpr;

#[derive(Clone)]
pub enum Value {
    Unit,
    Bool {
        bool: bool,
    },
    String {
        string: String,
    },
    Int {
        num: i64,
    },
    Float {
        num: f64,
    },
    Closure {
        params: Vec<IdentifierID>,
        body: Box<IrExpr>,
    },
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Bool { bool } => bool.to_string(),
            Value::String { string } => string.to_string(),
            Value::Int { num } => num.to_string(),
            Value::Float { num } => num.to_string(),
            Value::Closure { .. } => "<function>".to_string(),
        }
    }
}
