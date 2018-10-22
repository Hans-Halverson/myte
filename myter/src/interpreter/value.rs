pub enum Value {
    Int { num: i64 },
    Float { num: f64 },
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Int { num } => num.to_string(),
            Value::Float { num } => num.to_string(),
        }
    }
}
