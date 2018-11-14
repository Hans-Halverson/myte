use std::collections::HashMap;

use common::ident::VariableID;
use interpreter::value::Value;

pub struct Environment {
    variables: HashMap<VariableID, Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            variables: HashMap::new(),
        }
    }

    pub fn extend(&mut self, var: VariableID, val: Value) {
        self.variables.insert(var, val);
    }

    pub fn lookup(&self, var: VariableID) -> Value {
        self.variables[&var].clone()
    }
}
