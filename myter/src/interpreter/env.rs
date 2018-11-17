use std::collections::HashMap;

use common::ident::VariableID;
use interpreter::value::Value;

pub struct Environment {
    variables: Vec<HashMap<VariableID, Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            variables: vec![HashMap::new()],
        }
    }

    pub fn extend(&mut self, var: VariableID, val: Value) {
        let last_idx = self.variables.len() - 1;
        self.variables[last_idx].insert(var, val);
    }

    pub fn lookup(&self, var: VariableID) -> Value {
        for variables in self.variables.iter().rev() {
            if let Some(val) = variables.get(&var) {
                return val.clone();
            }
        }

        panic!("Variable not found in environment".to_string())
    }

    pub fn enter_scope(&mut self) {
        self.variables.push(HashMap::new())
    }

    pub fn exit_scope(&mut self) {
        self.variables.pop();
    }

    pub fn reset(&mut self) {
        while self.variables.len() > 1 {
            self.variables.pop();
        }
    }
}