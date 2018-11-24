use std::collections::HashMap;

use common::ident::IdentifierID;
use interpreter::value::Value;

pub struct Environment {
    variables: Vec<HashMap<IdentifierID, Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            variables: vec![HashMap::new()],
        }
    }

    pub fn extend(&mut self, var: IdentifierID, val: &Value) {
        let last_idx = self.variables.len() - 1;
        self.variables[last_idx].insert(var, val.clone());
    }

    pub fn lookup(&self, var: IdentifierID) -> Value {
        for variables in self.variables.iter().rev() {
            if let Some(val) = variables.get(&var) {
                return val.clone();
            }
        }

        panic!("Variable not found in environment".to_string())
    }

    pub fn reassign(&mut self, var: IdentifierID, val: &Value) {
        for variables in self.variables.iter_mut().rev() {
            if variables.contains_key(&var) {
                variables.insert(var, val.clone());
                return;
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
