use std::collections::HashMap;

pub type VariableID = usize;

#[derive(Clone)]
struct Identifier {
    name: String,
}

#[derive(Debug)]
pub struct UnresolvedVariable {
    pub name: String,
    scope_id: ScopeID,
}

type ScopeID = usize;

#[derive(Clone)]
struct Scope {
    parent_id: Option<ScopeID>,
    variables: HashMap<String, VariableID>,
}

#[derive(Clone)]
pub struct SymbolTable {
    variables: HashMap<VariableID, Identifier>,
    current_id: ScopeID,
    scopes: Vec<Scope>,
}

impl Scope {
    fn new(parent_id: Option<ScopeID>) -> Scope {
        Scope {
            parent_id,
            variables: HashMap::new(),
        }
    }
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let root = Scope::new(None);
        let scopes = vec![root];
        SymbolTable {
            variables: HashMap::new(),
            current_id: 0,
            scopes,
        }
    }

    pub fn enter_scope(&mut self) {
        let scope = Scope::new(Some(self.current_id));
        self.scopes.push(scope);
        self.current_id = self.scopes.len();
    }

    pub fn exit_scope(&mut self) {
        let parent_id = { self.get_scope(self.current_id).parent_id.unwrap() };
        self.current_id = parent_id;
    }

    pub fn add_variable(&mut self, name: &str) -> VariableID {
        let var_id = self.variables.len();
        self.scopes[self.current_id]
            .variables
            .insert(name.to_string(), var_id);
        var_id
    }

    pub fn unresolved_variable(&self, name: &str) -> UnresolvedVariable {
        UnresolvedVariable {
            name: name.to_string(),
            scope_id: self.current_id,
        }
    }

    pub fn resolve_variable(&self, var: &UnresolvedVariable) -> Option<VariableID> {
        return self.lookup_variable_in_scope(var.scope_id, &var.name);
    }

    fn get_scope(&self, scope_id: ScopeID) -> &Scope {
        return &self.scopes[scope_id];
    }

    fn lookup_variable_in_scope(&self, scope_id: ScopeID, name: &str) -> Option<VariableID> {
        let scope = self.get_scope(scope_id);
        match scope.variables.get(name) {
            Some(ident) => Some(ident.clone()),
            None => match &scope.parent_id {
                Some(parent_id) => self.lookup_variable_in_scope(*parent_id, name),
                None => None,
            },
        }
    }

    pub fn reset(&mut self) {
        self.current_id = 0;
    }
}
