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

#[derive(Clone, PartialEq)]
pub enum ScopeType {
    Package,
    Block,
    Function,
    FunctionBody,
    Variable,
}

#[derive(Clone)]
struct Scope {
    id: ScopeID,
    parent_id: Option<ScopeID>,
    variables: HashMap<String, VariableID>,
    ty: ScopeType,
}

#[derive(Clone)]
pub struct SymbolTable {
    variables: HashMap<VariableID, Identifier>,
    current_id: ScopeID,
    scopes: Vec<Scope>,
}

impl Scope {
    fn new(id: ScopeID, parent_id: Option<ScopeID>, ty: ScopeType) -> Scope {
        Scope {
            id,
            parent_id,
            variables: HashMap::new(),
            ty,
        }
    }
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let root_id = 0;
        let root = Scope::new(root_id, None, ScopeType::Package);
        let scopes = vec![root];
        SymbolTable {
            variables: HashMap::new(),
            current_id: root_id,
            scopes,
        }
    }

    pub fn enter_scope(&mut self, ty: ScopeType) {
        let scope_id = self.scopes.len();
        let scope = Scope::new(scope_id, Some(self.current_id), ty);
        self.current_id = scope_id;
        self.scopes.push(scope);
    }

    pub fn exit_scope(&mut self) {
        self.current_id = self
            .get_scope(self.current_non_shadow_scope())
            .parent_id
            .unwrap();
    }

    pub fn add_variable(&mut self, name: &str) -> VariableID {
        let var_id = self.variables.len();

        self.variables.insert(
            var_id,
            Identifier {
                name: name.to_string(),
            },
        );

        if self.get_scope(self.current_id).ty != ScopeType::Package {
            self.enter_scope(ScopeType::Variable);
        }

        self.scopes[self.current_id]
            .variables
            .insert(name.to_string(), var_id);

        var_id
    }

    pub fn add_function(&mut self, name: &str) -> VariableID {
        let var_id = self.variables.len();

        self.variables.insert(
            var_id,
            Identifier {
                name: name.to_string(),
            },
        );

        if self.get_scope(self.current_id).ty != ScopeType::Package
            && self.get_scope(self.current_id).ty != ScopeType::Function
        {
            self.enter_scope(ScopeType::Function);
        }

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

    fn current_non_shadow_scope(&self) -> ScopeID {
        let mut current = self.get_scope(self.current_id);
        while current.ty == ScopeType::Variable || current.ty == ScopeType::Function {
            current = self.get_scope(current.parent_id.unwrap());
        }

        current.id
    }

    pub fn reset(&mut self) {
        self.current_id = 0;
    }
}
