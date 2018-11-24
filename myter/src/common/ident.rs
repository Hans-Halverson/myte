use std::collections::HashMap;

use common::error::{ErrorContext, MyteError, MyteErrorType};
use common::span::Span;

pub type IdentifierID = u32;
pub type TypeIdentifierID = u32;

#[derive(Clone)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

#[derive(Clone)]
pub struct TypeIdentifier {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct UnresolvedVariable {
    pub name: String,
    scope_id: ScopeID,
}

#[derive(Debug)]
pub struct UnresolvedType {
    pub name: String,
    scope_id: ScopeID,
    span: Span,
    in_def: bool,
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
    variables: HashMap<String, IdentifierID>,
    types: HashMap<String, IdentifierID>,
    ty: ScopeType,
}

#[derive(Clone)]
pub struct SymbolTable {
    variables: Vec<Identifier>,
    types: Vec<TypeIdentifier>,
    current_id: ScopeID,
    scopes: Vec<Scope>,
    main_id: Option<IdentifierID>,
}

impl Scope {
    fn new(id: ScopeID, parent_id: Option<ScopeID>, ty: ScopeType) -> Scope {
        Scope {
            id,
            parent_id,
            variables: HashMap::new(),
            types: HashMap::new(),
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
            variables: Vec::new(),
            types: Vec::new(),
            current_id: root_id,
            scopes,
            main_id: None,
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

    pub fn add_variable(&mut self, name: &str, span: &Span) -> IdentifierID {
        let var_id = self.variables.len() as u32;

        self.variables.push(Identifier {
            name: name.to_string(),
            span: span.clone(),
        });

        if self.get_scope(self.current_id).ty != ScopeType::Package {
            self.enter_scope(ScopeType::Variable);
        }

        self.scopes[self.current_id]
            .variables
            .insert(name.to_string(), var_id);

        var_id
    }

    pub fn add_function(
        &mut self,
        name: &str,
        span: &Span,
        error_ctx: &mut ErrorContext,
    ) -> IdentifierID {
        let var_id = self.variables.len() as u32;

        match self.get_scope(self.current_id).ty {
            ScopeType::Function => {}
            ScopeType::Package => {
                if name == "main" {
                    match self.main_id {
                        None => self.main_id = Some(var_id),
                        Some(_) => error_ctx.add_error(MyteError::new(
                            "main already defined".to_string(),
                            span,
                            MyteErrorType::Resolve,
                        )),
                    }
                }
            }
            _ => self.enter_scope(ScopeType::Function),
        }

        self.variables.push(Identifier {
            name: name.to_string(),
            span: span.clone(),
        });

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

    pub fn unresolved_type(&self, name: &str, span: &Span, in_def: bool) -> UnresolvedType {
        UnresolvedType {
            name: name.to_string(),
            scope_id: self.current_id,
            span: span.clone(),
            in_def,
        }
    }

    pub fn resolve_variable(&self, var: &UnresolvedVariable) -> Option<IdentifierID> {
        return self.lookup_variable_in_scope(var.scope_id, &var.name);
    }

    pub fn resolve_type(&mut self, ty: &UnresolvedType) -> Option<TypeIdentifierID> {
        let type_scope_id = self.current_non_shadow_scope();
        if let Some(type_id) = self.lookup_type_in_scope(type_scope_id, &ty.name) {
            return Some(type_id);
        }

        if ty.in_def {
            let type_id = self.types.len() as u32;

            self.types.push(TypeIdentifier {
                name: ty.name.to_string(),
                span: ty.span.clone(),
            });

            self.scopes[type_scope_id]
                .types
                .insert(ty.name.to_string(), type_id);

            Some(type_id)
        } else {
            None
        }
    }

    fn get_scope(&self, scope_id: ScopeID) -> &Scope {
        return &self.scopes[scope_id];
    }

    fn lookup_variable_in_scope(&self, scope_id: ScopeID, name: &str) -> Option<IdentifierID> {
        let scope = self.get_scope(scope_id);
        match scope.variables.get(name) {
            Some(ident) => Some(ident.clone()),
            None => match &scope.parent_id {
                Some(parent_id) => self.lookup_variable_in_scope(*parent_id, name),
                None => None,
            },
        }
    }

    fn lookup_type_in_scope(&self, scope_id: ScopeID, name: &str) -> Option<TypeIdentifierID> {
        let scope = self.get_scope(scope_id);
        match scope.types.get(name) {
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

    pub fn get_main_id(&self) -> Option<IdentifierID> {
        self.main_id
    }

    pub fn get_ident(&self, var_id: IdentifierID) -> &Identifier {
        &self.variables[var_id as usize]
    }
}
