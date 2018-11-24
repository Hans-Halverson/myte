use std::collections::HashMap;

use common::ident::{IdentifierID, TypeIdentifierID};
use ir::ir::IrID;

pub type InferVarID = u32;
pub type ParamVarID = u32;

#[derive(Clone)]
pub struct InferContext {
    next_infer_var_id: InferVarID,
    next_param_var_id: ParamVarID,

    pub ir_to_type: HashMap<IrID, InferVarID>,
    pub ident_to_type: HashMap<IdentifierID, InferType>,
    pub type_ident_to_type: HashMap<TypeIdentifierID, InferType>,
}

impl InferContext {
    pub fn new() -> InferContext {
        InferContext {
            next_infer_var_id: 0,
            next_param_var_id: 0,
            ir_to_type: HashMap::new(),
            ident_to_type: HashMap::new(),
            type_ident_to_type: HashMap::new(),
        }
    }

    pub fn new_infer_var_id(&mut self) -> InferVarID {
        let id = self.next_infer_var_id;
        self.next_infer_var_id += 1;
        id
    }

    pub fn new_param_var_id(&mut self) -> InferVarID {
        let id = self.next_infer_var_id;
        self.next_infer_var_id += 1;
        id
    }
}

#[derive(Clone)]
pub enum InferType {
    InferVariable(InferVarID),
    ParamVariable(ParamVarID),
    Unit,
    Bool,
    Int,
    Float,
    String,
    Function(Vec<InferType>, Box<InferType>),
}
