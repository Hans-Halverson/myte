use std::collections::HashMap;
use std::fmt;

use common::ident::{IdentifierID, TypeIdentifierID};
use ir::ir::IrID;
use types::graph::TypeGraph;

pub type InferVarID = u32;
pub type ParamVarID = u32;

#[derive(Clone)]
pub struct InferContext {
    next_infer_var_id: InferVarID,
    next_param_var_id: ParamVarID,

    pub ir_to_type: HashMap<IrID, InferVarID>,
    pub ident_to_type: HashMap<IdentifierID, InferType>,
    pub type_ident_to_type: HashMap<TypeIdentifierID, InferType>,

    pub graph: TypeGraph,
}

impl InferContext {
    pub fn new() -> InferContext {
        InferContext {
            next_infer_var_id: 0,
            next_param_var_id: 0,
            ir_to_type: HashMap::new(),
            ident_to_type: HashMap::new(),
            type_ident_to_type: HashMap::new(),
            graph: TypeGraph::new(),
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

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum InferTypeVariable {
    Infer(InferVarID),
    Param(ParamVarID),
}

impl InferType {
    pub fn format(&self, vars: &HashMap<InferTypeVariable, String>) -> String {
        match self {
            InferType::InferVariable(var) => vars[&InferTypeVariable::Infer(*var)].clone(),
            InferType::ParamVariable(var) => vars[&InferTypeVariable::Param(*var)].clone(),
            InferType::Unit => "unit".to_string(),
            InferType::Bool => "bool".to_string(),
            InferType::Int => "int".to_string(),
            InferType::Float => "float".to_string(),
            InferType::String => "string".to_string(),
            InferType::Function(args, ret) => {
                let args_format = if args.len() != 0 {
                    args.iter()
                        .map(|arg| arg.format(vars))
                        .collect::<Vec<String>>()
                        .join("->")
                } else {
                    "unit".to_string()
                };

                format!("{} -> {}", args_format, ret.format(vars))
            }
        }
    }

    pub fn get_vars(&self) -> Vec<InferTypeVariable> {
        match self {
            InferType::InferVariable(var) => vec![InferTypeVariable::Infer(*var)],
            InferType::ParamVariable(var) => vec![InferTypeVariable::Param(*var)],
            InferType::Unit
            | InferType::Bool
            | InferType::Int
            | InferType::Float
            | InferType::String => Vec::new(),
            InferType::Function(args, ret) => {
                let mut all_vars = args
                    .iter()
                    .flat_map(|arg| arg.get_vars())
                    .collect::<Vec<InferTypeVariable>>();
                all_vars.extend(ret.get_vars());
                all_vars
            }
        }
    }

    pub fn format_types(tys: Vec<&InferType>) -> Vec<String> {
        let mut all_vars = tys
            .iter()
            .flat_map(|ty| ty.get_vars())
            .collect::<Vec<InferTypeVariable>>();
        all_vars.dedup();

        let mut var_strs = HashMap::new();
        for (idx, var) in all_vars.iter().enumerate() {
            let quot = idx / 26;
            let rem = idx % 26;

            let var_str = if quot == 0 {
                ((b'a' + rem as u8) as char).to_string()
            } else {
                format!("{}{}", (b'a' + rem as u8) as char, quot + 1)
            };

            var_strs.insert(var.clone(), var_str);
        }

        tys.iter().map(|ty| ty.format(&var_strs)).collect()
    }
}

impl fmt::Debug for InferType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InferType::InferVariable(var) => write!(f, "{}", *var),
            InferType::ParamVariable(var) => write!(f, "{}", *var),
            InferType::Unit => write!(f, "unit"),
            InferType::Bool => write!(f, "bool"),
            InferType::Int => write!(f, "int"),
            InferType::Float => write!(f, "float"),
            InferType::String => write!(f, "string"),
            InferType::Function(args, ret) => {
                let args_format = if args.len() != 0 {
                    args.iter()
                        .map(|arg| format!("{:?}", arg))
                        .collect::<Vec<String>>()
                        .join("->")
                } else {
                    "unit".to_string()
                };

                write!(f, "{} -> {}", args_format, format!("{:?}", ret))
            }
        }
    }
}
