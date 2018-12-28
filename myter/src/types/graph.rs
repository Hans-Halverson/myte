use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use types::infer::{InferType, InferVarID};

#[derive(Clone)]
pub struct InferNode {
    ty: InferType,
    parent: Option<Rc<RefCell<InferNode>>>,
    rank: u32,
}

#[derive(Clone)]
pub struct TypeGraph {
    pub infer_map: HashMap<InferVarID, Rc<RefCell<InferNode>>>,
}

impl TypeGraph {
    pub fn new() -> TypeGraph {
        TypeGraph {
            infer_map: HashMap::new(),
        }
    }

    fn get_root(&self, node: Rc<RefCell<InferNode>>) -> Rc<RefCell<InferNode>> {
        if let Some(ref parent) = node.borrow().parent {
            return self.get_root(parent.clone());
        }

        node
    }

    fn find_root(&mut self, var: InferVarID) -> Rc<RefCell<InferNode>> {
        if !self.infer_map.contains_key(&var) {
            let node = InferNode {
                ty: InferType::InferVariable(var),
                parent: None,
                rank: 0,
            };
            self.infer_map.insert(var, Rc::new(RefCell::new(node)));
            self.infer_map[&var].clone()
        } else {
            self.get_root(self.infer_map[&var].clone())
        }
    }

    pub fn unify(&mut self, ty1: &InferType, ty2: &InferType) -> bool {
        let t1 = if let InferType::InferVariable(var) = ty1 {
            let root_rc = self.find_root(*var);
            let root = root_rc.borrow();
            root.ty.clone()
        } else {
            ty1.clone()
        };

        let t2 = if let InferType::InferVariable(var) = ty2 {
            let root_rc = self.find_root(*var);
            let root = root_rc.borrow();
            root.ty.clone()
        } else {
            ty2.clone()
        };

        match (t1, t2) {
            (InferType::Unit, InferType::Unit)
            | (InferType::Bool, InferType::Bool)
            | (InferType::Int, InferType::Int)
            | (InferType::Float, InferType::Float)
            | (InferType::String, InferType::String) => true,
            (InferType::Never, _) | (_, InferType::Never) => true,
            (InferType::ParamVariable(p1), InferType::ParamVariable(p2)) => p1 == p2,
            (InferType::Function(ref args1, ref r1), InferType::Function(ref args2, ref r2)) => {
                args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2)
                        .all(|(arg1, arg2)| self.unify(arg1, arg2))
                    && self.unify(r1, r2)
            }
            (InferType::Tuple(ref elements1), InferType::Tuple(ref elements2)) => {
                elements1.len() == elements2.len()
                    && elements1
                        .iter()
                        .zip(elements2)
                        .all(|(ty1, ty2)| self.unify(ty1, ty2))
            }
            (InferType::InferVariable(var1), InferType::InferVariable(var2)) => {
                if var1 != var2 {
                    self.merge_variables(var1, var2);
                }

                true
            }
            (InferType::InferVariable(var), ty) | (ty, InferType::InferVariable(var)) => {
                self.resolve_variable(var, &ty);
                true
            }
            (_, _) => false,
        }
    }

    fn merge_variables(&mut self, var1: InferVarID, var2: InferVarID) {
        let r1_rc = self.find_root(var1);
        let mut r1 = r1_rc.borrow_mut();

        let r2_rc = self.find_root(var2);
        let mut r2 = r2_rc.borrow_mut();

        if r1.rank > r2.rank {
            r2.parent = Some(r1_rc.clone());
        } else {
            r1.parent = Some(r2_rc.clone());

            if r1.rank == r2.rank {
                r2.rank += 1
            }
        }
    }

    fn resolve_variable(&mut self, var: InferVarID, ty: &InferType) {
        let root_rc = self.find_root(var);
        let mut root = root_rc.borrow_mut();

        root.ty = ty.clone();
    }

    pub fn rep(&mut self, ty: &InferType) -> InferType {
        let rep_ty = match ty {
            InferType::InferVariable(var) => {
                let root_rc = self.find_root(*var);
                let root = root_rc.borrow();
                root.ty.clone()
            }
            ty => ty.clone(),
        };

        match rep_ty {
            InferType::Never
            | InferType::Unit
            | InferType::Bool
            | InferType::Int
            | InferType::Float
            | InferType::String
            | InferType::ParamVariable(_)
            | InferType::InferVariable(_) => rep_ty,
            InferType::Function(ref args, ref ret) => InferType::Function(
                args.iter().map(|arg| self.rep(arg)).collect(),
                Box::new(self.rep(ret)),
            ),
            InferType::Tuple(elements) => {
                InferType::Tuple(elements.iter().map(|ty| self.rep(ty)).collect())
            }
        }
    }
}
