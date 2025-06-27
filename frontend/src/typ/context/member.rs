use crate::ast::Access;
use crate::ast::Ident;
use crate::typ::ast::FunctionDef;
use crate::typ::ast::MethodDecl;
use crate::typ::FunctionSig;
use crate::typ::Type;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum TypeMember {
    Method(MethodGroupMember),
    MethodGroup(Vec<MethodGroupMember>),
}

impl TypeMember {
    pub fn from_method_members(mut members: Vec<MethodGroupMember>) -> TypeMember {
        if members.len() == 1 {
            let single_method = members.remove(0);
            TypeMember::Method(single_method)
        } else {
            TypeMember::MethodGroup(members)
        }
    }
}

#[derive(Clone, Debug)]
pub struct MethodGroupMember {
    pub iface_ty: Type,
    pub method: MethodDecl,
    pub index: usize,
}

impl TypeMember {
    pub fn access(&self) -> Access {
        match self {
            TypeMember::Method(member) => member.method.access,

            TypeMember::MethodGroup(members) => members
                .iter()
                .map(|m| m.method.access)
                .max()
                .unwrap_or(Access::Public),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(super) struct MethodKey {
    pub name: Ident,
    pub sig: Arc<FunctionSig>,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct MethodCollection {
    pub(super) methods: HashMap<MethodKey, Option<Arc<FunctionDef>>>,
}

impl MethodCollection {
    pub(super) fn new() -> Self {
        Self {
            methods: HashMap::new(),
        }
    }
}
