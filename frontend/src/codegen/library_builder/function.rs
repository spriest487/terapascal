use crate::ast::IdentPath;
use crate::typ;
use std::borrow::Cow;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDeclKey {
    Function {
        name: IdentPath,
        sig: Rc<typ::FunctionSig>,
    },
    Method(MethodDeclKey),
    VirtualMethod(VirtualMethodKey),
}

impl FunctionDeclKey {
    pub fn namespace(&self) -> Cow<IdentPath> {
        match self {
            FunctionDeclKey::Function { name, .. } => name
                .parent()
                .map(Cow::Owned)
                .expect("all functions must be declared within a namespace!"),

            FunctionDeclKey::VirtualMethod(key) => {
                key.iface_ty.full_path().expect("types used as interfaces should never be unnamed")
            },

            FunctionDeclKey::Method(key) => key
                .self_ty
                .full_path()
                .expect("types with method implementations should never be unnamed"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDeclKey {
    pub self_ty: typ::Type,
    pub method_index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VirtualMethodKey {
    pub iface_ty: typ::Type,
    pub iface_method_index: usize,

    pub impl_method: MethodDeclKey,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionDefKey {
    pub decl_key: FunctionDeclKey,
    pub type_args: Option<typ::TypeArgList>,
}
