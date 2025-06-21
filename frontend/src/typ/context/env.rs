use crate::ast::IdentPath;
use crate::typ::Type;
use crate::typ::TypeParamList;
use crate::ast::Ident;
use linked_hash_map::LinkedHashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Environment {
    Global,
    Namespace { namespace: IdentPath },
    TypeDecl { ty: Type },
    FunctionDecl { owning_ty_params: Option<TypeParamList> },
    FunctionBody(FunctionBodyEnvironment),
    ClosureBody(ClosureBodyEnvironment),
    Block { allow_unsafe: bool },
}

impl Environment {
    pub fn namespace(&self) -> Option<&IdentPath> {
        match self {
            Environment::Namespace { namespace } => Some(namespace),
            _ => None,
        }
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            Environment::Global => "Global",
            Environment::Namespace { .. } => "Namespace",
            Environment::TypeDecl { .. } => "TypeDecl",
            Environment::FunctionDecl { .. } => "FunctionDecl",
            Environment::FunctionBody { .. } => "FunctionBody",
            Environment::ClosureBody { .. } => "ClosureBody",
            Environment::Block { .. } => "Block",
        }
    }
}

impl From<FunctionBodyEnvironment> for Environment {
    fn from(value: FunctionBodyEnvironment) -> Self {
        Environment::FunctionBody(value)
    }
}

impl From<ClosureBodyEnvironment> for Environment {
    fn from(value: ClosureBodyEnvironment) -> Self {
        Environment::ClosureBody(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionBodyEnvironment {
    pub result_ty: Type,
    pub self_ty: Option<Type>,
    pub ty_params: Option<TypeParamList>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClosureBodyEnvironment {
    pub result_ty: Option<Type>,
    pub captures: LinkedHashMap<Ident, Type>
}
