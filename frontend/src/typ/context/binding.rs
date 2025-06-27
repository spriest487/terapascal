use crate::ast::Access;
use crate::ast::Ident;
use crate::ast::SemanticHint;
use crate::ast::Visibility;
use crate::typ::ast::FieldDecl;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::OverloadCandidate;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::ValueKind;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub ty: Type,
    pub kind: ValueKind,
    pub def: Option<Ident>,

    pub semantic_hint: SemanticHint,
}

impl Binding {
    pub fn pattern_binding(name: Ident, ty: impl Into<Type>) -> Self {
        Binding {
            ty: ty.into(),
            def: Some(name),
            kind: ValueKind::Immutable,
            semantic_hint: SemanticHint::Variable,
        }
    }
}

#[derive(Clone, Debug)]
pub enum InstanceMember {
    Field {
        ty: Type,
        access: Access,
        decl: FieldDecl,
        decl_index: usize,
    },
    Method {
        iface_ty: Type,
        self_ty: Type,
        method: MethodDecl,
    },
    UFCSCall {
        func_name: Symbol,
        visibility: Visibility,
        decl: Arc<FunctionDecl>,
    },
    Overloaded {
        candidates: Vec<OverloadCandidate>,
    },
}
