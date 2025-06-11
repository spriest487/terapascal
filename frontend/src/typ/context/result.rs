use crate::ast::{Ident, TypeIdentList};
use crate::ast::IdentPath;
use crate::typ::context::Decl;
use crate::typ::{FunctionSig, TypeParamList};
use crate::typ::ScopeMemberKind;
use crate::typ::Type;
use crate::typ::Value;
use terapascal_common::span::*;
use std::fmt;
use std::fmt::Debug;

#[derive(Debug)]
pub enum GenericTarget {
    Name(IdentPath),
    FunctionSig(FunctionSig),
    Type(Type),
}

impl fmt::Display for GenericTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericTarget::Name(name) => write!(f, "name `{}`", name),
            GenericTarget::FunctionSig(sig) => write!(f, "function with signature `{}`", sig),
            GenericTarget::Type(ty) => write!(f, "type `{}`", ty),
        }
    }
}

#[derive(Debug)]
pub enum GenericTypeHint {
    Unknown,
    ExpectedValueType(Type),
    ExpectedReturnType(Type),
    ArgTypes(Vec<Type>),
}

impl fmt::Display for GenericTypeHint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericTypeHint::Unknown => write!(f, "unknown"),

            GenericTypeHint::ExpectedValueType(ty) => {
                write!(f, "expected type {}", ty)
            },

            GenericTypeHint::ExpectedReturnType(ty) => {
                write!(f, "expected return type {}", ty)
            },

            GenericTypeHint::ArgTypes(tys) => {
                write!(f, "argument type")?;
                if tys.len() > 1 {
                    write!(f, "s")?;
                }

                if !tys.is_empty() {
                    write!(f, " ")?;
                    for (i, ty) in tys.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "`{}`", ty)?;
                    }
                }

                Ok(())
            },
        }
    }
}

#[derive(Debug)]
pub enum GenericError {
    ArgsLenMismatch {
        target: GenericTarget,
        expected: usize,
        actual: usize,
    },
    ParamMismatch {
        target: GenericTarget,

        expected: Option<TypeParamList>,
        actual: Option<TypeIdentList>,
    },
    UnexpectedConstraintList,
    UnexpectedConstraint {
        constraint: Ident,
    },
    DuplicateConstraint {
        constraint: Ident,
    },
    ConstraintNotSatisfied {
        is_not_ty: Type,
        actual_ty: Option<Type>, // may be unknown/not yet resolved when processing generics
    },
    CannotInferArgs {
        target: GenericTarget,
        hint: GenericTypeHint,
    },
    IllegalUnspecialized {
        ty: Type,
    },
}

pub type GenericResult<T> = Result<T, GenericError>;

impl fmt::Display for GenericError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GenericError::ArgsLenMismatch {
                target,
                expected,
                actual,
                ..
            } => write!(
                f,
                "{} expects {} type argument(s), found {}",
                target, expected, actual
            ),
            
            GenericError::ParamMismatch { target, expected, actual, .. } => {
                write!(f, "{} expects ", target)?;
                match expected {
                    Some(params) => write!(f, "parameter list {}", params)?,
                    None => write!(f, "no parameters")?,
                }
                write!(f, ", got ")?;
                match actual {
                    Some(params) => write!(f, "parameter list {}", params)?,
                    None => write!(f, "no parameters")?,
                }

                Ok(())
            },

            GenericError::UnexpectedConstraintList => {
                write!(f, "constraint list is not expected here")
            },

            GenericError::UnexpectedConstraint { constraint, .. } => {
                write!(f, "`{}` does not match an expected type parameter", constraint)
            }

            GenericError::DuplicateConstraint { constraint, .. } => {
                write!(f, "type parameter {} already has a constraint", constraint)
            }

            GenericError::ConstraintNotSatisfied {
                is_not_ty,
                actual_ty: Some(arg_ty),
                ..
            } => write!(
                f,
                "argument type {} does not meet the type constraint on this parameter: must be {}",
                arg_ty, is_not_ty,
            ),

            GenericError::ConstraintNotSatisfied {
                is_not_ty,
                ..
            } => write!(
                f,
                "argument does not meet the type constraint on this parameter: must be {}",
                is_not_ty,
            ),

            GenericError::CannotInferArgs { target, hint, .. } => {
                write!(f, "cannot infer type arguments for {}", target)?;

                match hint {
                    GenericTypeHint::Unknown 
                    | GenericTypeHint::ExpectedValueType(Type::Nothing) => Ok(()),
                    | GenericTypeHint::ExpectedReturnType(Type::Nothing) => Ok(()),
                    
                    hint => write!(f, " from {}", hint),
                }
            },

            GenericError::IllegalUnspecialized { ty, .. } => write!(
                f,
                "the type `{}` cannot be used without type arguments in this context",
                ty,
            ),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum ExpectedKind {
    AnyType,
    Record,
    Class,
    Variant,
    Interface,
    SetDecl,
    AnyBinding,
    Function,
    Namespace,
}

#[derive(Debug)]
pub enum Named {
    Decl(Decl),
    Namespace(IdentPath),
}

impl From<Decl> for Named {
    fn from(decl: Decl) -> Self {
        Named::Decl(decl)
    }
}

impl fmt::Display for Named {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Named::Decl(decl) => write!(f, "{}", decl),
            Named::Namespace(ident_path) => write!(f, "namespace `{}`", ident_path),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum NameContainer {
    Namespace(IdentPath),
    Type(Type),
}

impl NameContainer {
    pub fn from_value(a: &Value) -> Self {
        match a {
            Value::Namespace(ns, ..) => NameContainer::Namespace(ns.clone()),
            _ => NameContainer::Type(a.ty().into_owned()),
        }
    }
}

impl fmt::Display for NameContainer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameContainer::Namespace(ns) => write!(f, "namespace `{}`", ns),
            NameContainer::Type(ty) => write!(f, "type `{}`", ty),
        }
    }
}

#[derive(Debug)]
pub enum NameError {
    NotFound {
        ident: IdentPath,
    },
    MemberNotFound {
        base: NameContainer,
        member: Ident,
    },
    DefDeclMismatch {
        path: IdentPath,
        decl: Span,
        def: Span,
    },
    Unexpected {
        ident: IdentPath,
        expected: ExpectedKind,
        actual: Named,
    },
    AlreadyDeclared {
        new: Ident,
        existing: IdentPath,
        existing_kind: ScopeMemberKind,
        conflict: DeclConflict,
    },
    AlreadyDefined {
        ident: IdentPath,
        existing: Span,
    },
    NoImplementationFound {
        owning_ty: Type,
        impl_ty: Type,
    },
    AlreadyImplemented {
        owning_ty: IdentPath,
        for_ty: Type,
        method: Ident,
        existing: Span,
    },
    Ambiguous {
        ident: Ident,
        options: Vec<IdentPath>,
    },
    GenericError(GenericError),
}

impl NameError {
    pub fn not_found(name: impl Into<IdentPath>) -> Self {
        NameError::NotFound { ident: name.into() }
    }

    pub fn value_member_not_found(base: &Value, ident: impl Into<Ident>) -> Self {
        let base = NameContainer::from_value(base); 
        NameError::MemberNotFound {
            base,
            member: ident.into(),
        }
    }
    
    pub fn namespace_member_not_found(namespace: impl Into<IdentPath>, ident: impl Into<Ident>) -> Self {
        NameError::MemberNotFound {
            base: NameContainer::Namespace(namespace.into()),
            member: ident.into(),
        }
    }

    pub fn type_member_not_found(ty: impl Into<Type>, ident: impl Into<Ident>) -> Self {
        NameError::MemberNotFound {
            base: NameContainer::Type(ty.into()),
            member: ident.into(),
        }
    }
}

impl From<GenericError> for NameError {
    fn from(err: GenericError) -> Self {
        NameError::GenericError(err)
    }
}

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameError::NotFound { ident, .. } => {
                write!(f, "`{}` is not found in this scope", ident)
            },
            NameError::MemberNotFound { base, member, .. } => {
                write!(f, "{} does not have a member named `{}`", base, member)
            },
            NameError::Unexpected {
                ident,
                expected,
                actual,
            } => {
                let expected_desc = match expected {
                    ExpectedKind::Class => "class",
                    ExpectedKind::Record => "record",
                    ExpectedKind::Variant => "variant",
                    ExpectedKind::Interface => "interface",
                    ExpectedKind::Function => "function",
                    ExpectedKind::AnyBinding => "value",
                    ExpectedKind::Namespace => "namespace",
                    ExpectedKind::AnyType => "type",
                    ExpectedKind::SetDecl => "set",
                };

                write!(
                    f,
                    "`{}` did not refer to a {} in this scope (found: {})",
                    ident, expected_desc, actual,
                )
            },
            NameError::AlreadyDeclared { new, conflict, .. } => {
                write!(f, "`{}` was already declared in this scope with ", new)?;
                write!(f, "{}", match conflict {
                    DeclConflict::Name => "the same name",
                    DeclConflict::Type => "a conflicting type",
                    DeclConflict::Visibility => "conflicting visibility",
                })?;

                Ok(())
            },
            NameError::AlreadyDefined { ident, .. } => {
                write!(f, "`{}` was already defined", ident)
                },
            NameError::Ambiguous { ident, .. } => {
                write!(f, "`{}` is ambiguous in this context", ident)
            },
            
            NameError::NoImplementationFound { owning_ty, impl_ty } => {
                write!(f, "type `{}` does not implement `{}`", impl_ty, owning_ty)
            }

            NameError::AlreadyImplemented {
                owning_ty: iface,
                method,
                for_ty,
                ..
            } => write!(
                f,
                "`{}.{}` already implemented for `{}`",
                iface, method, for_ty
            ),

            NameError::DefDeclMismatch { path: ident, .. } => write!(
                f,
                "definition of `{}` does not match previous declaration",
                ident
            ),

            NameError::GenericError(err) => write!(f, "{}", err),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DeclConflict {
    Name,
    Type,
    Visibility,
}

pub type NameResult<T> = Result<T, NameError>;
