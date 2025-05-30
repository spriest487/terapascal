use crate::ast;
use crate::ast::Ident;
use crate::ast::TypeConstraint;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::typecheck_type;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Spanned;

pub type TypeParam = ast::TypeParam<Value>;

impl TypeParam {
    pub fn into_generic_param_ty(self) -> Type {
        match self.constraint {
            Some(constraint) => {
                Type::generic_constrained_param(self.name, constraint.is_ty)
            },
            None => {
                Type::generic_param(self.name)
            },
        }
    }
}

pub type TypeArgList = ast::TypeArgList<Value>;

impl TypeArgList {
    pub fn apply_type_args(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        self.map(|arg, _pos | arg.apply_type_args(params, args))
    }
}

pub type TypeParamList = ast::TypeList<TypeParam>;

impl TypeParamList {
    pub fn apply_type_args(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        self
            .map(|ty_param, _pos| {
                let constraint = ty_param.constraint
                    .map(|constraint| TypeConstraint {
                        is_ty: constraint.is_ty.apply_type_args(params, args),
                        ..constraint
                    });
                
                TypeParam {
                    constraint,
                    ..ty_param
                }
            })
    }
    
    pub fn into_type_args(self) -> TypeArgList {
        self.map(|item, _pos| Type::GenericParam(Arc::new(TypeParamType {
            name: item.name,
            is_ty: item
                .constraint
                .map(|constraint| constraint.is_ty)
                .unwrap_or(Type::Any)
        })))
    }
    
    // create a version of this list with type arguments made from its own parameters.
    // for example, a declared type "A[T]" has no type arguments (it's generic), but within its own
    // body, its members refer to the specialized type "A with [generic param type T]", 
    // where T can then be substituted for real types during monomorphization
    pub fn with_own_generic_args(self) -> Self {
        let params = self.clone();
        let args = self.clone().into_type_args();
        self.apply_type_args(&params, &args)
    }
}

pub fn typecheck_type_params(
    type_params: &ast::TypeList<ast::TypeParam>,
    ctx: &mut Context,
) -> TypeResult<TypeParamList> {
    let mut items = Vec::new();

    for ty_param in &type_params.items {
        let constraint = match &ty_param.constraint {
            Some(constraint) => {
                let is_ty = typecheck_type(&constraint.is_ty, ctx)?;
                
                Some(ast::TypeConstraint {
                    is_kw_span: constraint.is_kw_span.clone(),
                    name: ty_param.name.clone(),
                    span: constraint.span.clone(),
                    is_ty_span: Some(constraint.is_ty.span().clone()),
                    is_ty,
                })
            }
            None => None,
        };

        items.push(ast::TypeParam {
            name: ty_param.name.clone(),
            span: ty_param.span.clone(),
            constraint,
        });
    }

    Ok(TypeParamList::new(items, type_params.span().clone()))
}

pub fn validate_generic_constraints(args: &TypeArgList, params: &TypeParamList, ctx: &Context) -> GenericResult<()> {
    for pos in 0..params.len() {
        if let Some(constraint_ty) = &params[pos].constraint {
            let ty_arg = &args.items[pos];
            if !ty_arg.match_constraint(&constraint_ty.is_ty, ctx) {
                return Err(GenericError::ConstraintNotSatisfied {
                    is_not_ty: constraint_ty.is_ty.clone(),
                    actual_ty: Some(ty_arg.clone()),
                });
            }
        }
    }
    
    Ok(())
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct TypeParamType {
    pub name: Ident,
    pub is_ty: Type,
}

impl fmt::Display for TypeParamType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeArgsResult<'a> {
    Specialized(&'a TypeParamList, &'a TypeArgList),
    Unspecialized(&'a TypeParamList),
    NotGeneric,
}

pub trait TypeArgResolver {
    fn get(&self, pos: usize) -> Option<&Type>;

    fn len(&self) -> usize;
}

impl TypeArgResolver for TypeArgList {
    fn get(&self, pos: usize) -> Option<&Type> {
        self.items.get(pos)
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}

pub trait TypeParamContainer {
    fn find_position(&self, name: &str) -> Option<usize>;
    fn len(&self) -> usize;

    // if the given type is a generic param type corresponding to an item in this list,
    // return the corresponding argument in the given type arg list
    fn find_in_type_args<'arg>(&self, ty: &Type, args: &'arg impl TypeArgResolver) -> Option<&'arg Type> {
        let Type::GenericParam(param) = ty else {
            return None;
        };

        self
            .find_position(param.name.name.as_str())
            .and_then(|pos| args.get(pos))
    }
}

impl TypeParamContainer for TypeParamList {
    fn find_position(&self, name: &str) -> Option<usize> {
        self.items
            .iter()
            .position(|param| param.name.name.as_str() == name)
    }
    
    fn len(&self) -> usize {
        self.items.len()
    }
}
