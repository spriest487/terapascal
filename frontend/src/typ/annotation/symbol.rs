use crate::ast::IdentPath;
use crate::ast::TypeDeclName;
use crate::typ::typecheck_type_params;
use crate::typ::validate_generic_constraints;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::InvalidTypeParamsDeclKind;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeError;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use crate::typ::TypeResult;
use crate::Ident;
use common::span::Span;
use common::span::Spanned;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;
use crate::typ::ast::WhereClause;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Symbol {
    pub full_path: IdentPath,

    pub type_params: Option<TypeParamList>,
    pub type_args: Option<TypeArgList>,
}

impl Symbol {
    pub fn with_ty_params(self, type_params: Option<TypeParamList>) -> Self {
        Self {
            type_params,
            ..self
        }
    }
    
    pub fn with_ty_args(self, type_args: Option<TypeArgList>) -> Self {
        Self {
            type_args,
            ..self
        }
    }
    
    /// Creates a symbol of the given decl, using the context's current local namespace 
    /// to create the fully qualified name
    pub fn from_local_decl_name(
        decl_name: &TypeDeclName,
        constraint_clause: Option<&WhereClause>,
        ctx: &mut Context
    ) -> TypeResult<Self> {
        let type_params = match (&decl_name.type_params, constraint_clause) {
            (Some(param_name_list), None) => {
                let param_list = param_name_list
                    .clone()
                    .to_unconstrained_params();
                
                let params = typecheck_type_params(&param_list, ctx)?;
                Some(params)
            },

            (Some(param_name_list), Some(where_clause)) => {
                let params = where_clause
                    .clone()
                    .typecheck_constrained_params(param_name_list.clone())?;

                Some(params)
            }

            (None, None) => {
                None
            }

            (None, Some(unexpected_where)) => {
                let err = GenericError::UnexpectedConstraintList;
                return Err(TypeError::from_generic_err(err, unexpected_where.span.clone()));
            }
        };

        let full_path = ctx.qualify_name(decl_name.ident.clone());

        let sym = Symbol {
            full_path,
            type_params,

            type_args: None,
        };
        
        Ok(sym)
    }
    
    pub fn expect_not_unspecialized(&self) -> GenericResult<()> {
        if !self.is_unspecialized_generic() {
            Ok(())
        } else {
            Err(GenericError::IllegalUnspecialized {
                ty: Type::Class(Rc::new(self.clone())),
            })
        }
    }

    pub fn specialize<'a>(
        &'a self,
        args: &TypeArgList,
        ctx: &Context,
    ) -> GenericResult<Cow<'a, Symbol>> {
        let type_params = match self.type_params.as_ref() {
            None => return Ok(Cow::Borrowed(self)),
            Some(type_params) => type_params,
        };

        if args.len() != type_params.items.len() {
            return Err(GenericError::ArgsLenMismatch {
                target: GenericTarget::Name(self.full_path.clone()),
                expected: type_params.items.len(),
                actual: args.len(),
            });
        }

        validate_generic_constraints(args, type_params, ctx)?;

        let type_args = if let Some(existing_args) = &self.type_args {
            existing_args
                .clone()
                .map(|arg, _pos| arg.apply_type_args(type_params, args))
        } else {
            let mut resolved_args = Vec::with_capacity(type_params.len());

            for i in 0..type_params.len() {
                let arg = args.get(i).unwrap();
                resolved_args.push(arg.clone());
            }
            TypeArgList::new(resolved_args, self.span().clone())
        };

        let name = Symbol {
            type_args: Some(type_args),
            ..self.clone()
        };

        Ok(Cow::Owned(name))
    }
    
    pub fn expect_no_type_params(&self, invalid_kind: InvalidTypeParamsDeclKind) -> TypeResult<()> {
        match &self.type_params {
            None => Ok(()),

            Some(params) => {
                Err(TypeError::InvalidDeclWithTypeParams {
                    span: params.span.clone(),
                    kind: invalid_kind,
                })
            }
        }
    }
    
    pub fn ident(&self) -> &Ident {
        self.full_path.last()
    }

    pub fn visit_types_ref<Visitor>(&self, visitor: Visitor)
    where
        Visitor: Fn(&Type) + Copy
    {
        if let Some(arg_list) = &self.type_args {
            for ty_arg in &arg_list.items {
                visitor(ty_arg);
            }
        }

        if let Some(param_list) = &self.type_params {
            for ty_param in &param_list.items {
                if let Some(constraint) = &ty_param.constraint {
                    visitor(&constraint.is_ty);
                }
            }
        }
    }
    
    pub fn visit_types_mut<Visitor>(&mut self, visitor: Visitor) 
    where
        Visitor: Fn(&mut Type) + Copy
    {
        if let Some(arg_list) = &mut self.type_args {
            for ty_arg in &mut arg_list.items {
                ty_arg.visit_types_mut(visitor);
            }
        }

        if let Some(param_list) = &mut self.type_params {
            for ty_param in &mut param_list.items {
                if let Some(constraint) = &mut ty_param.constraint {
                    constraint.is_ty.visit_types_mut(visitor);
                }
            }
        }
    }
}

impl From<IdentPath> for Symbol {
    fn from(value: IdentPath) -> Self {
        Symbol {
            full_path: value,
            
            type_args: None,
            type_params: None,
        }
    }
}

impl Specializable for Symbol {
    type GenericID = IdentPath;

    /// is this type missing specialization e.g. it has type params and not type arguments
    fn is_unspecialized_generic(&self) -> bool {
        if self.type_params.is_none() {
            return false;
        }
        self.type_args.is_none()
    }

    fn name(&self) -> Cow<IdentPath> {
        Cow::Borrowed(&self.full_path)
    }

    fn apply_type_args(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        let sym_ty_args = self
            .type_args
            .map(|args_list| {
                args_list.apply_type_args(params, args)
            });

        let sym_ty_params = self
            .type_params
            .map(|params_list| {
                params_list.apply_type_args(params, args)
            });
        
        Symbol {
            full_path: self.full_path,
            type_args: sym_ty_args,
            type_params: sym_ty_params,
        }
    }
}

impl Spanned for Symbol {
    fn span(&self) -> &Span {
        self.full_path.span()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.full_path)?;

        if let Some(type_args) = &self.type_args {
            write!(f, "{}", type_args)?;
        } else if let Some(type_params) = &self.type_params {
            write!(f, "{}", type_params)?;
        }

        Ok(())
    }
}
