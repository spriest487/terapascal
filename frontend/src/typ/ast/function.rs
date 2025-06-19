#[cfg(test)]
mod test;
mod decl_mod;

pub use self::decl_mod::*;
use crate::ast;
use crate::ast::Ident;
use crate::ast::SemanticHint;
use crate::ast::TypeAnnotation;
use crate::ast::FunctionDeclKind;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::typecheck_block;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::where_clause::WhereClause;
use crate::typ::ast::Tag;
use crate::typ::{typecheck_type, typecheck_typename, TypeName};
use crate::typ::typecheck_type_params;
use crate::typ::typecheck_type_path;
use crate::typ::validate_generic_constraints;
use crate::typ::Binding;
use crate::typ::ClosureBodyEnvironment;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::FunctionBodyEnvironment;
use crate::typ::FunctionSig;
use crate::typ::FunctionSigParam;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::InvalidOverloadKind;
use crate::typ::MismatchedMethodDecl;
use crate::typ::NameError;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeError;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::ValueKind;
use derivative::Derivative;
use linked_hash_map::LinkedHashMap;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub const SELF_PARAM_NAME: &str = "self";
pub const SELF_TY_NAME: &str = "Self";

pub type FunctionDecl = ast::FunctionDecl<Value>;
pub type FunctionDef = ast::FunctionDef<Value>;
pub type FunctionParam = ast::FunctionParam<Value>;
pub type InterfaceMethodDecl = ast::InterfaceMethodDecl<Value>;
pub type AnonymousFunctionDef = ast::AnonymousFunctionDef<Value>;
pub type FunctionLocalBinding = ast::FunctionLocalBinding<Value>;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub enum FunctionDeclContext {
    FreeFunction,
    MethodDecl {
        enclosing_type: Type,
    },
    MethodDef {
        declaring_type: Type,

        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        ty_name_span: Span,

        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        ty_param_spans: Vec<Span>,
    },
}

impl FunctionDeclContext {
    pub fn method_decl(enclosing_type: Type) -> Self {
        FunctionDeclContext::MethodDecl { enclosing_type }
    }
    
    pub fn method_def<Param: Spanned>(
        ty: impl Into<Type>,
        src_name: &ast::IdentPath,
        src_params: Option<&ast::TypeList<Param>>,
    ) -> Self {
        FunctionDeclContext::MethodDef {
            declaring_type: ty.into(),
            ty_name_span: src_name.path_span(),
            ty_param_spans: src_params
                .as_ref()
                .map(|list| list.items
                    .iter()
                    .map(|param| param.span().clone())
                    .collect()
                )
                .unwrap_or_else(Vec::new),
        }
    }

    pub fn method_declaring_type(&self) -> Option<&Type> {
        match self {
            FunctionDeclContext::MethodDef { declaring_type, ..  } => Some(declaring_type),
            FunctionDeclContext::MethodDecl { enclosing_type, ..  } => Some(enclosing_type),
            _ => None,
        }
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct FunctionName {
    pub ident: Ident,
    pub type_params: Option<TypeParamList>,
    
    pub context: FunctionDeclContext,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl FunctionName {
    pub fn new_method_decl(
        ident: impl Into<Ident>,
        type_params: Option<TypeParamList>,
        enclosing_type: Type,
        span: impl Into<Span>
    ) -> Self {
        Self {
            ident: ident.into(),
            type_params,
            span: span.into(),
            context: FunctionDeclContext::MethodDecl {
                enclosing_type,
            },
        }
    }
    
    pub fn to_debug_string(&self, type_args: Option<&TypeArgList>) -> String {
        let mut string = String::new();

        if let FunctionDeclContext::MethodDef { declaring_type: self_ty, .. } = &self.context {
            string.push_str(&format!("{}.", self_ty));
        }

        string.push_str(&self.ident.as_str());
        
        match type_args {
            None => {
                if let Some(ty_list) = &self.type_params {
                    string.push_str(&ty_list.to_string());
                }
            }
            Some(args) => {
                string.push_str(&args.to_string());
            }
        }
        
        string
    }
}

impl ast::FunctionName for FunctionName {    
    fn ident(&self) -> &Ident {
        &self.ident
    }

    fn owning_type_name_semantic_hint(&self) -> SemanticHint {
        match &self.context {
            FunctionDeclContext::FreeFunction => SemanticHint::None,
            FunctionDeclContext::MethodDecl { enclosing_type, .. } => enclosing_type.semantic_hint(),
            FunctionDeclContext::MethodDef { declaring_type, .. } => declaring_type.semantic_hint(),
        }
    }

    fn owning_type_name_span(&self) -> Option<&Span> {
        match &self.context {
            FunctionDeclContext::MethodDef { ty_name_span, ..  } => Some(ty_name_span),
            _ => None,
        }
    }

    fn owning_type_params_len(&self) -> usize {
        match &self.context {
            FunctionDeclContext::MethodDef { ty_param_spans, ..  } => ty_param_spans.len(),
            _ => 0,
        }
    }

    fn owning_type_param_span(&self, index: usize) -> &Span {
        match &self.context {
            FunctionDeclContext::MethodDef { ty_param_spans, ..  } => &ty_param_spans[index],
            _ => panic!("function name `{}` does not have type params", self),
        }
    }

    fn type_params_len(&self) -> usize {
        self.type_params
            .as_ref()
            .map(|list| list.len())
            .unwrap_or(0)
    }

    fn type_param_span(&self, index: usize) -> &Span {
        let params = self.type_params.as_ref().unwrap();
        params.items[index].name.span()
    }
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_debug_string(None))
    }
}

impl FunctionDecl {
    pub fn typecheck(decl: &ast::FunctionDecl, is_def: bool, ctx: &mut Context) -> TypeResult<Self> {
        let tags = Tag::typecheck_tags(&decl.tags, ctx)?;
        
        let enclosing_ty = ctx.current_enclosing_ty().cloned();
        
        let decl_context = match (&decl.name.owning_ty_qual, &enclosing_ty) {
            (Some(type_qual), Some(..)) => {
                return Err(TypeError::InvalidMethodOwningType {
                    method_ident: decl.name.ident.clone(),
                    span: type_qual.span().clone()
                });
            }

            (None, None) if is_def && decl.kind.must_be_method() => {
                return Err(TypeError::MethodDeclMissingType {
                    span: decl.name.span().clone(),
                    ident: decl.name.ident.clone(),
                    kind: decl.kind,
                })
            }

            (Some(owning_ty_name), None) => {
                let ty = typecheck_type_path(owning_ty_name, ctx)?;
                
                FunctionDeclContext::method_def(
                    ty.clone(),
                    &owning_ty_name.name,
                    owning_ty_name.type_params.as_ref()
                )
            },

            (None, Some(enclosing_ty)) => {
                FunctionDeclContext::MethodDecl {
                    enclosing_type: enclosing_ty.clone(),
                }
            },

            _ => FunctionDeclContext::FreeFunction,
        };

        let is_method = decl_context.method_declaring_type().is_some();

        let env = Environment::FunctionDecl {
            owning_ty_params: decl_context
                .method_declaring_type()
                .as_ref()
                .and_then(|ty| ty.type_params().cloned()),
        };

        ctx.scope(env, |ctx| {
            // declare type params from the declaring type, if any
            // e.g. for method `MyClass[T].A()`, `T` is declared here for the function scope
            // if this decl is inside an enclosing type, they should already be declared in the body
            // scope of the type, and can't be redeclared
            if let FunctionDeclContext::MethodDef { declaring_type, .. } = &decl_context {
                if let Some(params) = declaring_type.type_params() {
                    ctx.declare_type_params(params)?;
                }
            }

            let decl_mods = DeclMod::typecheck_mods(&decl, is_method, ctx)?;

            let (type_params, where_clause) = match (&decl.name.type_params, &decl.where_clause) {
                (Some(decl_type_params), None) => {
                    let unconstrained = decl_type_params.clone().to_unconstrained_params();
                    let type_params = typecheck_type_params(&unconstrained, ctx)?;

                    ctx.declare_type_params(&type_params)?;
                    (Some(type_params), None)
                },

                (Some(decl_type_params), Some(where_clause)) => {
                    // TODO: need to typecheck these in a temp scope with the unconstrained params
                    // so we can support things like where T: ISomething[T]?

                    let where_clause = WhereClause::typecheck(where_clause, ctx)?;
                    let type_params = where_clause
                        .clone()
                        .typecheck_constrained_params(decl_type_params.clone())?;
                    
                    ctx.declare_type_params(&type_params)?;

                    (Some(type_params), Some(where_clause))
                }

                (None, Some(unexpected_where)) => {
                    let err = GenericError::UnexpectedConstraintList;
                    return Err(TypeError::from_generic_err(err, unexpected_where.span.clone()));
                }

                (None, None) => {
                    (None, None)
                },
            };

            let result_ty = match decl.kind {
                FunctionDeclKind::Function | FunctionDeclKind::ClassMethod =>  match &decl.result_ty {
                    ast::TypeName::Unspecified(..) => TypeName::inferred(Type::Nothing),
                    ty_name => typecheck_typename(ty_name, ctx)?,
                },
                
                FunctionDeclKind::Destructor => {
                    TypeName::inferred(Type::Nothing)
                }

                FunctionDeclKind::Constructor => {
                    assert!(
                        !decl.result_ty.is_known(),
                        "parser must not produce constructors with explicit return types"
                    );

                    let ctor_owning_ty = decl_context
                        .method_declaring_type()
                        .expect("owning type must not be null for constructors");

                    // the return type of methods in generic types has to be specialized 
                    // with the type's own params - this normally happens automatically because we
                    // declare the params before typechecking the return type, but for constructors
                    // the return type is implied so we have to grab the non-specialized version from
                    // earlier and specialize it manually
                    let mut ctor_return_ty = ctor_owning_ty.clone();
                    if let Some(owning_ty_params) = ctor_owning_ty.type_params() {
                        let own_ty_args = owning_ty_params.clone().into_type_args();

                        // specialize, not apply, because this is the declaring type of the ty params 
                        ctor_return_ty = ctor_return_ty
                            .specialize(&own_ty_args, ctx)
                            .map_err(|e| TypeError::from_generic_err(e, decl.span.clone()))?
                            .into_owned();
                    }
                    TypeName::inferred(ctor_return_ty)
                }
            };

            let params: Vec<FunctionParam>;

            match &decl_context {
                FunctionDeclContext::FreeFunction => {
                    params = typecheck_params(decl, None, ctx)?;
                },

                // method definition
                FunctionDeclContext::MethodDef { declaring_type, .. } => {
                    let explicit_ty_span = decl
                        .name
                        .owning_ty_qual
                        .as_ref()
                        .unwrap()
                        .span();

                    // in a method definition, Self is a known type and should already be translated 
                    // as the actual type itself, parameterized by its own type params.
                    let self_arg_ty = if !decl.kind.is_static_method() {
                        Some(specialize_self_ty(
                            declaring_type.clone(),
                            explicit_ty_span,
                            ctx
                        )?)
                    } else {
                        None
                    };

                    params = typecheck_params(decl, self_arg_ty, ctx)?;

                    let param_sigs = params.iter()
                        .cloned()
                        .map(|param| FunctionSigParam::from_decl_param(param))
                        .collect();
                    let method_sig = FunctionSig::new(
                        result_ty.ty().clone(),
                        param_sigs,
                        type_params.clone());

                    match &declaring_type {
                        // can't define interface methods
                        Type::Interface(..) => {
                            return Err(TypeError::AbstractMethodDefinition {
                                span: decl.span.clone(),
                                owning_ty: declaring_type.clone(),
                                method: decl.name.ident.clone(),
                            });
                        }

                        // method definition
                        _ => {
                            validate_method_def_matches_decl(
                                &declaring_type,
                                &explicit_ty_span,
                                &decl.name.ident,
                                &method_sig,
                                decl.kind,
                                ctx,
                                decl.span(),
                            )?;
                        }
                    }
                }

                FunctionDeclContext::MethodDecl { enclosing_type } => {
                    // if the owning type is an interface, this is an interface method definition,
                    // and the type of `self` is the generic `Self` (to stand in for implementing
                    // types in static interfaces). if it's NOT an interface, it must be a concrete
                    // type, in which case the type of self is just that type parameterized by itself
                    let self_param_ty = if let Type::Interface(..) = enclosing_type {
                        Some(Type::MethodSelf)
                    } else if !decl.kind.is_static_method() {
                        let at = decl.name.ident.span();
                        Some(specialize_self_ty(enclosing_type.clone(), at, ctx)?)
                    } else {
                        None
                    };

                    params = typecheck_params(decl, self_param_ty, ctx)?;
                }
            };

            let decl = FunctionDecl {
                kw_span: decl.kw_span.clone(),
                name: FunctionName {
                    ident: decl.name.ident.clone(),
                    type_params: type_params.clone(),
                    context: decl_context,
                    span: decl.name.span(),
                },
                tags,
                kind: decl.kind,
                params,
                where_clause,
                result_ty,
                span: decl.span.clone(),
                mods: decl_mods,
            };

            Ok(decl)
        })
    }

    pub fn check_new_overload<Overload>(
        &self, 
        overloads: impl IntoIterator<Item=Overload>
    ) -> Option<InvalidOverloadKind>
    where
        Overload: Deref<Target=Self>,
    {
        if !self.is_overload() {
            return Some(InvalidOverloadKind::MissingOverloadModifier);
        }

        let new_sig = self.sig();

        // check for duplicate overloads
        for (index, overload) in overloads.into_iter().enumerate() {
            if !overload.deref().is_overload() {
                return Some(InvalidOverloadKind::MissingOverloadModifier);
            }

            let sig = overload.deref().sig();
            if sig == new_sig {
                return Some(InvalidOverloadKind::Duplicate(index));
            }
        }
        
        None
    }
    
    /// If this is a method, the type that declared this method. This is always the concrete type,
    /// not the interface the method was declared in, even if the method is virtual.
    pub fn method_declaring_type(&self) -> Option<&Type> {
        self.name.context.method_declaring_type()
    }
    
    pub fn sig(&self) -> FunctionSig {
        FunctionSig::from_decl(self.clone())
    }
}

fn specialize_self_ty(self_ty: Type, at: &Span, ctx: &Context) -> TypeResult<Type> {
    if let Some(self_ty_params) = self_ty.type_params() {
        let params_as_args = self_ty_params
            .clone()
            .map(|param, _pos| param.into_generic_param_ty());

        let ty = self_ty
            .specialize(&params_as_args, ctx)
            .map_err(|e| {
                TypeError::from_generic_err(e, at.clone())
            })?
            .into_owned();
        
        Ok(ty)
    } else {
        Ok(self_ty)
    }
}

impl FunctionDecl {
    pub fn is_implementation_of(&self, iface_ty: &Type, ctx: &Context) -> TypeResult<bool> {
        let owning_ty = match self.method_declaring_type() {
            // not a method/can't be an implementation
            None | Some(Type::Interface(..)) => return Ok(false),
            
            Some(ty) => ty,
        };

        let implements = owning_ty.implemented_ifaces_at(ctx, &self.span)?;
        if !implements.contains(iface_ty) {
            return Ok(false);
        }
        
        let sig = self.sig();

        let methods = iface_ty.methods_at(ctx, &self.span)?;
        for impl_method in methods {
            if impl_method.func_decl.name.ident == self.name.ident {
                let iface_sig = impl_method.func_decl.sig().with_self(owning_ty);

                if iface_sig == sig {
                    return Ok(true);
                }
            }
        }
        
        Ok(false)
    }
}

fn typecheck_params(
    decl: &ast::FunctionDecl,
    implicit_self: Option<Type>,
    ctx: &mut Context
) -> TypeResult<Vec<FunctionParam>> {
    let mut params = Vec::new();

    if let Some(self_ty) = implicit_self {
        params.push(FunctionParam {
            ty: self_ty,
            ty_span: None,
            name: Arc::new(SELF_PARAM_NAME.to_string()),
            is_implicit_self: true,
            modifier: None,
            name_span: None,
        });
    }
    
    for param in &decl.params {
        let find_name_dup = params
            .iter()
            .find(|p| p.name == param.name);

        if let Some(prev) = find_name_dup {
            return Err(TypeError::DuplicateNamedArg {
                name: Ident::new(&param.name, decl.name.span().clone()),
                span: param.name_span.clone()
                    .unwrap_or_else(|| {
                        decl.span.clone()
                    }),
                previous: prev.name_span.clone(),
            });
        }

        let ty = typecheck_type(&param.ty, ctx)?;

        let param = FunctionParam {
            modifier: param.modifier.clone(),
            name: param.name.clone(),
            name_span: param.name_span.clone(),
            ty,
            ty_span: param.ty_span.clone(),
            is_implicit_self: false,
        };
        params.push(param);
    }

    Ok(params)
}

fn validate_method_def_matches_decl(
    owning_ty: &Type,
    owning_ty_span: &Span,
    method_ident: &Ident,
    method_sig: &FunctionSig,
    method_kind: FunctionDeclKind,
    ctx: &Context,
    def_span: &Span,
) -> TypeResult<()> {
    let Some(owning_ty_name) = owning_ty.full_name() else {
        return Err(TypeError::InvalidMethodInstanceType {
            ty: owning_ty.clone(),
            span: owning_ty_span.clone(),
        });
    };
    
    let owning_ty = match &owning_ty_name.type_args {
        // unspecialized
        None => ctx.find_type(&owning_ty_name.full_path)
            .map_err(|err| {
                TypeError::from_name_err(err, owning_ty_span.clone())
            })?.1
            .clone(),
        
        // specialized
        Some(owning_ty_args) => Type::specialize(owning_ty, &owning_ty_args, ctx)
            .map_err(|err| {
                TypeError::from_generic_err(err, owning_ty_span.clone())
            })?
            .into_owned(),
    };

    let decl_method = owning_ty
        .methods(ctx)
        .map(|methods| {
            methods
                .into_iter()
                .filter_map(|m| if m.func_decl.ident() == method_ident {
                    Some(m.func_decl)
                } else {
                    None
                })
                .collect::<Vec<_>>()
        })
        .map_err(|err| {
            TypeError::from_name_err(err, def_span.clone())
        })?;
    
    if decl_method.is_empty() {
        return Err(TypeError::from_name_err(
            NameError::type_member_not_found(owning_ty.clone(), method_ident.clone()), 
            def_span.clone()
        ));
    }
    
    let matches = decl_method
        .iter()
        .any(|decl_method| {
            let declared_sig = decl_method.sig();

            *method_sig == declared_sig && decl_method.kind == method_kind
        });
    
    if !matches {
        return Err(TypeError::MethodDefMismatch {
            span: def_span.clone(),
            decls: decl_method
                .into_iter()
                .map(|m| MismatchedMethodDecl {
                    name: m.name.ident.clone(),
                    kind: m.kind,
                    span: m.span.clone(),
                })
                .collect(),
            method_ident: method_ident.clone(),
            actual_kind: method_kind,
            owning_ty,
        });
    }
    
    Ok(())
}

pub fn typecheck_func_def(
    mut decl: FunctionDecl,
    def: &ast::FunctionDef<Span>,
    ctx: &mut Context,
) -> TypeResult<FunctionDef> {
    // in the body of a method definition, the type parameters of the enclosing type are
    // used to specialize the types in the decl
    let owning_ty = decl
        .name.context
        .method_declaring_type()
        .cloned();

    if let Some(outer_ty_params) = owning_ty
        .as_ref()
        .and_then(|ty| ty.type_params()) 
    {
        let outer_ty_args = outer_ty_params
            .clone()
            .into_type_args();
        
        decl = apply_func_decl_named_ty_args(decl, outer_ty_params, &outer_ty_args);
    }
    
    let decl = Arc::new(decl);

    let result_ty = decl.result_ty.clone();

    let body_env = FunctionBodyEnvironment {
        result_ty: Type::from(result_ty),
        ty_params: decl.name.type_params.clone(),
        self_ty: decl.method_declaring_type().cloned(),
    };

    ctx.scope(body_env, |ctx| {
        // declare type parameters from the owning type, if this is a method
        if let Some(owning_ty) = decl.method_declaring_type() {
            if let Some(enclosing_ty_params) = owning_ty.type_params() {
                ctx.declare_type_params(enclosing_ty_params)?;
            }
        }

        // declare decl's own type params within the body too
        if let Some(decl_type_params) = decl.name.type_params.as_ref() {
            ctx.declare_type_params(&decl_type_params)?;
        }
    
        declare_func_params_in_body(&decl.params, &decl.name.span, ctx)?;

        let locals = declare_locals_in_body(&def, ctx)?;
    
        let body = typecheck_block(&def.body, &decl.result_ty, ctx)?;

        Ok(FunctionDef {
            decl,
            locals,
            body,
            span: def.span.clone(),
        })
    })
}

fn declare_locals_in_body(
    def: &ast::FunctionDef,
    ctx: &mut Context
) -> TypeResult<Vec<FunctionLocalBinding>> {
    let mut locals = Vec::new();

    for local in &def.locals {
        let ty = typecheck_type(&local.ty, ctx)?;

        let initial_val = match &local.initial_val {
            Some(expr) => {
                let expr = typecheck_expr(expr, &ty, ctx)?;
                let value = expr.const_eval(ctx).ok_or_else(|| TypeError::InvalidConstExpr {
                    expr: Box::new(expr.clone()),
                })?;

                Some(value)
            },
            None => None,
        };

        match local.kind {
            ast::BindingDeclKind::Const => {
                let val = match &initial_val {
                    Some(val) => val,
                    None => {
                        return Err(TypeError::ConstDeclWithNoValue { span: local.span.clone() });
                    },
                };

                ctx.declare_local_const(local.ident.clone(), val.clone(), ty.clone(), local.span.clone())?;
            },

            ast::BindingDeclKind::Var => {
                let binding_kind = match &initial_val {
                    Some(..) => ValueKind::Mutable,
                    None => ValueKind::Uninitialized,
                };

                ctx.declare_local_var(local.ident.clone(), Binding {
                    kind: binding_kind,
                    ty: ty.clone(),
                    def: Some(local.ident.clone()),
                    semantic_hint: SemanticHint::Variable,
                })?;
            },
        }

        locals.push(FunctionLocalBinding {
            ident: local.ident.clone(),
            kind: local.kind,
            initial_val,
            ty,
            span: local.span.clone(),
        });
    }
    
    Ok(locals)
}

fn declare_func_params_in_body(params: &[FunctionParam], default_span: &Span, ctx: &mut Context) -> TypeResult<()> {
    for param in params {
        let (kind, init) = match param.get_modifier() {
            Some(ast::FunctionParamMod::Var) => (ValueKind::Mutable, true),
            Some(ast::FunctionParamMod::Out) => (ValueKind::Uninitialized, false),
            None => (ValueKind::Mutable, false),
        };
        
        // if the param doesn't have a span itself, it's an implicit span, so just use the function
        // name as the span
        let decl_span = param.name_span
            .clone()
            .unwrap_or_else(|| default_span.clone());
        let decl_ident = Ident::new(&param.name, decl_span);

        ctx.declare_local_var(
            decl_ident.clone(),
            Binding {
                ty: param.ty.clone(),
                kind,
                def: Some(decl_ident.clone()),
                semantic_hint: SemanticHint::Parameter,
            },
        )?;

        if init {
            ctx.initialize(&decl_ident);
        }
    }

    Ok(())
}

// Specialize a generic function decl, replacing any references to its type params with
// types provided in the args list. 
// This has no effect on functions that don't declare a type parameter list and returns a clone 
// of the original decl.
pub fn specialize_func_decl(
    decl: &FunctionDecl,
    args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<FunctionDecl> {
    let ty_params = match &decl.name.type_params {
        None => return Ok(decl.clone()),
        Some(list) => list,
    };
    
    if args.len() != ty_params.len() {
        return Err(GenericError::ArgsLenMismatch {
            target: GenericTarget::FunctionSig(decl.sig()),
            expected: ty_params.len(),
            actual: args.len(),
        });
    }

    validate_generic_constraints(args, ty_params, ctx)?;
    
    let mut decl = decl.clone();
    visit_type_refs(&mut decl, |ty| {
        *ty = ty.clone().apply_type_args(ty_params, args);
        Ok(())
    })?;
    Ok(decl)
}

pub fn apply_func_decl_named_ty_args(
    mut decl: FunctionDecl,
    params: &impl TypeParamContainer,
    args: &impl TypeArgResolver
) -> FunctionDecl {
    visit_type_refs(&mut decl, |ty| -> Result<(), ()> {
        *ty = ty.clone().apply_type_args(params, args);
        Ok(())
    }).unwrap();

    decl
}

pub fn apply_func_decl_ty_args(decl: &FunctionDecl, args: &TypeArgList) -> FunctionDecl {
    match decl.name.type_params.as_ref() {
        None => decl.clone(),

        Some(ty_params) => {
            let mut decl = decl.clone();
            
            _ = visit_type_refs(&mut decl, |ty| -> Result<(), ()> {
                *ty = ty.clone().apply_type_args(ty_params, args);
                Ok(())
            });
            
            decl
        }
    }
}

fn visit_type_refs<F, E>(decl: &mut FunctionDecl, mut f: F) -> Result<(), E> 
where
    F: FnMut(&mut Type) -> Result<(), E>
{
    for param in decl.params.iter_mut() {
        f(&mut param.ty)?;
    }

    f(&mut decl.result_ty)?;
    
    Ok(())
}

pub fn typecheck_func_expr(
    src_def: &ast::AnonymousFunctionDef<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<AnonymousFunctionDef> {
    let expect_sig = expect_ty.as_func().ok();

    let mut params = Vec::new();

    for (i, param) in src_def.params.iter().enumerate() {
        let ty = if param.ty.is_known() {
            typecheck_type(&param.ty, ctx)?
        } else {
            let sig_param = expect_sig
                .and_then(|sig| sig.params.get(i));
            
            match sig_param {
                Some(expect_param) if expect_param.modifier.is_none() => {
                    expect_param.ty.clone()
                },

                _ => {
                    return Err(TypeError::UnableToInferFunctionExprType {
                        func: Box::new(src_def.clone()),
                    });
                }
            }
        };

        params.push(FunctionParam {
            modifier: param.modifier.clone(),
            name: param.name.clone(),
            name_span: param.name_span.clone(),
            ty,
            ty_span: param.ty_span.clone(),
            is_implicit_self: false,
        });
    }

    // if the return type isn't explicitly specified, we might be able to infer it to aid
    // in typechecking the body if we have an expected function signature
    let known_return_ty = match &src_def.return_ty {
        ast::TypeName::Unspecified(..) => expect_sig.map(|sig| sig.result_ty.clone()),
        src_return_ty => Some(typecheck_type(src_return_ty, ctx)?),
    };

    let sig_params = params
        .iter()
        .map(|p|{
            FunctionSigParam::from_decl_param(p.clone())
        })
        .collect();

    // we manage the scope manually here so we can retrieve this environment object after
    // the body is finished and get the final captures
    let body_scope_id = ctx.push_scope(ClosureBodyEnvironment {
        result_ty: known_return_ty.clone(),
        captures: LinkedHashMap::new(),
    });

    let body_result = declare_func_params_in_body(&params, &src_def.annotation, ctx)
        .and_then(|_| {
            let expect_block_return = known_return_ty
                .as_ref()
                .unwrap_or(&Type::Nothing);

            typecheck_block(&src_def.body, &expect_block_return, ctx)
        });

    let closure_env = match ctx.pop_scope(body_scope_id).into_env() {
        Environment::ClosureBody(body_env) => body_env,
        _ => unreachable!(),
    };
    
    let body = body_result?;

    // use the result type here, because checking the body of the closure might have given
    // us an inferred return type too e.g. an explicit exit statement
    let return_ty = match closure_env.result_ty {
        Some(ty) => ty,
        None => body.annotation.ty().into_owned(),
    };

    if return_ty == Type::Nothing {
        assert_eq!(None, body.output);
    }

    let sig = Arc::new(FunctionSig {
        result_ty: return_ty.clone(),
        params: sig_params,
        type_params: None,
    });

    let value = TypedValue::temp(Type::Function(sig), src_def.span().clone());

    Ok(AnonymousFunctionDef {
        params,
        return_ty,
        annotation: Value::from(value),
        body,
        captures: closure_env.captures,
    })
}
