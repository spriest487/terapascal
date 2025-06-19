use crate::ast;
use crate::ast::TypeMemberDecl;
use crate::typ::ast::{infer_from_structural_ty_args, MethodDeclSection, SupersClause};
use crate::typ::ast::try_unwrap_inferred_args;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDeclContext;
use crate::typ::ast::InterfaceDecl;
use crate::typ::ast::InterfaceMethodDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::StructDecl;
use crate::typ::ast::VariantDecl;
use crate::typ::ast::WhereClause;
use crate::typ::ast::{apply_func_decl_named_ty_args, FieldDecl, StructDeclSection};
use crate::typ::FunctionSig;
use crate::typ::GenericContext;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use crate::typ::Context;
use std::borrow::Cow;
use std::sync::Arc;
use terapascal_common::span::Span;

pub trait Specializable {
    type GenericID: PartialEq + Clone;

    fn is_unspecialized_generic(&self) -> bool;
    fn name(&self) -> Cow<Self::GenericID>;

    fn infer_specialized_from_hint<'a, 'b>(&'a self, hint: &'a Self) -> Option<&'b Self>
    where 'a: 'b
    {
        if self.is_unspecialized_generic() {
            let is_specialization = self.is_unspecialized_generic()
                && !hint.is_unspecialized_generic()
                && self.name() == hint.name();

            if is_specialization {
                Some(hint)
            } else {
                None
            }
        } else {
            Some(self)
        }
    }

    fn apply_type_args(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self;
}

fn specialise_where_clause(generic_where: Option<&WhereClause>, generic_ctx: &GenericContext) -> Option<WhereClause> {
    generic_where
        .cloned()
        .map(|where_clause| {
            where_clause.apply_generics(&generic_ctx)
        })
}

pub fn specialize_struct_def<'a>(
    generic_def: &Arc<StructDecl>,
    ty_args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<Arc<StructDecl>> {
    let struct_ty_params = match &generic_def.name.type_params {
        None => return Ok(generic_def.clone()),
        Some(param_list) => param_list,
    };
    
    let specialized_name = generic_def.name.specialize(ty_args, ctx)?.into_owned();

    let inner_generic_ctx = GenericContext::new(&struct_ty_params, &ty_args);

    let specialized_where = specialise_where_clause(
        generic_def.where_clause.as_ref(),
        &inner_generic_ctx
    );

    let implements = match &generic_def.implements {
        Some(implements) => Some(apply_implements_ty_args(implements, &inner_generic_ctx)),
        None => None,
    };

    let self_ty = Type::from_struct_type(
        specialized_name.clone(),
        generic_def.kind
    );
    
    let mut sections = Vec::with_capacity(generic_def.sections.len());

    for generic_section in &generic_def.sections {
        let mut section = StructDeclSection {
            access: generic_section.access,
            access_kw_span: generic_section.access_kw_span.clone(),
            members: Vec::with_capacity(generic_section.members.len()),
        };

        for generic_member in &generic_section.members {
            let member = match generic_member {
                TypeMemberDecl::Field(generic_field) => {
                    let ty = generic_field.ty
                        .clone()
                        .apply_type_args(struct_ty_params, ty_args);
                    
                    TypeMemberDecl::Field(FieldDecl {
                        ty,
                        ..generic_field.clone()
                    })
                }

                TypeMemberDecl::Method(generic_method) => {
                    let specialized_decl = specialize_method_decl(
                        self_ty.clone(),
                        &generic_method.func_decl,
                        struct_ty_params,
                        ty_args,
                    );

                    TypeMemberDecl::Method(MethodDecl {
                        access: generic_method.access,
                        func_decl: Arc::new(specialized_decl),
                    })
                }
            };

            section.members.push(member);
        }
        
        sections.push(section);
    }

    Ok(Arc::new(StructDecl {
        kw_span: generic_def.kw_span.clone(),
        name: specialized_name,
        where_clause: specialized_where,
        tags: generic_def.tags.clone(),
        packed: generic_def.packed,
        implements,
        sections,
        span: generic_def.span.clone(),
        kind: generic_def.kind,
        forward: generic_def.forward,
        end_kw_span: generic_def.end_kw_span.clone(),
    }))
}

fn specialize_method_section(
    generic_section: &MethodDeclSection,
    self_ty: &Type,
    ty_params: &TypeParamList,
    ty_args: &TypeArgList
) -> MethodDeclSection {
    let mut section = MethodDeclSection {
        access: generic_section.access,
        access_kw_span: generic_section.access_kw_span.clone(),
        methods: Vec::with_capacity(generic_section.methods.len()),
    };
    
    for generic_method in &generic_section.methods {
        let specialized_decl = specialize_method_decl(
            self_ty.clone(),
            &generic_method.func_decl,
            ty_params,
            ty_args,
        );

        section.methods.push(MethodDecl {
            access: generic_method.access,
            func_decl: Arc::new(specialized_decl),
        });
    }
    
    section
}

pub fn specialize_variant_def(
    generic_def: &VariantDecl,
    args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<VariantDecl> {
    let variant_ty_params = match &generic_def.name.type_params {
        None => return Ok(generic_def.clone()),
        Some(param_list) => param_list,
    };

    let parameterized_name = generic_def.name.specialize(args, ctx)?.into_owned();

    let inner_generic_ctx = GenericContext::new(&variant_ty_params, &args);

    let implements = match &generic_def.implements {
        Some(implements) => Some(apply_implements_ty_args(implements, &inner_generic_ctx)),
        None => None,
    };

    let specialized_where = specialise_where_clause(
        generic_def.where_clause.as_ref(),
        &inner_generic_ctx
    );

    let cases: Vec<_> = generic_def
        .cases
        .iter()
        .map(|case| {
            let data = match &case.data {
                None => None,
                Some(data) => {
                    Some(ast::VariantCaseData {
                        ty: data.ty.clone().apply_type_args(variant_ty_params, args),
                        span: data.span.clone(),
                    })
                },
            };

            Ok(ast::VariantCase {
                data,
                ..case.clone()
            })
        })
        .collect::<GenericResult<_>>()?;

    let self_ty = Type::variant(parameterized_name.clone());
    
    let sections = generic_def.sections
        .iter()
        .map(|section| {
            specialize_method_section(section, &self_ty, variant_ty_params, args)
        })
        .collect();

    Ok(VariantDecl {
        kw_span: generic_def.kw_span.clone(),
        name: Arc::new(parameterized_name),
        where_clause: specialized_where,
        tags: generic_def.tags.clone(),
        span: generic_def.span.clone(),
        forward: generic_def.forward,
        cases,
        implements,
        sections,
        end_kw_span: generic_def.end_kw_span.clone(),
    })
}

pub fn specialize_iface_def<'a>(
    generic_def: &Arc<InterfaceDecl>,
    ty_args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<Arc<InterfaceDecl>> {
    let iface_ty_params = match &generic_def.name.type_params {
        None => return Ok(generic_def.clone()),
        Some(param_list) => param_list,
    };

    let specialized_name = generic_def.name.specialize(ty_args, ctx)?.into_owned();

    let inner_generic_ctx = GenericContext::new(&iface_ty_params, &ty_args);
    
    let specialized_where = specialise_where_clause(
        generic_def.where_clause.as_ref(),
        &inner_generic_ctx
    );
    
    let supers = match &generic_def.supers {
        Some(supers) => Some(apply_implements_ty_args(supers, &inner_generic_ctx)),
        None => None,
    };

    let self_ty = Type::interface(specialized_name.clone());

    let methods: Vec<_> = generic_def.methods
        .iter()
        .map(|generic_method| {
            let specialized_decl = specialize_method_decl(
                self_ty.clone(),
                &generic_method.decl,
                iface_ty_params,
                ty_args,
            );

            InterfaceMethodDecl {
                decl: Arc::new(specialized_decl),
            }
        })
        .collect();

    Ok(Arc::new(InterfaceDecl {
        name: specialized_name,
        where_clause: specialized_where,
        tags: generic_def.tags.clone(),
        supers,
        methods,
        forward: generic_def.forward,
        span: generic_def.span.clone(),
        kw_span: generic_def.kw_span.clone(),
        end_kw_span: generic_def.end_kw_span.clone(),
    }))
}

fn apply_implements_ty_args(
    implements: &SupersClause,
    generic_ctx: &GenericContext,
) -> SupersClause {
    let types = implements.types
        .iter()
        .map(|implements_ty| {
            implements_ty.clone().map(|ty| generic_ctx.apply_to_type(ty))
        })
        .collect();
    
    SupersClause {
        types,
        span: implements.span.clone(),
        kw_span: implements.kw_span.clone(),
    }
}

pub fn specialize_method_decl(
    self_ty: Type,
    generic_method: &FunctionDecl,
    struct_ty_params: &TypeParamList,
    ty_args: &TypeArgList
) -> FunctionDecl {
    let mut method = apply_func_decl_named_ty_args(
        generic_method.clone(),
        struct_ty_params,
        ty_args
    );

    // specialize the owning type of all methods
    method.name.context = FunctionDeclContext::MethodDecl { 
        enclosing_type: self_ty,
    };

    method
}

/// if the symbol isn't already specialized, try to infer it by matching the return type of
/// one of its methods to the expected return type
pub fn specialize_by_return_ty<'a>(
    name: &'a Symbol,
    generic_sig: &FunctionSig,
    expect_return_ty: &Type,
    span: &Span,
    ctx: &Context
) -> GenericResult<Cow<'a, Symbol>> {
    if !name.is_unspecialized_generic() {
        return Ok(Cow::Borrowed(name));
    }

    let ty_params = name.type_params.as_ref().unwrap();

    let mut inferred_ty_args = GenericContext::empty();
    infer_from_structural_ty_args(&generic_sig.result_ty, expect_return_ty, &mut inferred_ty_args, span);
    
    let ty_args = try_unwrap_inferred_args(&ty_params, inferred_ty_args, ctx, span)
        .ok_or_else(|| {
            GenericError::CannotInferArgs {
                target: GenericTarget::FunctionSig(generic_sig.clone()),
                hint: GenericTypeHint::ExpectedReturnType(expect_return_ty.clone()),
            }
        })?;

    let specialized = name
        .clone()
        .with_ty_args(Some(ty_args));

    Ok(Cow::Owned(specialized))
}
