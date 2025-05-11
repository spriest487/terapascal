use crate::ast;
use crate::ast::IdentPath;
use crate::typ::ast::apply_func_decl_named_ty_args;
use crate::typ::ast::infer_from_structural_ty_args;
use crate::typ::ast::try_unwrap_inferred_args;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::InterfaceDecl;
use crate::typ::ast::InterfaceMethodDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::StructDef;
use crate::typ::ast::TypedFunctionName;
use crate::typ::ast::VariantDef;
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
use common::span::Span;
use common::span::Spanned;
use std::borrow::Cow;
use std::rc::Rc;

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

pub fn specialize_struct_def<'a>(
    generic_def: &Rc<StructDef>,
    ty_args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<Rc<StructDef>> {
    let struct_ty_params = match &generic_def.name.type_params {
        None => return Ok(generic_def.clone()),
        Some(param_list) => param_list,
    };

    let specialized_name = generic_def.name.specialize(ty_args, ctx)?.into_owned();

    let implements = specialize_implements_clause(
        &generic_def.implements,
        struct_ty_params,
        ty_args
    );

    let fields: Vec<_> = generic_def
        .fields()
        .map(|generic_field| {
            let ty = generic_field.ty
                .clone()
                .apply_type_args(struct_ty_params, ty_args);

            Ok(ast::FieldDecl {
                ty,
                ..generic_field.clone()
            })
        })
        .collect::<GenericResult<_>>()?;
    
    let self_ty = Type::from_struct_type(
        specialized_name.clone(),
        generic_def.kind
    );
    
    let methods = specialize_methods(
        &self_ty,
        &generic_def.methods,
        &struct_ty_params,
        ty_args
    )?;

    Ok(Rc::new(StructDef {
        name: specialized_name,
        packed: generic_def.packed,
        implements,
        fields,
        methods,
        span: generic_def.span.clone(),
        kind: generic_def.kind,
        forward: generic_def.forward,
    }))
}

pub fn specialize_variant_def(
    variant: &VariantDef,
    args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<VariantDef> {
    let variant_ty_params = match &variant.name.type_params {
        None => return Ok(variant.clone()),
        Some(param_list) => param_list,
    };

    let parameterized_name = variant.name.specialize(args, ctx)?.into_owned();
    
    let implements = specialize_implements_clause(
        &variant.implements,
        variant_ty_params,
        args
    );

    let cases: Vec<_> = variant
        .cases
        .iter()
        .map(|case| {
            let data_ty = match &case.data_ty {
                None => None,
                Some(ty) => {
                    let ty = ty.clone().apply_type_args(variant_ty_params, args);
                    Some(ty)
                },
            };

            Ok(ast::VariantCase {
                data_ty,
                ..case.clone()
            })
        })
        .collect::<GenericResult<_>>()?;
    
    let self_ty = Type::variant(parameterized_name.clone());
    let methods = specialize_methods(
        &self_ty,
        &variant.methods,
        variant_ty_params,
        args
    )?;

    Ok(VariantDef {
        name: Rc::new(parameterized_name),
        span: variant.span().clone(),
        forward: variant.forward,
        cases,
        implements,
        methods,
    })
}

pub fn specialize_iface_def<'a>(
    generic_def: &Rc<InterfaceDecl>,
    ty_args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<Rc<InterfaceDecl>> {
    let iface_ty_params = match &generic_def.name.type_params {
        None => return Ok(generic_def.clone()),
        Some(param_list) => param_list,
    };

    let specialized_name = generic_def.name.specialize(ty_args, ctx)?.into_owned();
    
    let supers = specialize_implements_clause(
        &generic_def.supers,
        iface_ty_params,
        ty_args
    );

    let self_ty = Type::interface(specialized_name.clone());

    let methods: Vec<_> = generic_def.methods
        .iter()
        .map(|generic_method| {
            let specialized_decl = specialize_method_decl(
                &generic_def.name.full_path,
                self_ty.clone(),
                &generic_method.decl,
                iface_ty_params,
                ty_args,
            )?;

            Ok(InterfaceMethodDecl {
                decl: Rc::new(specialized_decl),
            })
        })
        .collect::<GenericResult<_>>()?;

    Ok(Rc::new(InterfaceDecl {
        name: specialized_name,
        supers,
        methods,
        span: generic_def.span.clone(),
        forward: generic_def.forward,
    }))
}

fn specialize_implements_clause(
    implements: &[Type],
    params: &TypeParamList,
    args: &TypeArgList
) -> Vec<Type> {
    implements
        .iter()
        .map(|implements_ty| {
            implements_ty
                .clone()
                .apply_type_args(params, args)
        })
        .collect()
}

fn specialize_methods(
    self_ty: &Type,
    generic_methods: &[MethodDecl],
    ty_params: &TypeParamList,
    ty_args: &TypeArgList,
) -> GenericResult<Vec<MethodDecl>> {
    let self_ty_name = self_ty
        .full_path()
        .expect("must be a named type");

    let methods = generic_methods
        .iter()
        .map(|generic_method| {
            let specialized_decl = specialize_method_decl(
                self_ty_name.as_ref(),
                self_ty.clone(),
                &generic_method.func_decl,
                ty_params,
                ty_args,
            )?;

            Ok(MethodDecl {
                access: generic_method.access,
                func_decl: Rc::new(specialized_decl),
            })
        })
        .collect::<GenericResult<_>>()?;

    Ok(methods)
}

fn specialize_method_decl(
    owning_ty_generic_name: &IdentPath,
    self_ty: Type,
    generic_method: &FunctionDecl,
    struct_ty_params: &TypeParamList,
    ty_args: &TypeArgList
) -> GenericResult<FunctionDecl> {
    let mut method = apply_func_decl_named_ty_args(
        generic_method.clone(),
        struct_ty_params,
        ty_args
    );

    // specialize the owning type of all methods
    method.name = TypedFunctionName {
        ident: method.name.ident.clone(),
        span: method.name.span.clone(),
        owning_ty: match &method.name.owning_ty {
            Some(ty) => {
                assert_eq!(
                    ty.full_path().map(Cow::into_owned).as_ref(),
                    Some(owning_ty_generic_name),
                    "owning type of a method must always be the type it's declared in"
                );

                Some(self_ty)
            },

            None => None,
        },
    };

    Ok(method)
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
    infer_from_structural_ty_args(&generic_sig.return_ty, expect_return_ty, &mut inferred_ty_args, span);
    
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
