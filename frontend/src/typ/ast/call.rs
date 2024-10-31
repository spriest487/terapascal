#[cfg(test)]
mod test;
mod overload;
mod args;

use crate::ast;
use crate::ast::{Ident, Visibility};
use crate::ast::TypeList;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_object_ctor;
use crate::typ::ast::Expr;
use crate::typ::ast::ObjectCtor;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::FunctionSigParam;
use crate::typ::FunctionSig;
use crate::typ::FunctionTyped;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::MethodTyped;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::OverloadTyped;
use crate::typ::Specializable;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::UfcsTyped;
use crate::typ::ValueKind;
pub use args::*;
use common::span::Span;
use common::span::Spanned as _;
pub use overload::*;
use std::borrow::Cow;
use std::iter;
use std::rc::Rc;

pub type MethodCall = ast::MethodCall<Typed>;
pub type FunctionCall = ast::FunctionCall<Typed>;
pub type FunctionCallNoArgs = ast::FunctionCallNoArgs<Typed>;
pub type MethodCallNoArgs = ast::MethodCallNoArgs<Typed>;
pub type VariantCtorCall = ast::VariantCtorCall<Typed>;
pub type Call = ast::Call<Typed>;

impl MethodCallNoArgs {
    pub fn method(&self) -> &MethodTyped {
        match self.target.annotation() {
            Typed::Method(method) => method.as_ref(),
            other => panic!("method call target can only be a method, got: {other}"),
        }
    }
}

// it's possible that during typechecking we discover what was parsed as a call with zero args
// is actually the ctor for a class with no fields
pub enum Invocation {
    Call(Box<Call>),
    Ctor(Box<ObjectCtor>),
}

fn invalid_args(
    actual_args: Vec<Expr>,
    expected: &[FunctionSigParam],
    span: Span,
) -> TypeError {
    let expected: Vec<_> = expected
        .iter()
        .map(|p| p.ty.clone())
        .collect();

    let actual: Vec<_> = actual_args
        .into_iter()
        .map(|arg| arg.annotation().ty().into_owned())
        .collect();

    TypeError::InvalidArgs {
        expected,
        actual,
        span,
    }
}

fn build_args_for_params(
    params: &[FunctionSigParam],
    src_args: &[ast::Expr<Span>],
    self_arg: Option<&Expr>,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Vec<Expr>> {
    let mut checked_args = Vec::new();

    let rest_params = if let Some(self_arg) = self_arg {
        if self_arg.annotation().value_kind().is_none() {
            panic!("build_args_for_params: self arg {} is not a value (at {})", self_arg, span)
        }

        let self_ty = &params[0].ty;

        let self_arg = implicit_conversion(self_arg.clone(), self_ty, ctx)?;
        checked_args.push(self_arg);

        &params[1..]
    } else {
        params
    };

    // iterator of the rest of the params wrapped in Some until we run out of those, then None
    let rest_params_or_none = rest_params
        .iter()
        .map(Some)
        .chain(iter::repeat(None));

    // typ each arg (don't do conversions yet, we haven't figured out the self ty_def yet)
    for (expected_param, arg) in rest_params_or_none.zip(src_args.iter()) {
        // keep checking the provided arguments even when there aren't parameters for them,
        // so we can show a complete error message. this will also make any errors checking the 
        // args themselves take precedence over the invalid args error
        let param_expect_ty = match expected_param {
            Some(param) => &param.ty,
            None => &Type::Nothing,
        };

        let arg_expr = typecheck_expr(arg, param_expect_ty, ctx)?;
        checked_args.push(arg_expr);
    }

    // does arg count match expected arg count?
    if checked_args.len() != params.len() {
        return Err(invalid_args(checked_args, params, span.clone()));
    }

    // find the self ty_def - take the actual type of the first arg that is passed to a Self-typed param
    let mut self_ty: Option<Type> = None;
    let mut params = params.to_vec();

    for i in 0..params.len() {
        let expected = &params[i];
        if expected.ty != Type::MethodSelf {
            continue;
        }

        match &self_ty {
            // this is the first arg passed as a Self param, use this as the self ty_def from now on
            None => {
                let actual_self_ty = checked_args[i].annotation().ty().into_owned();
                params[i].ty = actual_self_ty.clone();
                self_ty = Some(actual_self_ty);
            },

            // we have already deduced a self ty_def and are using that one
            Some(actual_self_ty) => {
                params[i].ty = actual_self_ty.clone();
            },
        }
    }
    
    args::validate_args(&mut checked_args, &params, span, ctx)?;

    Ok(checked_args)
}

pub fn typecheck_type_args(
    type_args: &TypeList<ast::TypeName>,
    ctx: &mut Context,
) -> TypeResult<TypeList<Type>> {
    let items: Vec<_> = type_args
        .items
        .iter()
        .map(|arg_ty| typecheck_type(arg_ty, ctx))
        .collect::<TypeResult<_>>()?;

    Ok(TypeList::new(items, type_args.span().clone()))
}

pub fn typecheck_call(
    call: &ast::Call<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Invocation> {
    match call {
        ast::Call::Function(func_call) => {
            typecheck_func_call(&func_call, expect_ty, ctx)
        },

        ast::Call::FunctionNoArgs(no_args_call) => {
            // this will be determined from the target expression
            assert!(no_args_call.self_arg.is_none(), "parser should not provide a self-arg");
            
            let as_func_call = ast::FunctionCall {
                target: no_args_call.target.clone(),
                type_args: no_args_call.type_args.clone(),
                args: no_args_call.self_arg.iter().cloned().collect(),
                annotation: no_args_call.annotation.clone(),
                args_span: no_args_call.target.span().clone(),
            };
            
            typecheck_func_call(&as_func_call, expect_ty, ctx)
        }
        other => unreachable!("parsing should not produce this call expression: {:?}", other),
    }
}

fn typecheck_func_call(
    func_call: &ast::FunctionCall,
    expect_ty: &Type,
    ctx: &mut Context
) -> TypeResult<Invocation> {
    let target = typecheck_expr(&func_call.target, expect_ty, ctx)?;

    // if the call target is a no-args call itself and this is also a no-args call, we are just
    // applying an empty argument list to the same call, so just unwrap it here
    // e.g. if we call `procedure X;` with `X()`, `X` is already a valid call on its own
    // there are edge cases here like functions that return callable types themselves, but
    // they can be disambiguated by various means in the code
    let (target, self_arg) = if func_call.args.len() == 0 {
        match target {
            Expr::Call(call) => match *call {
                Call::FunctionNoArgs(noargs_func_call) => {
                    (noargs_func_call.target, noargs_func_call.self_arg)
                }
                Call::MethodNoArgs(noargs_method_call) => {
                    (noargs_method_call.target, noargs_method_call.self_arg)
                }
                other_call => {
                    (Expr::from(other_call), None)
                },
            },

            other => {
                (other, None)
            }
        }
    } else {
        (target, None)
    };

    let expr = match target.annotation() {
        Typed::TypedValue(val) => match &val.ty {
            Type::Function(sig) => {
                let sig = sig.clone();

                typecheck_free_func_call(target, &func_call, self_arg.as_ref(), sig, ctx)
                    .map(Box::new)
                    .map(Invocation::Call)?
            },

            _ => match target {
                // when making a function call with an empty args list, and the target is a
                // call to a no-args function, this "inner" call replaces the outer call entirely
                // since the extra arg list is redundant
                Expr::Call(inner_call)
                if func_call.args.len() == 0 && inner_call.args().len() == 0 => {
                    Invocation::Call(inner_call)
                }

                _ => {
                    return Err(TypeError::NotCallable(Box::new(target)))
                },
            },
        },

        Typed::Function(func) => {
            func.check_visible(func_call.span(), ctx)?;
            
            let sig = func.sig.clone();
            
            typecheck_free_func_call(target, &func_call, self_arg.as_ref(), sig, ctx)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        Typed::UfcsFunction(ufcs_call) => {
            let typecheck_call = typecheck_ufcs_call(
                &ufcs_call,
                &func_call.args,
                func_call.annotation.span(),
                &func_call.args_span,
                ctx,
            );

            typecheck_call.map(Box::new).map(Invocation::Call)?
        },

        Typed::Overload(overloaded) => {
            typecheck_func_overload(ctx, func_call, &target, &overloaded)
                .map(Box::new)
                .map(Invocation::Call)?
        },

        Typed::Method(method) => {
            typecheck_method_call(method, func_call, self_arg, ctx)
                .map(Box::new)
                .map(Invocation::Call)?
        }

        Typed::Type(..) => {
            if let Some(ctor) = func_call.clone().try_into_empty_object_ctor() {
                typecheck_object_ctor(&ctor, ctor.span().clone(), expect_ty, ctx)
                    .map(Box::new)
                    .map(Invocation::Ctor)?
            } else {
                return Err(TypeError::NotCallable(Box::new(target)))
            }
        },

        Typed::VariantCase(variant, ..) => {
            if let Some(args_list) = &func_call.type_args {
                return Err(TypeError::InvalidExplicitVariantCtorTypeArgs {
                    span: args_list.span.clone(),
                });
            }

            let ctor_call = typecheck_variant_ctor_call(
                &variant.variant_name,
                &variant.case,
                &func_call.args,
                func_call.span().clone(),
                expect_ty,
                ctx,
            )?;

            Invocation::Call(Box::new(ctor_call))
        }

        _ => {
            return Err(TypeError::NotCallable(Box::new(target)))
        },
    };

    Ok(expr)
}

fn typecheck_func_overload(
    ctx: &mut Context,
    func_call: &ast::FunctionCall<Span>,
    target: &Expr,
    overloaded: &OverloadTyped,
) -> TypeResult<Call> {
    let type_args = match &func_call.type_args {
        Some(args) => Some(typecheck_type_args(args, ctx)?),
        None => None,
    };

    let overload = resolve_overload(
        &overloaded.candidates,
        &func_call.args,
        type_args.as_ref(),
        overloaded.self_arg.as_ref().map(|arg| arg.as_ref()),
        &overloaded.span,
        ctx,
    )?;

    let args_span = func_call.args_span.clone();

    let call = match &overloaded.candidates[overload.selected_sig] {
        OverloadCandidate::Method {
            self_ty: iface_ty,
            ident,
            access,
            sig,
            index,
            ..
        } => {
            // we resolved the overload using the local type args earlier, but we only get an
            // index back, so we need to apply them here
            let sig = match &overload.type_args {
                Some(args) => {
                    sig.specialize_generic(args, ctx)
                        .map_err(|e| {
                            TypeError::from_generic_err(e, func_call.span().clone())
                        })?
                },
                None => (**sig).clone(),
            };
            
            let self_type = match &overloaded.self_arg {
                Some(self_arg) => self_arg.annotation().ty().into_owned(),

                None => {
                    let arg_tys: Vec<_> = overload
                        .args
                        .iter()
                        .map(|a| a.annotation().ty().into_owned())
                        .collect();

                    match sig.self_ty_from_args(&arg_tys) {
                        Some(self_ty) => self_ty.clone(),
                        None => {
                            return Err(TypeError::AmbiguousSelfType {
                                span: overloaded.span.clone(),
                                method: ident.clone(),
                                iface: iface_ty.clone(),
                            })
                        },
                    }
                },
            };

            if self_type.get_current_access(ctx) < *access {
                return Err(TypeError::TypeMemberInaccessible {
                    ty: self_type,
                    access: *access,
                    member: ident.clone(),
                    span: func_call.span().clone(),
                })
            }

            let sig = Rc::new(sig.with_self(&self_type));

            let return_annotation = TypedValue {
                span: overloaded.span.clone(),
                ty: sig.return_ty.clone(),
                decl: Some(ident.clone()),
                value_kind: ValueKind::Temporary,
            }
            .into();

            let method_call = MethodCall {
                owning_type: iface_ty.clone(),
                self_type,
                method_index: *index,
                annotation: return_annotation,
                ident: ident.clone(),
                args: overload.args,
                type_args: overload.type_args,
                func_type: Type::Function(sig.clone()),
                args_span,
            };

            ast::Call::Method(method_call)
        },

        OverloadCandidate::Function { decl_name, sig, visibility } => {
            if *visibility < Visibility::Interface 
                && !ctx.is_current_namespace_child(&decl_name.full_path) {
                return Err(TypeError::NameNotVisible {
                    name: decl_name.full_path.clone(),
                    span: func_call.span().clone(),
                });
            }
            
            let return_annotation = TypedValue {
                span: overloaded.span.clone(),
                ty: sig.return_ty.clone(),
                decl: Some(decl_name.ident().clone()),
                value_kind: ValueKind::Temporary,
            }
            .into();
            
            // the original target expr must have overload type, but we now know the function type
            // of the expression and should update it accordingly
            let mut target = target.clone();
            match target.annotation_mut() {
                annotation @ Typed::Overload(..) => {
                    *annotation = Typed::Function(Rc::new(FunctionTyped {
                        name: decl_name.clone(),
                        sig: sig.clone(),
                        span: annotation.span().clone(),
                        visibility: *visibility,
                    })) 
                },

                _ => panic!("typecheck_func_overload: target expr must be annotated as an overload"),
            }

            let func_call = FunctionCall {
                annotation: return_annotation,
                args: overload.args,
                type_args: overload.type_args.clone(),
                target: target.clone(),
                args_span,
            };

            ast::Call::Function(func_call)
        },
    };

    Ok(call)
}

pub fn check_overload_visibility(
    overload: &Overload,
    candidates: &[OverloadCandidate],
    span: &Span,
    ctx: &Context
) -> TypeResult<()> {
    match &candidates[overload.selected_sig] {
        OverloadCandidate::Function { visibility, decl_name, .. } => {
            if *visibility < Visibility::Interface
                && !ctx.is_current_namespace_child(&decl_name.full_path) {
                Err(TypeError::NameNotVisible {
                    name: decl_name.full_path.clone(),
                    span: span.clone(),
                })
            } else {
                Ok(())
            }
        }
        OverloadCandidate::Method { .. } => Ok(())
    }
}

pub fn overload_to_no_args_call(
    candidates: &[OverloadCandidate],
    overload: Overload,
    mut target: Expr,
    self_arg: Option<Expr>,
    span: &Span,
) -> Expr {
    assert_eq!(self_arg.iter().len(), overload.args.len());

    let call = match &candidates[overload.selected_sig] {
        OverloadCandidate::Function { sig, decl_name, visibility } => {
            assert_eq!(sig.type_params_len(), overload.type_args_len());

            let return_value = TypedValue {
                ty: sig.return_ty.clone(),
                span: span.clone(),
                decl: None,
                value_kind: ValueKind::Temporary,
            };

            *target.annotation_mut() = Typed::from(FunctionTyped {
                name: decl_name.clone(),
                sig: sig.clone(),
                span: target.span().clone(),
                visibility: *visibility
            });

            Call::FunctionNoArgs(FunctionCallNoArgs {
                target: Expr::from(target),
                self_arg,
                type_args: overload.type_args,
                annotation: return_value.into(),
            })
        }

        OverloadCandidate::Method { self_ty, index, sig, ident, access, .. } => {
            assert_eq!(sig.type_params_len(), overload.type_args_len());

            let return_value = TypedValue {
                ty: sig.return_ty.clone(),
                span: span.clone(),
                decl: None,
                value_kind: ValueKind::Temporary,
            };

            *target.annotation_mut() = Typed::from(MethodTyped {
                self_ty: self_ty.clone(),
                index: *index,
                name: ident.clone(),
                access: *access,
                decl_sig: sig.clone(),
                span: target.span().clone(),
            });

            Call::MethodNoArgs(MethodCallNoArgs {
                target: Expr::from(target),
                self_arg,
                type_args: overload.type_args,
                annotation: return_value.into(),
                owning_type: self_ty.clone(),
            })
        }
    };

    Expr::Call(Box::new(call))
}

/// * `iface_method` - Method to be called
/// * `func_call` - Source node of this call
/// * `with_self_arg` - If present, an expression which in a previous phase has been determined
///     to be the self-argument of this call. For example, when converting a call like `a.B` which
///     was previously read as a no-args call and is now being converted into a full call (`a.B()`),
///     the `a` reference is the self-arg, but isn't part of the argument list `()` of the outer
///     call expression, so needs to be provided separately
/// * `ctx` - current typechecking context
fn typecheck_method_call(
    method: &MethodTyped,
    func_call: &ast::FunctionCall<Span>,
    with_self_arg: Option<Expr>,
    ctx: &mut Context,
) -> TypeResult<Call> {
    if method.self_ty.get_current_access(ctx) < method.access {
        return Err(TypeError::TypeMemberInaccessible {
            ty: method.self_ty.clone(),
            access: method.access,
            member: method.name.clone(),
            span: func_call.span().clone(),
        })
    }

    // not yet supported
    if let Some(call_type_args) = &func_call.type_args {
        return Err(TypeError::from_generic_err(
            GenericError::ArgsLenMismatch {
                expected: 0,
                actual: call_type_args.len(),
                target: GenericTarget::FunctionSig((*method.decl_sig).clone()),
            },
            call_type_args.span().clone(),
        ));
    }

    // branch the context to check the self-arg, because we're about to re-check it in a second
    let self_type = match &with_self_arg {
        Some(self_arg) => self_arg.annotation().ty(),

        None => {
            let mut ctx = ctx.clone();
            let first_self_pos = method
                .decl_sig
                .params
                .iter()
                .position(|param| param.ty == Type::MethodSelf);
            
            match first_self_pos {
                Some(index) => {
                    // note that this isn't the "self arg" used for typecheck_args, because we're not passing
                    // it implicitly as a separate arg (like the self-arg `x` of `x.Y()` in a UFCS call).
                    // it's just the first arg from which we can infer the self-type
                    let first_self_arg = typecheck_expr(
                        &func_call.args[index],
                        &Type::Nothing,
                        &mut ctx
                    )?;

                    Cow::Owned(first_self_arg.annotation().ty().into_owned())
                }
                
                None => Cow::Owned(Type::Nothing),
            }
        }
    };
    
    if let Type::Interface(..) = method.self_ty {
        let is_impl = ctx
            .is_implementation(self_type.as_ref(), &method.self_ty)
            .map_err(|err| TypeError::from_name_err(err, func_call.span().clone()))?;

        if !is_impl {
            return Err(TypeError::from_name_err(NameError::NoImplementationFound {
                owning_ty: method.self_ty.clone(),
                impl_ty: self_type.into_owned(),
            }, func_call.span().clone()));
        }
    } else if let Some(..) = &with_self_arg {
        if method.self_ty != *self_type {
            return Err(TypeError::TypeMismatch {
                span: func_call.span().clone(),
                expected: method.self_ty.clone(),
                actual: self_type.into_owned(),
            });
        }
    }

    let sig = method.decl_sig.with_self(&self_type);

    let typechecked_args = build_args_for_params(
        &sig.params, 
        &func_call.args, 
        with_self_arg.as_ref(), 
        func_call.span(), ctx
    )?;
    
    Ok(Call::Method(MethodCall {
        annotation: TypedValue {
            ty: sig.return_ty.clone(),
            span: func_call.span().clone(),
            value_kind: ValueKind::Temporary,
            decl: None,
        }
        .into(),
        method_index: method.index,
        args_span: func_call.args_span.clone(),
        func_type: Type::Function(Rc::new(sig)),
        self_type: self_type.into_owned(),
        owning_type: method.self_ty.clone(),
        ident: method.name.clone(),
        type_args: None,
        args: typechecked_args,
    }))
}

fn typecheck_ufcs_call(
    ufcs_call: &UfcsTyped,
    rest_args: &[ast::Expr<Span>],
    span: &Span,
    args_span: &Span,
    ctx: &mut Context,
) -> TypeResult<Call> {
    if ufcs_call.visibility < Visibility::Interface
        && !ctx.is_current_namespace_child(&ufcs_call.function_name.full_path) {
        return Err(TypeError::NameNotVisible {
            name: ufcs_call.function_name.full_path.clone(),
            span: span.clone(),
        });
    }
    
    let mut specialized_call_args = args::specialize_call_args(
        &ufcs_call.sig,
        &rest_args,
        Some(&ufcs_call.self_arg),
        None, // ufcs call can't have explicit type args
        &span,
        ctx,
    )?;
    args::validate_args(
        &mut specialized_call_args.actual_args,
        &specialized_call_args.sig.params,
        span,
        ctx,
    )?;
    
    let func_sym = ufcs_call.function_name
        .clone()
        .with_ty_args(specialized_call_args.type_args.clone());

    let func_annotation = FunctionTyped {
        name: func_sym,
        span: span.clone(),
        sig: Rc::new(specialized_call_args.sig.clone()),
        visibility: ufcs_call.visibility,
    }
    .into();

    // todo: this should construct a fully qualified path expr instead
    let target = ast::Expr::Ident(ufcs_call.function_name.ident().clone(), func_annotation);

    let annotation = TypedValue {
        ty: specialized_call_args.sig.return_ty.clone(),
        span: span.clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
    }
    .into();

    Ok(ast::Call::Function(FunctionCall {
        args: specialized_call_args.actual_args,
        args_span: args_span.clone(),
        annotation,
        type_args: specialized_call_args.type_args,
        target,
    }))
}

fn typecheck_free_func_call(
    mut target: Expr,
    func_call: &ast::FunctionCall<Span>,
    self_arg: Option<&Expr>,
    sig: Rc<FunctionSig>,
    ctx: &mut Context,
) -> TypeResult<Call> {    
    let span = func_call.span().clone();

    let type_args = match func_call.type_args.as_ref() {
        Some(call_type_args) => Some(typecheck_type_args(call_type_args, ctx)?),
        None => None,
    };

    let mut specialized_call_args = specialize_call_args(
        sig.as_ref(),
        &func_call.args,
        self_arg,
        type_args,
        &span,
        ctx
    )?;

    validate_args(
        &mut specialized_call_args.actual_args,
        &specialized_call_args.sig.params,
        func_call.span(),
        ctx,
    )?;

    let return_ty = specialized_call_args.sig.return_ty.clone();

    let annotation = match return_ty {
        Type::Nothing => Typed::Untyped(span),
        return_ty => TypedValue {
            ty: return_ty,
            value_kind: ValueKind::Temporary,
            span,
            decl: None,
        }
        .into(),
    };
    
    if let Some(ty_args) = &specialized_call_args.type_args {
        match target.annotation_mut() {
            Typed::Function(func_type) => {
                func_type.check_visible(func_call.span(), ctx)?;
                
                let specialized_func_type = FunctionTyped {
                    name: (**func_type).clone().name.with_ty_args(Some(ty_args.clone())),
                    sig,
                    span: func_type.span.clone(),
                    visibility: func_type.visibility,
                };
                
                *func_type = Rc::new(specialized_func_type);
            }
            _ => panic!("typecheck_func_call: target did not have function type"),
        }
    }

    Ok(ast::Call::Function(FunctionCall {
        annotation,
        args: specialized_call_args.actual_args,
        type_args: specialized_call_args.type_args,
        target,
        args_span: func_call.args_span.clone(),
    }))
}

fn typecheck_variant_ctor_call(
    variant: &Symbol,
    case: &Ident,
    args: &[ast::Expr<Span>],
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Call> {
    if !ctx.is_visible(&variant.full_path) {
        return Err(TypeError::NameNotVisible {
            name: variant.full_path.clone(),
            span: span.clone(),
        });
    }
    
    let unspecialized_def = ctx
        .find_variant_def(&variant.full_path)
        .cloned()
        .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

    // infer the specialized generic type if the written one is generic and the hint is a specialized
    // version of that same generic variant
    let variant_sym = match expect_ty {
        Type::Variant(expect_variant)
            if expect_variant.full_path == unspecialized_def.name.full_path =>
        {
            (*expect_variant).as_ref()
        },

        _ => &unspecialized_def.name,
    };

    if variant_sym.is_unspecialized_generic() {
        return Err(TypeError::from_generic_err(
            GenericError::CannotInferArgs {
                target: GenericTarget::Name(variant_sym.full_path.clone()),
                hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
            },
            span,
        ));
    }

    let variant_def = ctx
        .instantiate_variant_def(&variant_sym)
        .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

    let case_index = match variant_def.case_position(case) {
        Some(index) => index,

        None => {
            return Err(TypeError::from_name_err(
                NameError::MemberNotFound {
                    member: case.clone(),
                    base: NameContainer::Type(Type::variant(variant_sym.clone())),
                },
                span,
            ));
        },
    };

    let arg = match &variant_def.cases[case_index].data_ty {
        None => {
            if !args.is_empty() {
                let bad_args: Vec<_> = args
                    .iter()
                    .map(|arg| {
                        typecheck_expr(arg, &Type::Nothing, ctx)
                            .map(|arg| arg.annotation().ty().into_owned())
                    })
                    .collect::<TypeResult<_>>()?;

                return Err(TypeError::InvalidArgs {
                    expected: Vec::new(),
                    actual: bad_args,
                    span,
                });
            }

            None
        },

        Some(data_ty) => {
            let args: Vec<Expr> = args
                .iter()
                .map(|arg| typecheck_expr(arg, data_ty, ctx))
                .collect::<TypeResult<_>>()?;

            if args.len() != 1 {
                let bad_args: Vec<_> = args
                    .into_iter()
                    .map(|arg| arg.annotation().ty().into_owned())
                    .collect();

                return Err(TypeError::InvalidArgs {
                    expected: Vec::new(),
                    actual: bad_args,
                    span,
                });
            }

            args[0].annotation().expect_value(data_ty)?;

            Some(args.into_iter().next().unwrap())
        },
    };

    let case = variant_def.cases[case_index].ident.clone();

    let annotation = TypedValue {
        decl: None,
        span,
        ty: Type::Variant(variant_sym.clone().into()),
        value_kind: ValueKind::Temporary,
    }
    .into();

    Ok(ast::Call::VariantCtor(ast::VariantCtorCall {
        variant: variant_def.name.clone(),
        annotation,
        arg,
        case,
    }))
}

