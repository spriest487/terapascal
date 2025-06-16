#[cfg(test)]
mod test;
mod overload;
mod args;

use crate::ast;
use crate::ast::Visibility;
use crate::typ::annotation::UfcsValue;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::function::FunctionValue;
use crate::typ::method::MethodValue;
use crate::typ::overload::OverloadValue;
use crate::typ::typecheck_typename;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::FunctionSigParam;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::Invocation;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeError;
use crate::typ::TypeName;
use crate::typ::TypeResult;
use crate::typ::Value;
pub use args::*;
pub use overload::*;
use std::iter;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned as _;

pub type MethodCall = ast::MethodCall<Value>;
pub type FunctionCall = ast::FunctionCall<Value>;
pub type FunctionCallNoArgs = ast::FunctionCallNoArgs<Value>;
pub type MethodCallNoArgs = ast::MethodCallNoArgs<Value>;
pub type VariantCtorCall = ast::VariantCtorCall<Value>;
pub type Call = ast::Call<Value>;

impl MethodCallNoArgs {
    pub fn method(&self) -> &MethodValue {
        match self.target.annotation() {
            Value::Method(method) => method.as_ref(),
            other => panic!("method call target can only be a method, got: {other}"),
        }
    }
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

pub(crate) fn build_args_for_params(
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

    // typecheck each arg (don't do conversions yet, we haven't figured out the self type yet)
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
        
        if expected.ty == Type::MethodSelf {
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
    }
    
    validate_args(&mut checked_args, &params, span, ctx)?;

    Ok(checked_args)
}

pub fn typecheck_type_args(
    type_args: &ast::TypeList<ast::TypeName>,
    ctx: &mut Context,
) -> TypeResult<TypeArgList> {
    let items: Vec<_> = type_args
        .items
        .iter()
        .map(|arg_ty| typecheck_typename(arg_ty, ctx))
        .collect::<TypeResult<_>>()?;

    Ok(TypeArgList::new(items, type_args.span().clone()))
}

pub fn typecheck_call(
    call: &ast::Call<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Call> {
    let func_call = match call {
        ast::Call::Function(func_call) => {
            typecheck_func_call(&func_call, expect_ty, ctx)?
        },
    };
    
    Ok(Call::Function(func_call))
}

fn typecheck_func_call(
    func_call: &ast::FunctionCall,
    expect_ty: &Type,
    ctx: &mut Context
) -> TypeResult<FunctionCall> {
    let target = typecheck_expr(&func_call.target, expect_ty, ctx)?;

    // eprintln!("typechecking `{}`: target is {}", func_call, target.annotation());
    // if let Some(args) = &func_call.type_args {
    //     eprintln!("\ttype args: {}", args);
    // }

    let invocation = match target.annotation() {
        // value with function type (eg lambda, reference to function)
        Value::Typed(..) | Value::Invocation(..) => match target.annotation().ty().as_ref() {
            Type::Function(sig) => {
                let sig = sig.clone();

                typecheck_func_value_invocation(target.clone(), &func_call, None, &sig, ctx)
                    .map(Arc::new)?
            },

            _ => {
                return Err(TypeError::NotCallable(Box::new(target.annotation().clone())))
            },
        },

        // direct reference to function
        Value::Function(func) => {
            let span = func_call.span().clone();

            let type_args = match func_call.type_args.as_ref() {
                Some(call_type_args) => Some(typecheck_type_args(call_type_args, ctx)?),
                None => None,
            };

            let specialized_call_args = specialize_call_args(
                &func.decl,
                &func_call.args,
                None,
                type_args,
                &span,
                ctx
            )?;
            
            let mut func_val = (**func).clone();
            func_val.sig = Arc::new(specialized_call_args.sig);

            let args = &specialized_call_args.actual_args;
            let args_span = func_call.args_span.as_ref();
            let type_args = specialized_call_args.type_args.as_ref();

            func_val.create_invocation(args, args_span, type_args, expect_ty, &span, ctx)
                .map(Arc::new)?
        },

        // reference to non-method function via UFCS syntax
        Value::UfcsFunction(ufcs_call) => {
            let call_ty_args = match &func_call.type_args {
                Some(call_ty_args) => {
                    Some(typecheck_type_args(call_ty_args, ctx)?)
                }

                None => None,
            };

            let call_span = func_call.annotation.span();
            
            let args = &func_call.args;
            let args_span = func_call.args_span.as_ref();

            typecheck_ufcs_invocation(ufcs_call, args, call_ty_args, call_span, args_span, ctx)
                .map(Arc::new)?
        },

        // reference to an overloaded name that could resolve to a method, function or ufcs function
        Value::Overload(overloaded) => {
            typecheck_func_overload_call(&overloaded, func_call, &target, expect_ty, ctx)
                .map(Arc::new)?
        },

        // direct reference to a single method
        Value::Method(method) => {
            typecheck_method_call(method, func_call, ctx)
                .map(Arc::new)?
        }

        // object constructor invocation without args is indistinguishable from a call expression
        Value::Type(target_type, span) => {
            let Some(ctor) = func_call.clone().try_into_empty_object_ctor() else {
                return Err(TypeError::NotCallable(Box::new(target.annotation().clone())))
            };

            let type_args = match ctor.type_args {
                Some(list) => Some(typecheck_type_args(&list, ctx)?),
                None => None,
            };
            
            assert_eq!(0, ctor.args.members.len());
            
            let object_type = TypeName::named(target_type.clone(), span.clone());
            
            Arc::new(Invocation::ObjectCtor {
                span: ctor.annotation.clone(),
                type_args,
                members: Vec::new(),
                object_type, 
            })
        }

        Value::VariantCase(case_val, ..) => {
            let ctor_invocation = case_val.typecheck_invocation(
                &func_call.args,
                func_call.type_args.as_ref(),
                expect_ty,
                func_call.span(),
                ctx,
            )?;
            
            Arc::new(ctor_invocation)
        }

        other => {
            return Err(TypeError::NotCallable(Box::new(other.clone())))
        },
    };

    Ok(FunctionCall {
        target,
        type_args: invocation.type_args().cloned(),
        args: invocation.args().cloned().collect(),
        args_span: invocation.args_span().cloned(),
        annotation: Value::Invocation(invocation),
    })
}

fn typecheck_func_overload_call(
    overloaded: &OverloadValue,
    func_call: &ast::FunctionCall<Span>,
    target: &Expr,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Invocation> {
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

    match &overloaded.candidates[overload.selected_sig] {
        OverloadCandidate::Function { decl_name, decl, visibility } => {
            let sig = overload.func_sig(&Type::Nothing, &overloaded.candidates);

            let func_val = FunctionValue::new(
                decl_name.clone(),
                *visibility,
                decl.clone(),
                sig,
                target.span().clone(),
            );
            
            func_val.create_invocation(
                &overload.args, 
                func_call.args_span.as_ref(),
                overload.type_args.as_ref(),
                expect_ty,
                func_call.span(),
                ctx
            )
        },

        OverloadCandidate::Method {
            self_ty,
            iface_ty,
            decl,
            index,
            ..
        } => {
            let sig = overload.func_sig(self_ty, &overloaded.candidates);

            assert_eq!(
                overload.args.len(), 
                sig.params.len(), 
                "in expression {}: resolved overload had wrong number of arguments", 
                func_call
            );

            if self_ty.get_current_access(ctx) < decl.access {
                return Err(TypeError::TypeMemberInaccessible {
                    ty: self_ty.clone(),
                    access: decl.access,
                    member: decl.func_decl.ident().clone(),
                    span: func_call.span().clone(),
                })
            }

            // eprintln!("method call (overload) {} = ({}){}.{} -> {} ({})", func_call, iface_ty, self_ty, ident, index, sig);
            
            let args: Vec<_> = overload.args
                .into_iter()
                .collect();
            
            let self_arg = args
                .get(0)
                .expect("method must have at least one arg")
                .clone();

            let method = Arc::new(MethodValue {
                span: func_call.span().clone(),
                self_arg: Some(Box::new(self_arg)),
                self_ty: TypeName::inferred(iface_ty.clone()),
                decl: decl.clone(),
                index: *index,
                sig: Arc::new(sig),
            });

            assert_eq!(
                args.len(),
                method.decl.func_decl.params.len(),
                "argument count mismatch for {} at {}",
                method.decl.func_decl.name,
                method.span,
            );
            
            Ok(Invocation::Method {
                method,
                self_ty: self_ty.clone(),
                type_args: overload.type_args,
                args,
                span: func_call.span().clone(),
                args_span: func_call.args_span.clone(),
            })
        },
    }
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
// 
// pub fn overload_to_no_args_call(
//     candidates: &[OverloadCandidate],
//     overload: Overload,
//     mut target: Expr,
//     self_arg: Option<Expr>,
//     span: &Span,
//     ctx: &Context,
// ) -> TypeResult<Expr> {
//     assert_eq!(self_arg.iter().len(), overload.args.len());
//     
//     let decl = candidates[overload.selected_sig].decl();
//     assert_eq!(decl.type_params_len(), overload.type_args_len());
// 
//     let call_decl = match &overload.type_args {
//         Some(type_args) => {
//             let specialized = specialize_func_decl(decl, type_args, ctx)
//                 .map_err(|e| TypeError::from_generic_err(e, span.clone()))?;
// 
//             Arc::new(specialized)
//         }
// 
//         None => decl.clone(),
//     };
// 
//     let call = match &candidates[overload.selected_sig] {
//         OverloadCandidate::Function { decl_name, visibility, .. } => {
//             let return_value = TypedValue::temp(call_decl.result_ty.clone(), span.clone());
// 
//             *target.annotation_mut() = Value::from(FunctionValue::new(
//                 decl_name.clone(),
//                 *visibility,
//                 decl.clone(),
//                 target.span().clone(),
//             ));
// 
//             Call::FunctionNoArgs(FunctionCallNoArgs {
//                 target: Expr::from(target),
//                 self_arg,
//                 type_args: overload.type_args,
//                 annotation: Value::from(return_value),
//             })
//         }
// 
//         OverloadCandidate::Method { self_ty, index, decl, .. } => {
//             let return_value = TypedValue::temp(call_decl.result_ty.clone(), span.clone());
// 
//             *target.annotation_mut() = Value::from(MethodValue {
//                 self_ty: TypeName::inferred(self_ty.clone()),
//                 index: *index,
//                 decl: decl.clone(),
//                 span: target.span().clone(),
//             });
// 
//             Call::MethodNoArgs(MethodCallNoArgs {
//                 target: Expr::from(target),
//                 self_arg,
//                 method_name: decl.func_decl.name.ident.clone(),
//                 type_args: overload.type_args,
//                 annotation: return_value.into(),
//                 owning_type: self_ty.clone(),
//             })
//         }
//     };
// 
//     Ok(Expr::Call(Box::new(call)))
// }

/// * `iface_method` - Method to be called
/// * `func_call` - Source node of this call
/// * `with_self_arg` - If present, an expression which in a previous phase has been determined
///     to be the self-argument of this call. For example, when converting a call like `a.B` which
///     was previously read as a no-args call and is now being converted into a full call (`a.B()`),
///     the `a` reference is the self-arg, but isn't part of the argument list `()` of the outer
///     call expression, so needs to be provided separately
/// * `ctx` - current typechecking context
fn typecheck_method_call(
    method: &Arc<MethodValue>,
    func_call: &ast::FunctionCall<Span>,
    ctx: &mut Context,
) -> TypeResult<Invocation> {
    // not yet supported
    if let Some(call_type_args) = &func_call.type_args {
        return Err(TypeError::from_generic_err(
            GenericError::ArgsLenMismatch {
                expected: 0,
                actual: call_type_args.len(),
                target: GenericTarget::FunctionSig(method.decl.func_decl.sig()),
            },
            call_type_args.span().clone(),
        ));
    }
    
    let invocation = method.create_invocation(&func_call.args, func_call.args_span.as_ref(), func_call.span(), ctx)?;
    
    // eprintln!("{}: {func_call} => {:#?}", func_call.span(), invocation);
    
    Ok(invocation)
}

fn typecheck_ufcs_invocation(
    ufcs: &UfcsValue,
    rest_args: &[ast::Expr<Span>],
    type_args: Option<TypeArgList>,
    span: &Span,
    args_span: Option<&Span>,
    ctx: &mut Context,
) -> TypeResult<Invocation> {
    if ufcs.visibility < Visibility::Interface
        && !ctx.is_current_namespace_child(&ufcs.function_name.full_path) {
        return Err(TypeError::NameNotVisible {
            name: ufcs.function_name.full_path.clone(),
            span: span.clone(),
        });
    }

    let mut specialized_call_args = specialize_call_args(
        &ufcs.decl,
        &rest_args,
        Some(&ufcs.self_arg),
        type_args,
        &span,
        ctx,
    )?;
    assert_eq!(specialized_call_args.actual_args.len(), rest_args.len() + 1);

    validate_args(
        &mut specialized_call_args.actual_args,
        &specialized_call_args.sig.params,
        span,
        ctx,
    )?;
    
    let func_val = FunctionValue {
        name: ufcs.function_name
            .clone()
            .with_ty_args(specialized_call_args.type_args.clone()),
        span: span.clone(),
        sig: Arc::new(specialized_call_args.sig),
        decl: ufcs.decl.clone(),
        visibility: ufcs.visibility,
    };
    
    // eprintln!("-- func invocation: {} with {} args", func_val.decl, specialized_call_args.actual_args.len());

    Ok(Invocation::Function {
        function: Arc::new(func_val),
        args: specialized_call_args.actual_args,
        args_span: args_span.cloned(),
        type_args: specialized_call_args.type_args,
        span: span.clone(),
    })
}

// invocations of function values (lambdas, references to free functions) must have a fixed
// signature that was already resolved when the reference was created, so we expect to know
// all parameter types exactly and have no type arguments
fn typecheck_func_value_invocation(
    target: Expr,
    func_call: &ast::FunctionCall<Span>,
    self_arg: Option<&Expr>,
    sig: &Arc<FunctionSig>,
    ctx: &mut Context,
) -> TypeResult<Invocation> {
    let args = build_args_for_params(
        &sig.params,
        &func_call.args,
        self_arg,
        func_call.span(),
        ctx
    )?;

    Ok(Invocation::FunctionValue {
        value: target,
        args,
        args_span: func_call.args_span.clone(),
        sig: sig.clone(),
    })
}

pub fn evaluate_no_args_function_call(func: &Arc<FunctionValue>) -> TypeResult<Invocation> {
    if func.sig.params.len() > 0 {
        return Err(TypeError::invalid_args_with_sig(&func.sig, [], func.span.clone()));
    }
    
    if func.sig.type_params.is_some() {
        return Err(TypeError::from_generic_err(GenericError::ArgsLenMismatch {
            actual: 0,
            expected: func.sig.type_params_len(),
            target: GenericTarget::FunctionSig((*func.sig).clone()),
        }, func.span.clone()));
    }
    
    let invocation = Invocation::Function {
        span: func.span.clone(),
        function: func.clone(),
        type_args: None,
        args: Vec::new(),
        args_span: None,
    };
    
    Ok(invocation)
}
