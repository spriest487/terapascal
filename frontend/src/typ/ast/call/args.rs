use crate::ast;
use crate::ast::FunctionParamMod;
use crate::ast::TypeConstraint;
use crate::ast::TypeList;
use crate::typ::ast::call;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::FunctionSigParam;
use crate::typ::GenericContext;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgsResult;
use crate::typ::TypeError;
use crate::typ::TypeParam;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use crate::typ::TypeResult;
use crate::typ::ValueKind;
use common::span::Span;

pub struct SpecializedCallArgs {
    pub sig: FunctionSig,
    pub type_args: Option<TypeList<Type>>,
    pub actual_args: Vec<Expr>,
}

fn specialize_arg<ArgProducer>(
    param_ty: &Type,
    inferred_ty_args: &mut GenericContext,
    produce_expr: ArgProducer,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Expr>
where
    ArgProducer: FnOnce(&Type, &mut Context) -> TypeResult<Expr>,
{
    let mut expect_ty = inferred_ty_args.apply_to_type(param_ty.clone());

    if expect_ty.is_generic_param()
        || expect_ty.is_unspecialized_generic()
        || expect_ty.contains_unresolved_params(ctx) {
        // not enough info to resolve this generic type fully yet: arg expr type drives param type
        expect_ty = Type::Nothing;
    }

    let actual_arg =  produce_expr(&expect_ty, ctx)?;
    let actual_ty = actual_arg.annotation().ty().clone();
    
    infer_from_structural_ty_args(param_ty, &actual_ty, inferred_ty_args, span);

    Ok(actual_arg)
}

pub fn infer_from_structural_ty_args(
    param_ty: &Type,
    actual_ty: &Type,
    inferred_ty_args: &mut GenericContext,
    span: &Span,
) {    
    // in param_ty, find all the references to generic params within the type and their
    // corresponding values in the actual type, in the order they appear.
    // for example if the expected type is `array of T`, and the actual
    // arg is `array of Int32`, `T` is added to the param ty_def args and `Int32` is added to the 
    // actual ty_def args.
    // at this point, if the types are fundamentally incompatible we should bail
    let (param_ty_args, actual_ty_args) = match (param_ty, actual_ty) {
        // plain generic param can be substituted for anything
        (Type::GenericParam(..), actual) => {
            (vec![param_ty.clone()], vec![actual.clone()])
        }

        // static-length arrays can be substituted into a generic array of the same length 
        (Type::Array(param_array_ty), Type::Array(actual_array_ty)) => {
            if actual_array_ty.dim != param_array_ty.dim {
                return;
            }

            (
                vec![param_array_ty.element_ty.clone()],
                vec![actual_array_ty.element_ty.clone()],
            )
        },

        // dynarrays only need element type to match
        (Type::DynArray { element: param_el }, Type::DynArray { element: actual_el }) => {
            (vec![(**param_el).clone()], vec![(**actual_el).clone()])
        },

        (Type::Function(param_sig), Type::Function(actual_sig)) => {
            let mut param_tys = Vec::new();
            param_tys.push(param_sig.return_ty.clone());
            param_tys.extend(param_sig.params.iter().map(|p| p.ty.clone()));

            let mut arg_tys = Vec::new();
            arg_tys.push(actual_sig.return_ty.clone());
            arg_tys.extend(actual_sig.params.iter().map(|p| p.ty.clone()));

            (param_tys, arg_tys)
        }

        _ => {
            // all other types: must be the exact same declaration (struct, enum etc)
            if !param_ty.same_decl_type(actual_ty) {
                return;
            }

            let param_ty_args = match param_ty.type_args() {
                TypeArgsResult::Specialized(_, args) => args.items.clone(),
                _ => return,
            };
            let actual_ty_args = match actual_ty.type_args() {
                TypeArgsResult::Specialized(_, args) => args.items.clone(),
                _ => return,
            };
            (param_ty_args, actual_ty_args)
        },
    };

    let all_ty_args = param_ty_args.iter().zip(actual_ty_args.iter());
    for (param_ty_arg, actual_ty_arg) in all_ty_args {
        match param_ty_arg {
            Type::GenericParam(param_generic) => {
                // if we already inferred a type for this param, we don't care about the new type
                // here - if it doesn't match, we want to get an InvalidArgs error later
                if inferred_ty_args.find_position(param_generic.name.as_str()).is_some() {
                    continue;
                }
                
                let inferred_param = TypeParam {
                    name: param_generic.name.clone(),
                    constraint: match &param_generic.is_ty {
                        Type::Any => None,

                        is_ty => Some(TypeConstraint {
                            name: param_generic.name.clone(),
                            is_ty: is_ty.clone(),
                            span: span.clone(),
                        })
                    },
                };

                inferred_ty_args.add(inferred_param, actual_ty_arg.clone());
            }

            _ => infer_from_structural_ty_args(param_ty_arg, actual_ty_arg, inferred_ty_args, span),
        }
    }
}

/// for a function decl, figure out the actual arg types and values from the provided
/// expressions. evaluate arguments left-to-right until we either resolve a signature that
/// matches the args, or conclude that the args don't match the call and return an error.
///
/// for example `function A[T](t: T; t2: array[2] of T)`' s actual signature can be deduced
/// from the actual type of `t`, and `t2`'s value can be checked against that.
pub fn specialize_call_args(
    decl: &FunctionDecl,
    args: &[ast::Expr<Span>],
    self_arg: Option<&Expr>,
    explicit_ty_args: Option<TypeList<Type>>,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<SpecializedCallArgs> {
    let decl_sig = match self_arg {
        Some(expr) => decl.sig().with_self(expr.annotation().ty().as_ref()),
        None => decl.sig(),
    };
    
    let ty_params = match &decl.name.type_params {
        None => {
            if let Some(explicit_ty_args) = explicit_ty_args {
                return Err(TypeError::from_generic_err(
                    GenericError::ArgsLenMismatch {
                        expected: 0,
                        actual: explicit_ty_args.len(),
                        target: GenericTarget::FunctionSig(decl_sig),
                    },
                    span.clone(),
                ));
            }

            // no type params
            let actual_args = call::build_args_for_params(
                &decl_sig.params, // todo this could avoid copying
                args,
                self_arg,
                span,
                ctx
            )?;

            return Ok(SpecializedCallArgs {
                sig: decl_sig,
                actual_args,
                type_args: None,
            });
        }

        Some(params) => params,
    };

    if let Some(explicit_ty_args) = explicit_ty_args {
        // we have explicit args, don't try to infer types
        let call_sig = decl_sig.apply_type_args(ty_params, &explicit_ty_args);
        let actual_args = call::build_args_for_params(&call_sig.params, args, self_arg, span, ctx)?;

        Ok(SpecializedCallArgs {
            sig: call_sig,
            actual_args,
            type_args: Some(explicit_ty_args),
        })
    } else {
        // we haven't checked arg length matches yet because we haven't typechecked args
        let self_arg_len = if self_arg.is_some() { 1 } else { 0 };
        if args.len() + self_arg_len != decl.params.len() {
            // this is an inferral error because we don't have enough information to report
            // it as an arg type mismatch
            return Err(TypeError::from_generic_err(
                GenericError::CannotInferArgs {
                    target: GenericTarget::FunctionSig(decl_sig),
                    hint: GenericTypeHint::Unknown,
                },
                span.clone(),
            ));
        }

        // try to infer type from args, left to right
        let mut inferred_ty_args = GenericContext::empty();
        let mut actual_args = Vec::new();

        if let Some(self_arg) = self_arg.cloned() {
            let decl_self_param = &decl.params[0];
            let actual_self_arg = specialize_arg(
                &decl_self_param.ty,
                &mut inferred_ty_args,
                |_expect_ty, _ctx| Ok(self_arg),
                span,
                ctx,
            )?;

            actual_args.push(actual_self_arg);
        }

        for (i, arg) in args.iter().enumerate() {
            let param_ty = &decl.params[i + self_arg_len].ty;

            let actual_arg = specialize_arg(
                param_ty,
                &mut inferred_ty_args,
                |expect_ty, ctx| typecheck_expr(arg, expect_ty, ctx),
                span,
                ctx,
            )?;

            actual_args.push(actual_arg);
        }
        
        let call_sig = inferred_ty_args.apply_to_sig(&decl_sig);

        let call_ty_args = match try_unwrap_inferred_args(ty_params, inferred_ty_args, ctx, span) {
            Some(list) => list,
            None => {
                let arg_tys = actual_args
                    .iter()
                    .map(|a| a.annotation().ty().into_owned())
                    .collect();

                return Err(TypeError::from_generic_err(
                    GenericError::CannotInferArgs {
                        target: GenericTarget::FunctionSig(decl_sig.clone()),
                        hint: GenericTypeHint::ArgTypes(arg_tys),
                    },
                    span.clone(),
                ));
            }
        };

        // eprintln!(
        //     "INFERRED ARGS:\n\tdecl: {}\n\tinferred: {}\n\tfinal sig: {}\n\tfinal argument types: [{}]",
        //     decl,
        //     call_ty_args,
        //     call_sig,
        //     actual_args
        //         .iter()
        //         .map(|arg| arg.annotation().ty().to_string())
        //         .collect::<Vec<_>>()
        //         .join(", ")
        // );

        Ok(SpecializedCallArgs {
            type_args: Some(call_ty_args),
            sig: call_sig,
            actual_args,
        })
    }
}

/// type list of the complete inferred type args, or None if any of them can't be unwrapped because
/// we haven't successfully inferred them
pub fn try_unwrap_inferred_args(
    type_params: &TypeParamList,
    inferred_args: GenericContext,
    ctx: &Context,
    span: &Span,
) -> Option<TypeList<Type>> {
    if !validate_inferred_args(type_params, &inferred_args, ctx) {
        return None;
    }

    let items: Vec<_> = inferred_args
        .into_items()
        .into_iter()
        .map(|resolved| resolved.arg)
        .collect();

    Some(TypeList::new(items, span.clone()))
}

fn validate_inferred_args(
    type_params: &TypeParamList,
    inferred_args: &GenericContext,
    ctx: &Context,
) -> bool {
    for param in type_params.iter() {
        let ty_arg = match inferred_args.find_arg(param.name.as_str()) {
            Some(ty) => ty,
            None => return false,
        };

        if let Some(constraint) = &param.constraint {
            if !ty_arg.match_constraint(&constraint.is_ty, ctx) {
                return false;
            }
        }
    }

    true
}

/// final arg validation when all arg types are known and specialized correctly, and any
/// self-args have been processed into normal arguments to match the sig
/// - insert any required implicit type conversions
/// - validate length
/// - ensure that the expressions provided for out/var refs are mutable l-values
/// - mark the names referenced in out vars as initialized
pub fn validate_args(
    args: &mut [Expr],
    params: &[FunctionSigParam],
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<()> {
    if args.len() != params.len() {
        return Err(call::invalid_args(args.to_vec(), params, span.clone()));
    }

    for i in 0..params.len() {
        // only do implicit conversion for non-ref params
        if params[i].is_by_ref() {
            continue;
        }

        args[i] = implicit_conversion(args[i].clone(), &params[i].ty, ctx)
            .map_err(|err| match err {
                TypeError::TypeMismatch { .. } => {
                    call::invalid_args(args.to_vec(), params, span.clone())
                },
                err => err,
            })?;
    }

    let args_and_params = args.iter().zip(params.iter());
    for (arg, param) in args_and_params {
        let (is_in_ref, is_out_ref) = match &param.modifier {
            None => (false, false),
            Some(FunctionParamMod::Out) => (false, true),
            Some(FunctionParamMod::Var) => (true, true),
        };

        if is_in_ref || is_out_ref {
            match arg.annotation().value_kind() {
                // in-refs shouldn't really allow uninitialized values here but we do init checking
                // in a separate pass
                Some(ValueKind::Mutable) | Some(ValueKind::Uninitialized) => {},
                _ => {
                    return Err(TypeError::NotMutable {
                        expr: Box::new(arg.clone()),
                        decl: None,
                    });
                },
            }

            if is_out_ref {
                if let Expr::Ident(ref_ident, ..) = &arg {
                    ctx.initialize(ref_ident);
                }
            }
        }
    }

    Ok(())
}
