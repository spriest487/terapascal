mod init;
mod literal;

use crate::ast;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Visibility;
pub use crate::typ::ast::call::typecheck_call;
pub use crate::typ::ast::call::Invocation;
use crate::typ::ast::cast::typecheck_cast_expr;
use crate::typ::ast::{check_overload_visibility, typecheck_type_args};
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::overload_to_no_args_call;
use crate::typ::ast::try_resolve_overload;
use crate::typ::ast::typecheck_bin_op;
use crate::typ::ast::typecheck_block;
use crate::typ::ast::typecheck_case_expr;
use crate::typ::ast::typecheck_collection_ctor;
use crate::typ::ast::typecheck_exit;
use crate::typ::ast::typecheck_func_expr;
use crate::typ::ast::typecheck_if_cond_expr;
use crate::typ::ast::typecheck_match_expr;
use crate::typ::ast::typecheck_object_ctor;
use crate::typ::ast::typecheck_raise;
use crate::typ::ast::typecheck_unary_op;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::OverloadCandidate;
use crate::typ::Decl;
use crate::typ::FunctionValue;
use crate::typ::NameError;
use crate::typ::OverloadValue;
use crate::typ::ScopeMemberRef;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::Context;
use crate::IntConstant;
use terapascal_common::span::*;
pub use init::*;
pub use literal::*;

pub type Expr = ast::Expr<Value>;

pub fn const_eval_string(expr: &Expr, ctx: &Context) -> TypeResult<String> {
    match expr.const_eval(ctx) {
        Some(Literal::String(src_str)) => Ok((*src_str).clone()),

        _ => Err(TypeError::InvalidConstExpr {
            expr: Box::new(expr.clone()),
        }),
    }
}

pub fn const_eval_integer(expr: &Expr, ctx: &Context) -> TypeResult<IntConstant> {
    match expr.const_eval(ctx) {
        Some(Literal::Integer(int_const)) => Ok(int_const),

        _ => Err(TypeError::InvalidConstExpr {
            expr: Box::new(expr.clone()),
        }),
    }
}

pub fn typecheck_expr(
    expr_node: &ast::Expr<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    match expr_node {
        ast::Expr::Literal(lit, span) => literal::typecheck_literal(lit, expect_ty, span, ctx),

        ast::Expr::Ident(ident, span) => typecheck_ident(ident, expect_ty, span, ctx),

        ast::Expr::BinOp(bin_op) => typecheck_bin_op(bin_op, expect_ty, ctx),

        ast::Expr::UnaryOp(unary_op) => {
            let unary_op = typecheck_unary_op(unary_op, expect_ty, ctx)?;
            Ok(ast::Expr::from(unary_op))
        },

        ast::Expr::Call(call) => {
            let expr = match typecheck_call(call, expect_ty, ctx)? {
                Invocation::Call(call) => ast::Expr::from(*call),
                Invocation::Ctor(ctor) => ast::Expr::from(*ctor),
            };
            Ok(expr)
        },

        ast::Expr::ObjectCtor(ctor) => {
            let span = ctor.annotation.span().clone();
            let ctor = typecheck_object_ctor(ctor, span, expect_ty, ctx)?;
            Ok(ast::Expr::from(ctor))
        },

        ast::Expr::CollectionCtor(ctor) => {
            let ctor = typecheck_collection_ctor(ctor, expect_ty, ctx)?;
            Ok(ast::Expr::from(ctor))
        },

        ast::Expr::IfCond(if_cond) => {
            let if_cond = typecheck_if_cond_expr(if_cond, expect_ty, ctx)?;
            Ok(ast::Expr::from(if_cond))
        },

        ast::Expr::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;
            Ok(ast::Expr::from(block))
        },

        ast::Expr::Raise(raise) => {
            let raise = typecheck_raise(raise, expect_ty, ctx)?;
            Ok(ast::Expr::from(raise))
        },

        ast::Expr::Case(case) => {
            let case = typecheck_case_expr(case, expect_ty, ctx)?;
            Ok(ast::Expr::from(case))
        },

        ast::Expr::Match(match_expr) => {
            let match_expr = typecheck_match_expr(match_expr, expect_ty, ctx)?;
            Ok(ast::Expr::from(match_expr))
        },

        ast::Expr::Exit(exit) => {
            let exit = typecheck_exit(exit, expect_ty, ctx)?;
            Ok(ast::Expr::from(exit))
        },

        ast::Expr::Cast(cast) => {
            let cast = typecheck_cast_expr(cast, ctx)?;
            Ok(ast::Expr::from(cast))
        },

        ast::Expr::AnonymousFunction(def) => {
            let anon_func = typecheck_func_expr(def, expect_ty, ctx)?;
            Ok(ast::Expr::from(anon_func))
        },
        
        ast::Expr::ExplicitSpec(with_expr) => {
            let mut base_expr = typecheck_expr(&with_expr.type_expr, &Type::Nothing, ctx)?;
            let type_args = typecheck_type_args(&with_expr.type_args, ctx)?;
            
            match base_expr.annotation() {
                Value::Type(generic_ty, span) => {
                    let spec_ty = generic_ty
                        .specialize(&type_args, ctx)
                        .map_err(|err| TypeError::from_generic_err(err, span.clone()))?
                        .into_owned();
                    
                    *base_expr.annotation_mut() = Value::Type(spec_ty, span.clone());
                    Ok(base_expr)
                }
                
                other=> Err(TypeError::InvalidExplicitSpec {
                    target: other.clone(),
                }),
            }
        }
    }
}

fn typecheck_ident(
    ident: &Ident,
    expect_ty: &Type,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let decl = match ctx.find_name(ident) {
        Some(decl) => decl,
        None => {
            let not_found_ident = ident.clone().into();
            return Err(TypeError::NameError {
                err: NameError::NotFound {
                    ident: not_found_ident,
                },
                span: span.clone(),
            });
        },
    };

    match &decl {
        ScopeMemberRef::Decl {
            value: Decl::Function { overloads, .. },
            parent_path,
            ..
        } => {
            let decl_annotation = member_annotation(&decl, span.clone(), ctx);
            let decl_namespace = parent_path.to_namespace();

            // an ident referencing a function with no parameters is interpreted as a call to
            // that function, but we wrap it in the NoArgs type so it can be unwrapped if this
            // expression appears in an actual call node
            let can_call_noargs = overloads.len() == 1
                && should_call_noargs_in_expr(overloads[0].decl(), expect_ty, &Type::Nothing)
                && (overloads[0].visiblity() >= Visibility::Interface 
                    || ctx.is_current_namespace(&decl_namespace));
            
            let call_expr = if can_call_noargs {
                let func_decl = overloads[0].decl().clone();
                let func_vis = overloads[0].visiblity();

                let func_path = decl_namespace.child(ident.clone());
                let func_sym = Symbol::from(func_path)
                    .with_ty_params(func_decl.name.type_params.clone());

                let candidates = [OverloadCandidate::Function {
                    decl_name: func_sym.clone(),
                    decl: func_decl.clone(),
                    visibility: func_vis,
                }];

                let overload_result = try_resolve_overload(
                    &candidates,
                    &[],
                    None,
                    None,
                    span,
                    ctx
                );

                match overload_result {
                    Some(overload) => {
                        check_overload_visibility(&overload, &candidates, span, ctx)?;
                        
                        let func_annotation = FunctionValue::new(
                            func_sym,
                            func_vis,
                            func_decl,
                            span.clone(),
                        );
                        
                        let func_expr = Expr::Ident(ident.clone(), func_annotation.into());
                        
                        let call = overload_to_no_args_call(
                            &candidates,
                            overload,
                            func_expr,
                            None,
                            span,
                            ctx
                        )?;
                        Some(call)
                    },

                    None => None,
                }
            } else {
                None
            };
            
            match call_expr {
                Some(expr) => Ok(expr),
                None => member_ident_expr(decl_annotation, ident, ctx)
            }
        },

        ScopeMemberRef::Decl {
            value: Decl::GlobalConst { val, .. } | Decl::LocalConst { val, .. }, .. } => {
            let value = member_annotation(&decl, span.clone(), ctx);

            Ok(Expr::Literal(val.clone(), value))
        },

        _ => {
            let decl_annotation = member_annotation(&decl, span.clone(), ctx);
            member_ident_expr(decl_annotation, ident, ctx)
        },
    }
}

fn member_ident_expr(
    member_val: Value,
    ident: &Ident,
    ctx: &mut Context
) -> Result<Expr, TypeError> {
    add_ident_closure_capture(&member_val, ctx);

    Ok(ast::Expr::Ident(ident.clone(), member_val))
}

// if a value references a local ident, and it comes from a scope above the current
// closure scope if any, then add it to the closure captures
fn add_ident_closure_capture(value: &Value, ctx: &mut Context) {
    let decl_name = match value.decl() {
        Some(path) if path.len() == 1 => path,
        _ => return,
    };
    
    let Some(decl_scope) = ctx.get_decl_scope(decl_name.last()) else {
        return;
    };

    let decl_scope_id = decl_scope.id();
    if let Some(closure_scope_id) = ctx.get_closure_scope().map(|s| s.id()) {
        if decl_scope_id < closure_scope_id {
            ctx.add_closure_capture(decl_name.last(), value.ty().as_ref());
        }
    }
}

fn should_call_noargs_in_expr(decl: &FunctionDecl, expect_ty: &Type, self_arg_ty: &Type) -> bool {
    decl.sig().should_call_noargs_in_expr(expect_ty, self_arg_ty)
}

pub fn member_annotation(member: &ScopeMemberRef, span: Span, ctx: &Context) -> Value {
    match member {
        ScopeMemberRef::Decl {
            value: Decl::Alias(aliased),
            ..
        } => {
            let alias_ref = ctx
                .find_path(aliased)
                .unwrap_or_else(|| panic!("invalid alias to {}", aliased));

            member_annotation(&alias_ref, span, ctx)
        },

        ScopeMemberRef::Decl { value: Decl::LocalVariable { binding, .. }, .. } => {
            let decl_name = binding.def
                .as_ref()
                .map(|name| IdentPath::from(name.clone()));

            Value::from(TypedValue {
                span,
                ty: binding.ty.clone(),
                value_kind: binding.kind,
                decl: decl_name,
            })
        }

        ScopeMemberRef::Decl { value: Decl::GlobalVariable { binding, .. }, parent_path, .. } => {
            let decl_name = binding.def
                .as_ref()
                .map(|name| parent_path.to_namespace().child(name.clone()));

            Value::from(TypedValue {
                span,
                ty: binding.ty.clone(),
                value_kind: binding.kind,
                decl: decl_name,
            })
        }

        ScopeMemberRef::Decl {
            value: Decl::Function { overloads, visibility, .. },
            parent_path,
            key,
        } => {
            let func_path = parent_path.to_namespace().child((*key).clone());
            
            if overloads.len() == 1 {
                let decl = overloads[0].decl();
                if parent_path.as_slice().is_empty() {
                    panic!("empty path for decl {}", key);
                }

                // the named version of the function never has type args, the caller will have
                // to specialize the expr to add some
                let func_path = parent_path.to_namespace().child((*key).clone());
                let func_sym = Symbol::from(func_path)
                    .with_ty_params(decl.name.type_params.clone());

                FunctionValue::new(
                    func_sym,
                    *visibility,
                    decl.clone(),
                    span,
                ).into()
            } else {
                let candidates = overloads
                    .iter()
                    .map(|overload| {
                        let func_sym = Symbol::from(func_path.clone())
                            .with_ty_params(overload.decl().name.type_params.clone());

                        OverloadCandidate::Function {
                            decl_name: func_sym,
                            decl: overload.decl().clone(),
                            visibility: overload.visiblity(),
                        }
                    })
                    .collect();

                OverloadValue {
                    span,
                    candidates,
                    self_arg: None,
                    sig: None,
                }.into()
            }
        },

        ScopeMemberRef::Decl { value: Decl::GlobalConst { ty, .. }, parent_path, key, .. } => {
            let decl_name = parent_path.to_namespace().child((**key).clone());
            let typed_val = TypedValue::unit_const(ty.clone(), decl_name, span.clone());

            Value::from(typed_val)
        },

        ScopeMemberRef::Decl { value: Decl::LocalConst { ty, .. }, key, .. } => {
            let decl_name = (**key).clone();
            let typed_val = TypedValue::local_const(ty.clone(), decl_name, span.clone());

            Value::from(typed_val)
        },

        ScopeMemberRef::Decl { value: Decl::Type { ty, .. }, .. } => {
            Value::Type(ty.clone(), span)
        },

        ScopeMemberRef::Decl { value: Decl::Namespace(path), .. } => {
            Value::Namespace(path.clone(), span)
        },

        ScopeMemberRef::Scope { path } => {
            Value::Namespace(IdentPath::from_parts(path.keys().cloned()), span)
        },
    }
}

