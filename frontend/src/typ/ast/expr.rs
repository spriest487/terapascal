mod init;
mod literal;

use crate::ast;
use crate::ast::{Ident, SemanticHint};
use crate::ast::IdentPath;
pub use crate::typ::ast::call::typecheck_call;
use crate::typ::ast::cast::typecheck_cast_expr;
use crate::typ::ast::const_eval::ConstEval;
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
use crate::typ::ast::typecheck_type_args;
use crate::typ::ast::typecheck_unary_op;
use crate::typ::ast::OverloadCandidate;
use crate::typ::function::FunctionValue;
use crate::typ::overload::OverloadValue;
use crate::typ::Context;
use crate::typ::Decl;
use crate::typ::EvaluatedConstExpr;
use crate::typ::NameError;
use crate::typ::ScopeMemberRef;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::IntConstant;
pub use init::*;
pub use literal::*;
use terapascal_common::span::*;

pub type Expr = ast::Expr<Value>;

impl Expr {
    // where a typed value is expected, convert an expression that might refer to some non-value 
    // entity like a type or function into a value, or fail if it can't be converted. references
    // to functions without call operators will be treated as invocations with zero arguments here
    // and checked against the function signature accordingly
    pub fn evaluate(mut self, expect_ty: &Type, ctx: &mut Context) -> TypeResult<Self> {
        self.annotation_mut().evaluate(expect_ty, ctx)?;
        
        Ok(self)
    }
}

pub fn const_eval_string(expr: &Expr, ctx: &Context) -> TypeResult<EvaluatedConstExpr<String>> {
    match expr.const_eval(ctx) {
        Some(Literal::String(src_str)) => {
            let evaluated = EvaluatedConstExpr {
                value: (*src_str).clone(),
                expr: Box::new(expr.clone()),
            };

            Ok(evaluated)
        },

        _ => Err(TypeError::InvalidConstExpr {
            expr: Box::new(expr.clone()),
        }),
    }
}

pub fn const_eval_integer(expr: &Expr, ctx: &Context) -> TypeResult<EvaluatedConstExpr<IntConstant>> {
    match expr.const_eval(ctx) {
        Some(Literal::Integer(int_const)) => {
            let evaluated = EvaluatedConstExpr {
                value: int_const,
                expr: Box::new(expr.clone()),
            };

            Ok(evaluated)
        },

        _ => Err(TypeError::InvalidConstExpr {
            expr: Box::new(expr.clone()),
        }),
    }
}

pub fn evaluate_expr(
    expr_node: &ast::Expr<Span>,
    expect_ty: &Type,
    ctx: &mut Context
) -> TypeResult<Expr> {
    typecheck_expr(expr_node, expect_ty, ctx)?.evaluate(expect_ty, ctx)
}

pub fn typecheck_expr(
    expr_node: &ast::Expr<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    match expr_node {
        ast::Expr::Literal(lit) => typecheck_literal(&lit.literal, expect_ty, &lit.annotation, ctx),

        ast::Expr::Ident(ident, span) => typecheck_ident(ident, span, ctx),

        ast::Expr::BinOp(bin_op) => typecheck_bin_op(bin_op, expect_ty, ctx),

        ast::Expr::UnaryOp(unary_op) => {
            let unary_op = typecheck_unary_op(unary_op, expect_ty, ctx)?;
            Ok(ast::Expr::from(unary_op))
        },

        ast::Expr::Call(call) => {
            let call = typecheck_call(call, expect_ty, ctx)?;
            Ok(Expr::from(call))
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
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let Some(decl) = ctx.find_name(ident) else {
        return Err(TypeError::from_name_err(
            NameError::NotFound {
                ident: ident.clone().into(),
            }, 
            span.clone())
        );
    };

    match &decl {
        ScopeMemberRef::Decl { value: Decl::Function { .. }, .. } => {
            let decl_annotation = member_annotation(&decl, span.clone(), ctx);
            member_ident_expr(decl_annotation, ident, ctx)
        },

        ScopeMemberRef::Decl {
            value: Decl::GlobalConst { val, .. } | Decl::LocalConst { val, .. }, ..
        } => {
            let value = member_annotation(&decl, span.clone(), ctx);

            Ok(Expr::literal(val.clone(), value))
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
                semantic_hint: binding.semantic_hint,
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
                semantic_hint: SemanticHint::Variable,
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
                
                let sig = decl.sig();

                Value::from(FunctionValue::new(
                    func_sym,
                    *visibility,
                    decl.clone(),
                    sig,
                    span,
                ))
            } else {
                let candidates: Vec<_> = overloads
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
                
                Value::from(OverloadValue {
                    span,
                    candidates,
                    self_arg: None,
                    sig: None,
                })
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

