use crate::ast;
use crate::ast::SemanticHint;
use crate::typ::ast::evaluate_expr;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::typecheck_stmt;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::typecheck_typename;
use crate::typ::Binding;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeName;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::ValueKind;
use crate::Operator;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type ForLoop = ast::ForLoop<Value>;
pub type WhileLoop = ast::WhileLoop<Value>;

const DEFAULT_COUNTER_TY: Type = Type::Primitive(Primitive::Int32);

pub fn typecheck_for_loop(for_loop: &ast::ForLoop<Span>, ctx: &mut Context) -> TypeResult<ForLoop> {
    let annotation = Value::Untyped(for_loop.annotation.clone());

    let inner_scope = ctx.push_scope(Environment::Block {
        allow_unsafe: false,
    });

    let range = match &for_loop.range {
        ast::ForLoopRange::UpTo(range) => {
            let (init, counter_ty) = match &range.init {
                ast::ForLoopCounterInit::Binding {
                    binding_kw_span,
                    name,
                    init,
                    ty,
                    assign_op_span,
                } => {
                    let (counter_ty, init_expr) = if ty.is_known() {
                        let counter_ty = typecheck_typename(ty, ctx)?;
                        let mut init_expr = evaluate_expr(init, counter_ty.ty(), ctx)?;

                        init_expr = implicit_conversion(init_expr, counter_ty.ty(), ctx)?;

                        (counter_ty, init_expr)
                    } else {
                        let init_expr = evaluate_expr(init, &DEFAULT_COUNTER_TY, ctx)?;
                        let counter_ty =
                            TypeName::inferred(init_expr.annotation().ty().into_owned());

                        (counter_ty, init_expr)
                    };

                    let binding = Binding {
                        kind: ValueKind::Mutable,
                        ty: counter_ty.ty().clone(),
                        def: Some(name.clone()),
                        semantic_hint: SemanticHint::Variable,
                    };

                    ctx.declare_local_var(name.clone(), binding)?;

                    let init = ast::ForLoopCounterInit::Binding {
                        binding_kw_span: binding_kw_span.clone(),
                        ty: counter_ty.clone(),
                        init: init_expr,
                        name: name.clone(),
                        assign_op_span: assign_op_span.clone(),
                    };

                    (init, counter_ty)
                },

                ast::ForLoopCounterInit::Assignment {
                    counter,
                    value,
                    assign_op_span,
                } => {
                    let counter = evaluate_expr(counter, &DEFAULT_COUNTER_TY, ctx)?;
                    if let ast::Expr::Ident(ident, ..) = &counter {
                        if ctx.get_decl_scope(ident).is_some() {
                            ctx.initialize(ident)
                        }
                    }

                    let counter_ty = counter.annotation().ty().into_owned();
                    let value = evaluate_expr(value, &counter_ty, ctx)?;
                    let init = ast::ForLoopCounterInit::Assignment {
                        counter: Box::new(counter),
                        value: Box::new(value),
                        assign_op_span: assign_op_span.clone(),
                    };

                    (init, TypeName::inferred(counter_ty))
                },
            };

            if !counter_ty.ty().as_primitive().map(|p| p.is_integer()).unwrap_or(false) {
                return Err(TypeError::InvalidLoopCounterType {
                    counter_ty: counter_ty.ty().clone(),
                    span: annotation.span().clone(),
                });
            }

            let to_expr = evaluate_expr(&range.to_expr, counter_ty.ty(), ctx)?;
            to_expr.annotation().expect_value(counter_ty.ty())?;

            ast::ForLoopRange::UpTo(ast::ForLoopCounterRange {
                init,
                to_expr,
                to_kw_span: range.to_kw_span.clone(),
            })
        },

        ast::ForLoopRange::InSequence(range) => {
            let mut binding_ty = if range.binding_ty.is_known() {
                typecheck_typename(&range.binding_ty, ctx)?
            } else {
                TypeName::inferred(Type::Nothing)
            };

            let expect_src_ty = match &range.src_expr {
                ast::Expr::CollectionCtor(ctor) => {
                    binding_ty.ty().clone().array(ctor.elements.len())
                },
                _ => binding_ty.ty().clone().dyn_array(),
            };

            let mut seq_expr = evaluate_expr(&range.src_expr, &expect_src_ty, ctx)?;

            // either validate that the sequence can produce elements of the explicit binding type,
            // or infer that binding type from the elements produced by the sequence type
            match (
                seq_expr.annotation().ty().as_ref(),
                binding_ty.is_known(),
            ) {
                (Type::Array(array_ty), true) => {
                    binding_ty = TypeName::inferred(array_ty.element_ty.clone());
                },

                (Type::Array(array_ty), false) => {
                    let binding_array_ty = binding_ty.ty().clone().array(array_ty.dim);
                    seq_expr = implicit_conversion(seq_expr, &binding_array_ty, ctx)?;
                },

                (Type::DynArray { element }, true) => {
                    binding_ty = TypeName::inferred((**element).clone());
                },

                (Type::DynArray { .. }, false) => {
                    let binding_dyn_array_ty = binding_ty.ty().clone().dyn_array();
                    seq_expr = implicit_conversion(seq_expr, &binding_dyn_array_ty, ctx)?
                },

                (ty, inferred) => {
                    let element_ty = match TypeSequenceSupport::try_from_type(ty, ctx) {
                        Err(err) => {
                            return Err(TypeError::InvalidLoopSeqType {
                                target_ty: ty.clone(),
                                span: seq_expr.span().clone(),
                                err,
                            })
                        },

                        Ok(seq_support) => seq_support.item_type,
                    };

                    if inferred {
                        binding_ty = TypeName::inferred(element_ty);
                    } else {
                        if *binding_ty.ty() != element_ty {
                            return Err(TypeError::InvalidBinOp {
                                span: seq_expr.span().clone(),
                                op: Operator::Assignment,
                                lhs: binding_ty.into(),
                                rhs: element_ty,
                            });
                        }
                    }
                },
            };

            let binding = Binding {
                kind: ValueKind::Immutable,
                ty: binding_ty.ty().clone(),
                def: Some(range.binding_name.clone()),
                semantic_hint: SemanticHint::Variable,
            };

            ctx.declare_local_var(range.binding_name.clone(), binding)?;

            ast::ForLoopRange::InSequence(ast::ForLoopSequenceRange {
                binding_kw_span: range.binding_kw_span.clone(),
                binding_ty,
                binding_name: range.binding_name.clone(),
                in_kw_span: range.in_kw_span.clone(),
                src_expr: seq_expr,
            })
        },
    };

    // loops bodies never have values
    let body_expect_ty = Type::Nothing;

    ctx.push_loop(for_loop.span().clone());
    let body = typecheck_stmt(&for_loop.body, &body_expect_ty, ctx).map(Box::new)?;
    ctx.pop_loop();

    ctx.pop_scope(inner_scope);

    Ok(ForLoop {
        for_kw_span: for_loop.for_kw_span.clone(),
        range,
        body,
        annotation,
        do_kw_span: for_loop.do_kw_span.clone(),
    })
}

pub fn typecheck_while_loop(
    while_loop: &ast::WhileLoop<Span>,
    ctx: &mut Context,
) -> TypeResult<WhileLoop> {
    let annotation = Value::Untyped(while_loop.span().clone());

    let bool_ty = Type::Primitive(Primitive::Boolean);
    let condition = evaluate_expr(&while_loop.condition, &bool_ty, ctx)?;

    condition.annotation().expect_value(&bool_ty)?;

    // loops bodies never have values
    let body_expect_ty = Type::Nothing;

    ctx.push_loop(while_loop.span().clone());
    let body = typecheck_stmt(&while_loop.body, &body_expect_ty, ctx).map(Box::new)?;
    ctx.pop_loop();

    Ok(WhileLoop {
        condition,
        body,
        annotation,
        while_kw_span: while_loop.while_kw_span.clone(),
        do_kw_span: while_loop.do_kw_span.clone(),
    })
}
