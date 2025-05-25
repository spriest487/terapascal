use crate::ast;
use crate::ast::TypeAnnotation;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_stmt;
use crate::typ::seq::TypeSequenceError;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::typecheck_type;
use crate::typ::Binding;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::ValueKind;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type ForLoop = ast::ForLoop<Value>;
pub type WhileLoop = ast::WhileLoop<Value>;

const DEFAULT_COUNTER_TY: Type = Type::Primitive(Primitive::Int32);

pub fn typecheck_for_loop(
    for_loop: &ast::ForLoop<Span>,
    ctx: &mut Context,
) -> TypeResult<ForLoop> {
    let annotation = Value::Untyped(for_loop.annotation.clone());

    let inner_scope = ctx.push_scope(Environment::Block {
        allow_unsafe: false,
    });
    
    let range = match &for_loop.range {
        ast::ForLoopRange::UpTo(range) => {
            let (init, counter_ty) = match &range.init {
                ast::ForLoopCounterInit::Binding { name, init, ty } => {
                    let expect_counter_ty = if ty.is_known() {
                        typecheck_type(ty, ctx)?
                    } else {
                        Type::from(DEFAULT_COUNTER_TY)
                    };
                    
                    let init_expr = typecheck_expr(init, &expect_counter_ty, ctx)?;
                    let counter_ty = init_expr.annotation().ty().into_owned();

                    let binding = Binding {
                        kind: ValueKind::Mutable,
                        ty: counter_ty.clone(),
                        def: Some(name.clone()),
                    };
                    
                    ctx.declare_local_var(name.clone(), binding)?;
                    
                    let init = ast::ForLoopCounterInit::Binding {
                        ty: counter_ty.clone(),
                        init: init_expr,
                        name: name.clone(),
                    };

                    (init, counter_ty)
                },

                ast::ForLoopCounterInit::Assignment { counter, value } => {
                    let counter = typecheck_expr(counter, &DEFAULT_COUNTER_TY, ctx)?;
                    if let ast::Expr::Ident(ident, ..) = &counter {
                        if ctx.get_decl_scope(ident).is_some() {
                            ctx.initialize(ident)
                        }
                    }

                    let counter_ty = counter.annotation().ty().into_owned();
                    let value = typecheck_expr(value, &counter_ty, ctx)?;
                    let init = ast::ForLoopCounterInit::Assignment {
                        counter: Box::new(counter),
                        value: Box::new(value),
                    };

                    (init, counter_ty)
                },
            };

            if !counter_ty
                .as_primitive()
                .map(|p| p.is_integer())
                .unwrap_or(false)
            {
                return Err(TypeError::InvalidLoopCounterType {
                    counter_ty,
                    span: annotation.span().clone(),
                });
            }

            let to_expr = typecheck_expr(&range.to_expr, &counter_ty, ctx)?;
            to_expr.annotation().expect_value(&counter_ty)?;
            
            ast::ForLoopRange::UpTo(ast::ForLoopCounterRange {
                init,
                to_expr,
            })
        }

        ast::ForLoopRange::InSequence(range) => {
            let expect_element_ty = if range.binding_ty.is_known() {
                typecheck_type(&range.binding_ty, ctx)?
            } else {
                DEFAULT_COUNTER_TY
            };

            let expect_ty = match &range.src_expr {
                ast::Expr::CollectionCtor(ctor) => {
                    expect_element_ty.array(ctor.elements.len())
                }
                _ => {
                    expect_element_ty.dyn_array()
                }
            };

            let seq_expr = typecheck_expr(&range.src_expr, &expect_ty, ctx)?;
            let seq_span = seq_expr.span();

            // for now, the range must be an array or dynarray
            let binding_ty = match seq_expr.annotation().ty().as_ref() {
                Type::Array(array_ty) => array_ty.element_ty.clone(),
                Type::DynArray { element } => (**element).clone(),
                
                ty => {
                    match TypeSequenceSupport::try_from_type(ty, ctx) {
                        Err(TypeSequenceError::MethodNotFound) => {
                            return Err(TypeError::InvalidLoopSeqType {
                                seq_ty: ty.clone(),
                                span: seq_expr.span().clone(),
                            })
                        }
                        
                        Err(TypeSequenceError::MethodNotAccessible(ty, ident, access)) => {
                            return Err(TypeError::TypeMemberInaccessible {
                                ty,
                                member: ident,
                                access,
                                span: seq_expr.span().clone(),
                            })
                        }
                        
                        Err(TypeSequenceError::Error(err)) => {
                            return Err(TypeError::from_name_err(err, seq_span.clone()));
                        }

                        Err(TypeSequenceError::AmbiguousSequenceMethod(candidates)) => {
                            return Err(TypeError::AmbiguousFunction {
                                candidates,
                                span: seq_span.clone(),
                            });
                        }
                        
                        Err(TypeSequenceError::AmbiguousNextMethod(candidates)) => {
                            return Err(TypeError::AmbiguousFunction {
                                candidates,
                                span: seq_span.clone(),
                            });
                        }

                        Ok(seq_support) => {
                            seq_support.item_type
                        }
                    }
                }
            };

            let binding = Binding {
                kind: ValueKind::Immutable,
                ty: binding_ty.clone(),
                def: Some(range.binding_name.clone()),
            };

            ctx.declare_local_var(range.binding_name.clone(), binding)?;
            
            ast::ForLoopRange::InSequence(ast::ForLoopSequenceRange {
                binding_ty,
                binding_name: range.binding_name.clone(),
                src_expr: seq_expr,
            })
        }
    };

    // loops bodies never have values
    let body_expect_ty = Type::Nothing;

    ctx.push_loop(for_loop.span().clone());
    let body = typecheck_stmt(&for_loop.body, &body_expect_ty, ctx).map(Box::new)?;
    ctx.pop_loop();

    ctx.pop_scope(inner_scope);

    Ok(ForLoop {
        range,
        body,
        annotation,
    })
}

pub fn typecheck_while_loop(
    while_loop: &ast::WhileLoop<Span>,
    ctx: &mut Context,
) -> TypeResult<WhileLoop> {
    let annotation = Value::Untyped(while_loop.span().clone());

    let bool_ty = Type::Primitive(Primitive::Boolean);
    let condition = typecheck_expr(&while_loop.condition, &bool_ty, ctx)?;

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
    })
}
