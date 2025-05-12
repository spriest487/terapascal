#[cfg(test)]
mod test;

use crate::ast;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::{create_default_literal, Expr};
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_type_args;
use crate::typ::ArrayType;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use crate::typ::Context;
use common::span::Span;
use common::span::Spanned;
use linked_hash_map::LinkedHashMap;
use std::iter;
use std::rc::Rc;

pub type ObjectCtor = ast::ObjectCtor<Value>;
pub type ObjectCtorMember = ast::ObjectCtorMember<Value>;
pub type ObjectCtorArgs = ast::ObjectCtorArgs<Value>;
pub type CollectionCtor = ast::CollectionCtor<Value>;
pub type CollectionCtorElement = ast::CollectionCtorElement<Value>;

pub fn typecheck_object_ctor(
    ctor: &ast::ObjectCtor<Span>,
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<ObjectCtor> {
    let ty_args = match &ctor.type_args {
        Some(list) => Some(typecheck_type_args(list, ctx)?),
        None => None,
    };
    
    let type_expr = match &ctor.type_expr {
        Some(expr) => Some(typecheck_expr(expr, &Type::Nothing, ctx)?),
        None => None,
    };

    let ctor_ty = find_ctor_ty(expect_ty, ty_args.as_ref(), type_expr.as_ref(), &span, ctx)?;
    let ctor_ty_name = ctor_ty
        .full_name()
        .ok_or_else(|| TypeError::InvalidCtorType {
            ty: ctor_ty.clone(),
            span: span.clone(),
        })?;

    if ctor_ty.is_unspecialized_generic() {
        let err = GenericError::CannotInferArgs {
            target: GenericTarget::Type(ctor_ty),
            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
        };

        return Err(TypeError::NameError {
            span: span.clone(),
            err: NameError::GenericError(err),
        });
    }

    if !ctx.is_visible(&ctor_ty_name.full_path) {
        return Err(TypeError::NameNotVisible {
            name: ctor_ty_name.into_owned().full_path,
            span,
        });
    }

    let mut expect_fields: LinkedHashMap<_, _> = ctor_ty
        .fields(ctx)
        .map_err(|err| TypeError::NameError {
            err,
            span: span.clone(),
        })?
        .into_iter()
        .map(|member| (member.ident, (member.ty, member.span, member.access)))
        .collect();

    let mut fields: Vec<ObjectCtorMember> = Vec::new();

    for arg in &ctor.args.members {
        // check for duplicate items for the same field
        let find_prev = fields
            .iter()
            .find(|a| a.ident == arg.ident);

        if let Some(prev) = find_prev {
            return Err(TypeError::DuplicateNamedArg {
                name: arg.ident.clone(),
                span: arg.span().clone(),
                previous: prev.span().clone(),
            });
        }

        let (member_ty, _member_span, member_access) = match expect_fields.remove(&arg.ident) {
            Some(member) => member,
            None => {
                // ctor has a named argument which doesn't exist in the type
                let err = NameError::MemberNotFound {
                    base: NameContainer::Type(ctor_ty),
                    member: arg.ident.clone(),
                };
                return Err(TypeError::NameError {
                    span: arg.span().clone(),
                    err,
                });
            },
        };
        
        if ctor_ty.get_current_access(ctx) < member_access {
            return Err(TypeError::TypeMemberInaccessible {
                span: arg.span.clone(),
                access: member_access,
                ty: ctor_ty,
                member: arg.ident.clone(),
            });
        }

        let value = implicit_conversion(
            typecheck_expr(&arg.value, &member_ty, ctx)?,
            &member_ty,
            ctx,
        )?;

        fields.push(ObjectCtorMember {
            ident: arg.ident.clone(),
            value,
            span: arg.span.clone(),
        });
    }

    // any remaining members must have valid default values
    let mut missing_members = Vec::new();
    for (member_ident, (member_ty, member_span, _)) in expect_fields {
        let has_default = member_ty
            .has_default(ctx)
            .map_err(|e| TypeError::from_name_err(e, member_span.clone()))?;

        if has_default {
            fields.push(ObjectCtorMember {
                ident: member_ident,
                span: member_span.clone(),
                value: create_default_literal(member_ty, ctor.annotation.clone()),
            });
        } else {
            missing_members.push(member_ident);
        }
    }

    if !missing_members.is_empty() {
        return Err(TypeError::CtorMissingMembers {
            ctor_ty,
            span: ctor.annotation.clone(),
            members: missing_members,
        });
    }

    let args = ObjectCtorArgs {
        span: ctor.args.span.clone(),
        members: fields,
    };

    let annotation = TypedValue {
        ty: ctor_ty,
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(ObjectCtor {
        type_expr,
        args,
        type_args: ty_args,
        annotation,
    })
}

fn find_ctor_ty(
    expect_ty: &Type,
    ty_args: Option<&TypeArgList>,
    type_expr: Option<&Expr>,
    span: &Span,
    ctx: &mut Context
) -> TypeResult<Type>  {
    let Some(type_expr) = type_expr else {
        return Ok(expect_ty.clone());
    };
    
    match type_expr.annotation() {
        Value::Type(generic_ty, _) => {
            match ty_args {
                None => {
                    // infer the type args from the expected type of the expression
                    let spec_ty = generic_ty
                        .infer_specialized_from_hint(expect_ty)
                        .ok_or_else(|| {
                            // eprintln!("specialization failed:\n{:#?}\ninto:\n{:#?}", generic_ty, expect_ty);

                            TypeError::from_generic_err(GenericError::CannotInferArgs {
                                target: GenericTarget::Type(generic_ty.clone()),
                                hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                            }, span.clone())
                        })?
                        .clone();

                    Ok(spec_ty)
                },

                Some(args) => {
                    let spec_ty = generic_ty
                        .specialize(args, ctx)
                        .map_err(|err| TypeError::from_generic_err(err, span.clone()))?
                        .into_owned();

                    Ok(spec_ty)
                },
            }
        }
        
        other => {
            Err(TypeError::InvalidCtorType {
                ty: other.ty().into_owned(),
                span: span.clone(),
            })
        }
    }
}

pub fn typecheck_collection_ctor(
    ctor: &ast::CollectionCtor<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<CollectionCtor> {
    let (collection_ty, elements) = match expect_ty {
        // any dynamic array - creating a dynamic array
        Type::DynArray { .. } => {
            let (elements, element_ty) = array_ctor_elements(expect_ty, ctor, ctx)?;
            let array_ty = Type::DynArray {
                element: Rc::new(element_ty),
            };

            (array_ty, elements)
        },

        // any static array - creating a static array of known length
        Type::Array(array_ty) => {
            let (mut elements, element_ty) = array_ctor_elements(expect_ty, ctor, ctx)?;
            default_fill_elements(
                array_ty.dim,
                &element_ty,
                &mut elements,
                ctx,
                ctor.annotation.span()
            )?;

            let array_ty = ArrayType::new(element_ty, elements.len()).into();

            (array_ty, elements)
        }
        
        // any set - creating that set
        Type::Set(set_type) => {
            let element_type = &set_type.item_type;
            let elements = collection_ctor_elements(ctor, &element_type, ctx)?;

            (expect_ty.clone(), elements)
        }

        // unknown/no type hint - this ctor is creating a static array of exactly these elements
        _ => {
            let (elements, element_ty) = array_ctor_elements(expect_ty, ctor, ctx)?;
            let array_ty = ArrayType::new(element_ty, elements.len()).into();

            (array_ty, elements)
        },
    };

    let annotation = TypedValue::temp(collection_ty, ctor.annotation.clone());

    Ok(CollectionCtor {
        elements,
        annotation: Value::from(annotation),
    })
}

fn array_ctor_elements(
    expect_ty: &Type,
    ctor: &ast::CollectionCtor<Span>,
    ctx: &mut Context
) -> TypeResult<(Vec<CollectionCtorElement>, Type)> {
    let (elements, element_ty) = match expect_ty.element_ty() {
        Some(elem_ty) if !elem_ty.contains_unresolved_params(ctx) => {
            let elements = collection_ctor_elements(ctor, elem_ty, ctx)?;
            (elements, elem_ty.clone())
        },

        _ => {
            let elements = elements_for_inferred_ty(ctor, ctx)?;
            let elem_ty = elements[0].value.annotation().ty().into_owned();
            (elements, elem_ty)
        },
    };
    
    Ok((elements, element_ty))
}

fn elements_for_inferred_ty(
    ctor: &ast::CollectionCtor<Span>,
    ctx: &mut Context,
) -> TypeResult<Vec<CollectionCtorElement>> {
    // must have at least one element to infer types
    if ctor.elements.is_empty() {
        return Err(TypeError::UnableToInferType {
            expr: Box::new(ast::Expr::from(ctor.clone())),
        });
    }

    let mut elements = Vec::new();
    let first_element_val = typecheck_expr(&ctor.elements[0].value, &Type::Nothing, ctx)?;
    let expected_ty = first_element_val.annotation().ty().into_owned();

    elements.push(CollectionCtorElement {
        value: first_element_val,
    });

    for e in ctor.elements.iter().skip(1) {
        let element = typecheck_expr(&e.value, &expected_ty, ctx)?;
        element.annotation().expect_value(&expected_ty)?;

        elements.push(CollectionCtorElement { value: element });
    }

    Ok(elements)
}

pub fn collection_ctor_elements(
    ctor: &ast::CollectionCtor<Span>,
    expect_element_type: &Type,
    ctx: &mut Context,
) -> TypeResult<Vec<CollectionCtorElement>> {
    let mut elements = Vec::new();
    for e in &ctor.elements {
        let value = typecheck_expr(&e.value, expect_element_type, ctx)?;
        value.annotation().expect_value(&expect_element_type)?;

        elements.push(CollectionCtorElement { value });
    }

    Ok(elements)
}

fn default_fill_elements(
    expect_dim: usize,
    element_ty: &Type,
    elements: &mut Vec<CollectionCtorElement>,
    ctx: &Context,
    span: &Span
) -> TypeResult<()> {
    if expect_dim <= elements.len() {
        return Ok(());
    }

    let has_default = element_ty
        .has_default(ctx)
        .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

    if has_default {
        let default_count = expect_dim - elements.len();
        let default_val = create_default_literal(element_ty.clone(), span.clone());
        
        let default_element = CollectionCtorElement { value: default_val.clone() };

        let default_elements = iter::repeat(default_element)
            .take(default_count);

        elements.extend(default_elements);
    }
    
    Ok(())
}
