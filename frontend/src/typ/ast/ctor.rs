#[cfg(test)]
mod test;

use crate::ast;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::create_default_literal;
use crate::typ::ast::evaluate_expr;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_type_args;
use crate::typ::ast::Expr;
use crate::typ::ArrayType;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::Invocation;
use crate::typ::NameError;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeError;
use crate::typ::TypeName;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use linked_hash_map::LinkedHashMap;
use std::iter;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

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
    let (ctor_ty, type_expr, type_args) = typecheck_object_ctor_type(
        ctor.type_expr.as_ref(),
        ctor.type_args.as_ref(),
        expect_ty,
        &span,
        ctx,
    )?;

    let args = typecheck_object_ctor_args(&ctor_ty, &ctor.annotation, &ctor.args, ctx)?;

    let invocation = Value::from(Invocation::ObjectCtor {
        span: span.clone(),
        object_type: ctor_ty.clone(),
        type_args: type_args.clone(),
        members: args.members.clone(),
    });

    Ok(ObjectCtor {
        type_expr,
        args,
        type_args,
        annotation: invocation,
    })
}

pub fn typecheck_object_ctor_args(
    ctor_ty: &Type,
    ctor_span: &Span,
    src: &ast::ObjectCtorArgs,
    ctx: &mut Context,
) -> TypeResult<ObjectCtorArgs> {
    let ty_fields = ctor_ty.fields(ctx).map_err(|err| TypeError::NameError {
        err,
        span: ctor_span.clone(),
    })?;

    let mut expect_fields: LinkedHashMap<_, _> = ty_fields
        .into_iter()
        .flat_map(|member| {
            member.idents.into_iter().map(move |ident| {
                (
                    ident,
                    (member.ty.clone(), member.span.clone(), member.access),
                )
            })
        })
        .collect();

    let mut members: Vec<ObjectCtorMember> = Vec::new();

    for member in &src.members {
        // check for duplicate items for the same field
        let find_prev = members.iter().find(|a| a.ident == member.ident);

        if let Some(prev) = find_prev {
            return Err(TypeError::DuplicateNamedArg {
                name: member.ident.clone(),
                span: member.span().clone(),
                previous: Some(prev.span().clone()),
            });
        }

        let (member_ty, _member_span, member_access) = match expect_fields.remove(&member.ident) {
            Some(member) => member,
            None => {
                // ctor has a named argument which doesn't exist in the type
                return Err(TypeError::from_name_err(
                    NameError::type_member_not_found(ctor_ty.clone(), member.ident.clone()),
                    member.span().clone(),
                ));
            },
        };

        if ctor_ty.get_current_access(ctx) < member_access {
            return Err(TypeError::TypeMemberInaccessible {
                span: member.span.clone(),
                access: member_access,
                ty: ctor_ty.clone(),
                member: member.ident.clone(),
            });
        }

        let val_expr = evaluate_expr(&member.value, &member_ty, ctx)?;

        let value = implicit_conversion(
            val_expr,
            &member_ty,
            ctx,
        )?;

        let member = ObjectCtorMember {
            ident: member.ident.clone(),
            value,
            span: member.span.clone(),
        };

        members.push(member);
    }

    // any remaining members must have valid default values
    let mut missing_members = Vec::new();
    for (member_ident, (member_ty, member_span, _)) in expect_fields {
        let has_default = member_ty
            .has_default(ctx)
            .map_err(|e| TypeError::from_name_err(e, member_span.clone()))?;

        if has_default {
            let default_typename = TypeName::inferred(member_ty);
            
            members.push(ObjectCtorMember {
                ident: member_ident,
                span: member_span.clone(),
                value: create_default_literal(default_typename, ctor_span.clone()),
            });
        } else {
            missing_members.push(member_ident);
        }
    }

    if !missing_members.is_empty() {
        return Err(TypeError::CtorMissingMembers {
            ctor_ty: ctor_ty.clone(),
            span: ctor_span.clone(),
            members: missing_members,
        });
    }

    let args = ObjectCtorArgs {
        span: src.span.clone(),
        members,
    };

    Ok(args)
}

fn typecheck_object_ctor_type(
    type_expr: Option<&ast::Expr>,
    type_args: Option<&ast::TypeArgList>,
    expect_ty: &Type,
    ctor_span: &Span,
    ctx: &mut Context,
) -> TypeResult<(TypeName, Option<Expr>, Option<TypeArgList>)> {
    let ty_args = match type_args {
        Some(list) => Some(typecheck_type_args(list, ctx)?),
        None => None,
    };

    let type_expr = match type_expr {
        Some(expr) => Some(typecheck_expr(expr, &Type::Nothing, ctx)?),
        None => None,
    };

    let ctor_ty = find_ctor_ty(
        expect_ty,
        ty_args.as_ref(),
        type_expr.as_ref(),
        ctor_span,
        ctx,
    )?;

    let Some(ctor_ty_name) = ctor_ty.full_name() else {
        return Err(TypeError::InvalidCtorType {
            ty: ctor_ty.into(),
            span: ctor_span.clone(),
        });
    };

    if ctor_ty.is_unspecialized_generic() {
        let err = GenericError::CannotInferArgs {
            target: GenericTarget::Type(ctor_ty.into()),
            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
        };

        return Err(TypeError::NameError {
            span: ctor_span.clone(),
            err: NameError::GenericError(err),
        });
    }

    if !ctx.is_visible(&ctor_ty_name.full_path) {
        return Err(TypeError::NameNotVisible {
            name: ctor_ty_name.into_owned().full_path,
            span: ctor_span.clone(),
        });
    }

    Ok((ctor_ty, type_expr, ty_args))
}

fn find_ctor_ty(
    expect_ty: &Type,
    ty_args: Option<&TypeArgList>,
    type_expr: Option<&Expr>,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<TypeName> {
    let Some(type_expr) = type_expr else {
        return Ok(TypeName::Inferred(expect_ty.clone()));
    };

    // the target expression of a call evaluated as a constructor must name a Type
    let Value::Type(generic_ty, _) = type_expr.annotation() else {
        return Err(TypeError::InvalidCtorType {
            ty: type_expr.annotation().ty().into_owned(),
            span: span.clone(),
        });
    };

    match ty_args {
        None if generic_ty.is_unspecialized_generic() => {
            // infer the type args from the expected type of the expression
            let spec_ty = generic_ty
                .infer_specialized_from_hint(expect_ty)
                .ok_or_else(|| {
                    // eprintln!("specialization failed:\n{:#?}\ninto:\n{:#?}", generic_ty, expect_ty);

                    TypeError::from_generic_err(
                        GenericError::CannotInferArgs {
                            target: GenericTarget::Type(generic_ty.clone()),
                            hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                        },
                        span.clone(),
                    )
                })?
                .clone();

            Ok(TypeName::named(spec_ty, type_expr.span().clone()))
        },

        None => Ok(TypeName::named(
            generic_ty.clone(),
            type_expr.span().clone(),
        )),

        Some(args) => {
            let spec_ty = generic_ty
                .specialize(args, ctx)
                .map_err(|err| TypeError::from_generic_err(err, span.clone()))?
                .into_owned();

            Ok(TypeName::named(spec_ty, type_expr.span().clone()))
        },
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
                element: Arc::new(element_ty),
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
                ctor.annotation.span(),
            )?;

            let array_ty = ArrayType::new(element_ty, elements.len()).into();

            (array_ty, elements)
        },

        // any set - creating that set
        Type::Set(set_type) => {
            let element_type = &set_type.item_type;
            let elements = collection_ctor_elements(ctor, &element_type, ctx)?;

            (expect_ty.clone(), elements)
        },

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
    ctx: &mut Context,
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
    let first_element_val = evaluate_expr(&ctor.elements[0].value, &Type::Nothing, ctx)?;
    let expected_ty = first_element_val.annotation().ty().into_owned();

    elements.push(CollectionCtorElement {
        value: first_element_val,
    });

    for e in ctor.elements.iter().skip(1) {
        let element = evaluate_expr(&e.value, &expected_ty, ctx)?;
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
        let value = evaluate_expr(&e.value, expect_element_type, ctx)?;
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
    span: &Span,
) -> TypeResult<()> {
    if expect_dim <= elements.len() {
        return Ok(());
    }

    let has_default =
        element_ty.has_default(ctx).map_err(|e| TypeError::from_name_err(e, span.clone()))?;

    if has_default {
        let default_count = expect_dim - elements.len();
        
        let default_typename = TypeName::inferred(element_ty.clone()); 
        let default_val = create_default_literal(default_typename, span.clone());

        let default_element = CollectionCtorElement {
            value: default_val.clone(),
        };

        let default_elements = iter::repeat(default_element).take(default_count);

        elements.extend(default_elements);
    }

    Ok(())
}
