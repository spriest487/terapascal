#[cfg(test)]
mod test;

mod variant_case;

use crate::ast;
use crate::ast::{Ident, IncompleteExpr};
use crate::ast::IdentPath;
use crate::ast::Literal;
use crate::ast::Operator;
use crate::ast::SemanticHint;
use crate::typ::annotation::UfcsValue;
use crate::typ::ast::{collection_ctor_elements};
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::evaluate_expr;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::member_annotation;
use crate::typ::ast::op::variant_case::typecheck_variant_type_member;
use crate::typ::ast::op::variant_case::VariantTypeMemberValue;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::SetDecl;
use crate::typ::builtin_displayable_name;
use crate::typ::builtin_string_name;
use crate::typ::function::FunctionValue;
use crate::typ::method::MethodValue;
use crate::typ::overload::OverloadValue;
use crate::typ::ConstValue;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::FunctionSigParam;
use crate::typ::InstanceMember;
use crate::typ::Invocation;
use crate::typ::NameError;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeMember;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::ValueKind;
use crate::typ::DISPLAYABLE_TOSTRING_METHOD;
use crate::typ::STRING_CONCAT_FUNC_NAME;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::IntConstant;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use crate::typ::completion::CompletionContext;

pub type BinOp = ast::BinOp<Value>;

fn invalid_bin_op(bin_op: &ast::BinOp<Span>, lhs: &Expr, rhs: &Expr) -> TypeError {
    TypeError::InvalidBinOp {
        lhs: lhs.annotation().ty().into_owned(),
        rhs: rhs.annotation().ty().into_owned(),
        op: bin_op.op,
        span: bin_op.annotation.span().clone(),
    }
}

pub fn typecheck_bin_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let span = &bin_op.annotation;

    match &bin_op.op {
        Operator::Period => typecheck_member_op(bin_op, expect_ty, ctx),

        Operator::Index => typecheck_indexer(bin_op, ctx),

        Operator::In => typecheck_in_set_operator(bin_op, ctx),

        Operator::And | Operator::Or => {
            let bin_op = typecheck_logical_op(bin_op, span.clone(), ctx)?;
            Ok(Expr::from(bin_op))
        }

        Operator::Equals | Operator::NotEquals => {
            let bin_op = typecheck_equality(bin_op, span.clone(), ctx)?;
            Ok(Expr::from(bin_op))
        }

        Operator::Gt | Operator::Gte | Operator::Lt | Operator::Lte => {
            let bin_op = typecheck_comparison(bin_op, span.clone(), ctx)?;
            Ok(Expr::from(bin_op))
        }

        Operator::BitAnd | Operator::BitOr | Operator::Shr | Operator::Shl | Operator::Caret => {
            let bin_op = typecheck_bitwise_op(bin_op, expect_ty, ctx)?;
            Ok(Expr::from(bin_op))
        }

        Operator::Assignment => {
            unreachable!("typecheck_bin_op: assignment should never be checked as expression, only a statement")
        }

        _ => {
            let mut lhs = evaluate_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let mut rhs = evaluate_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;

            // if both operands are strings, this will actually call System.StringConcat
            let string_ty = Type::class(builtin_string_name());

            let mut lhs_string = *lhs.annotation().ty() == string_ty;
            let mut rhs_string = *rhs.annotation().ty() == string_ty;

            // if one operand is a string, and the other can be converted using ToString, do that
            if lhs_string && !rhs_string {
                if let Some(rhs_to_string) = desugar_to_string(&rhs, &span, ctx) {
                    rhs = rhs_to_string;
                    rhs_string = true;
                }
            } else if !lhs_string && lhs_string {
                if let Some(lhs_to_string) = desugar_to_string(&lhs, &span, ctx) {
                    lhs = lhs_to_string;
                    lhs_string = true;
                }
            }

            let is_string_concat = bin_op.op == Operator::Add && lhs_string && rhs_string;

            // check valid ops etc, result type etc
            let lhs_ty = lhs.annotation().ty();

            let result_ty = if is_string_concat {
                string_ty.clone()
            } else {
                lhs_ty
                    .arithmetic_op_result(bin_op.op, rhs.annotation().ty().as_ref())
                    .ok_or_else(|| {
                        invalid_bin_op(&bin_op, &lhs, &rhs)
                    })?
                    .into_owned()
            };

            let annotation = match result_ty {
                Type::Nothing => Value::Untyped(span.clone()),
                ty => Value::from(TypedValue::temp(ty, span.clone())),
            };

            let bin_op = ast::BinOp {
                lhs,
                op: bin_op.op,
                op_span: bin_op.op_span.clone(),
                rhs,
                annotation,
            };

            if is_string_concat {
                desugar_string_concat(bin_op, &string_ty, ctx)
            } else {
                Ok(Expr::from(bin_op))
            }
        }
    }
}

fn typecheck_logical_op(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    let bool_ty = Type::Primitive(Primitive::Boolean);

    let lhs = evaluate_expr(&bin_op.lhs, &bool_ty, ctx)?;
    lhs.annotation().expect_value(&bool_ty)?;

    let rhs = evaluate_expr(&bin_op.rhs, &bool_ty, ctx)?;
    rhs.annotation().expect_value(&bool_ty)?;

    let value = TypedValue::temp(bool_ty, span);

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        op_span: bin_op.op_span.clone(),
        annotation: Value::from(value),
    })
}

fn typecheck_equality(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    let lhs = evaluate_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
    lhs.annotation().expect_any_value()?;

    let rhs = evaluate_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;
    rhs.annotation().expect_any_value()?;

    if !lhs.annotation().ty().equatable(&rhs.annotation().ty(), ctx.allow_unsafe()) {
        return Err(invalid_bin_op(bin_op, &lhs, &rhs));
    }

    let value = TypedValue::temp(Primitive::Boolean, span);

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        op_span: bin_op.op_span.clone(),
        annotation: Value::from(value),
    })
}

fn typecheck_comparison(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    let lhs = evaluate_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
    lhs.annotation().expect_any_value()?;

    let rhs = evaluate_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;
    rhs.annotation().expect_value(&lhs.annotation().ty())?;

    if !lhs.annotation().ty().self_orderable() {
        return Err(invalid_bin_op(&bin_op, &lhs, &rhs));
    }

    let value = TypedValue::temp(Primitive::Boolean, span);

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        op_span: bin_op.op_span.clone(),
        annotation: Value::from(value),
    })
}

fn typecheck_bitwise_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    // if there's no expected type for a bitwise op, expect UInt32
    let expect_ty = match expect_ty {
        Type::Nothing => &Type::Primitive(Primitive::UInt32),
        _ => expect_ty,
    };

    let lhs = evaluate_expr(&bin_op.lhs, expect_ty, ctx)?;
    lhs.annotation().expect_any_value()?;

    let lhs_ty = lhs.annotation().ty();

    // for bitwise operations to make sense the lhs and rhs must be the exact same type so insert a
    // conversion here as necessary
    let rhs = implicit_conversion(
        evaluate_expr(&bin_op.rhs, lhs_ty.as_ref(), ctx)?,
        lhs_ty.as_ref(),
        ctx,
    )?;
    rhs.annotation().expect_any_value()?;

    let rhs_ty = rhs.annotation().ty();

    let result_ty = lhs_ty
        .arithmetic_op_result(bin_op.op, &rhs_ty)
        .ok_or_else(|| invalid_bin_op(&bin_op, &lhs, &rhs))?;

    let val = TypedValue::temp(result_ty.into_owned(), bin_op.span().clone());

    Ok(BinOp {
        annotation: Value::from(val),
        lhs,
        rhs,
        op: bin_op.op,
        op_span: bin_op.op_span.clone(),
    })
}

// turn value `x` that implements the displayable (IToString) interface into a call to 
// the `ToString(x)` method of that interface, when `x` is evaluated in a context where 
// a string is expected e.g. string concat
fn desugar_to_string(expr: &Expr, span: &Span, ctx: &Context) -> Option<Expr> {
    let src_ty = expr.annotation().ty();

    let to_string_ident = Ident::new(DISPLAYABLE_TOSTRING_METHOD, span.clone());

    let displayable_ty = Type::interface(builtin_displayable_name().full_path);

    let is_impl = ctx
        .is_implementation(src_ty.as_ref(), &displayable_ty)
        .ok()
        .unwrap_or(false);

    if !is_impl {
        return None;
    }

    let impl_sig = FunctionSig {
        result_ty: Type::class(builtin_string_name()),
        type_params: None,
        params: vec![FunctionSigParam {
            ty: src_ty.clone().into_owned(),
            modifier: None,
        }],
    };

    let (impl_method_index, impl_method_decl) = src_ty
        .find_method(&to_string_ident, &impl_sig, ctx)
        .ok()
        .flatten()?;

    let impl_method_val = MethodValue::new(
        src_ty.clone().into_owned(),
        Some(expr.clone()),
        impl_method_index,
        impl_method_decl,
        span.clone(),
    );

    let mut expr = expr.clone();

    let args = vec![expr.clone()];
    let span = expr.span().clone();

    let invocation = Invocation::Method {
        method: Arc::new(impl_method_val),
        self_ty: src_ty.into_owned(),
        args,
        type_args: None,
        span,
        args_span: None,
    };

    *expr.annotation_mut() = Value::from(invocation);

    Some(expr)
}

// desugar a binary + operation on two strings into a call to System.StringConcat
fn desugar_string_concat(
    mut bin_op: BinOp,
    string_ty: &Type,
    ctx: &Context,
) -> TypeResult<Expr> {
    let span = bin_op.lhs.span().to(bin_op.rhs.span());

    // if LHS and RHS are both string literals, we can concat them ahead of time
    bin_op.annotation = match (bin_op.lhs.annotation(), bin_op.rhs.annotation()) {
        (
            Value::Const(lhs_const),
            Value::Const(rhs_const),
        ) if lhs_const.value.as_string().is_some() && rhs_const.value.as_string().is_some() => {
            let lhs_string = lhs_const.value.as_string().unwrap();
            let rhs_string = rhs_const.value.as_string().unwrap();

            let concat_val = lhs_string.as_ref().clone() + rhs_string.as_ref();
            Value::from(ConstValue {
                value: Literal::String(Arc::new(concat_val)),
                decl: None,
                span,
                ty: string_ty.clone(),
            })
        }

        _ => {
            let system_path = IdentPath::from(Ident::new(SYSTEM_UNIT_NAME, span.clone()));
            let concat_path = system_path.child(Ident::new(STRING_CONCAT_FUNC_NAME, span.clone()));
            let (concat_path, concat_decls) = ctx
                .find_function(&concat_path)
                .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

            assert_eq!(1, concat_decls.len());

            let concat_decl = &concat_decls[0];
            let concat_sym = Symbol::from(concat_path.clone());

            let concat_func_val = FunctionValue::new(
                concat_sym.clone(),
                concat_decl.visiblity(),
                concat_decl.decl().clone(),
                concat_decl.sig().clone(),
                span.clone(),
            );

            let concat_args = vec![bin_op.lhs.clone(), bin_op.rhs.clone()];
            let concat_span = bin_op.span().clone();

            let invocation = Invocation::Function {
                function: Arc::new(concat_func_val),
                args: concat_args,
                args_span: None,
                type_args: None,
                span: concat_span,
            };

            Value::from(invocation)
        }
    };

    Ok(Expr::from(bin_op))
}

fn typecheck_member_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let span = &bin_op.annotation;

    let lhs = evaluate_expr(&bin_op.lhs, &Type::Nothing, ctx)?;

    if ctx.opts().lang_server {
        let completion_span = bin_op.op_span.to(bin_op.rhs.span());
        let context = CompletionContext::new(completion_span, ctx);

        ctx.hint_completion(IncompleteExpr {
            target: Box::new(lhs.clone()),
            completion_op: Some(Operator::Period),
            context,
        });
    }

    match &bin_op.rhs {
        // x.y
        ast::Expr::Ident(member_ident, ..) => {
            let rhs_value = member_value(&lhs, member_ident, expect_ty, ctx)?;
            let op_value = rhs_value.with_span(span.clone());

            let rhs = Expr::Ident(member_ident.clone(), rhs_value);


            let member_op = BinOp {
                lhs,
                op: Operator::Period,
                op_span: bin_op.op_span.clone().into(),
                rhs,
                annotation: op_value,
            };

            Ok(Expr::from(member_op))
        }

        _ => {
            let rhs = typecheck_expr(&bin_op.rhs, &Type::Nothing, ctx)?;

            Err(TypeError::InvalidBinOp {
                lhs: lhs.annotation().ty().into_owned(),
                rhs: rhs.annotation().ty().into_owned(),
                span: span.clone(),
                op: Operator::Period,
            })
        }
    }
}

fn typecheck_type_member(
    ty: &Type,
    member_ident: &Ident,
    expect_return_ty: &Type,
    span: Span,
    ctx: &Context,
) -> TypeResult<Value> {
    let type_member = ctx.find_type_member(
        ty,
        member_ident,
        expect_return_ty,
        &span,
        ctx,
    )?;

    let member_access = type_member.access();
    if ty.get_current_access(ctx) < member_access {
        return Err(TypeError::TypeMemberInaccessible {
            member: member_ident.clone(),
            ty: ty.clone(),
            access: member_access,
            span,
        });
    }

    let annotation = match type_member {
        TypeMember::Method(candidate) => {
            if candidate.iface_ty.get_current_access(ctx) < candidate.method.access {
                return Err(TypeError::TypeMemberInaccessible {
                    ty: candidate.iface_ty.clone(),
                    member: member_ident.clone(),
                    access: candidate.method.access,
                    span,
                });
            }

            // this is a reference to the method itself, args list to follow presumably
            MethodValue::new(candidate.iface_ty, None, candidate.index, candidate.method, span).into()
        }

        TypeMember::MethodGroup(group) => {
            let candidates: Vec<_> = group
                .into_iter()
                .map(|method_group_item| {
                    OverloadCandidate::Method {
                        iface_ty: method_group_item.iface_ty.clone(),
                        self_ty: method_group_item.iface_ty,
                        index: method_group_item.index,

                        decl: method_group_item.method.clone(),
                    }
                })
                .collect();

            OverloadValue::new(candidates, None, span).into()
        }
    };

    Ok(annotation)
}

pub fn member_value(
    base_expr: &Expr,
    member_ident: &Ident,
    expect_ty: &Type,
    ctx: &Context,
) -> TypeResult<Value> {
    let member_span = member_ident.span.clone();

    match base_expr.annotation() {
        // x is the name of a variant type - we are constructing that variant
        Value::Type(Type::Variant(variant_name), variant_name_span) => {
            match typecheck_variant_type_member(variant_name, member_ident, variant_name_span, ctx)? {
                VariantTypeMemberValue::Case(value)
                | VariantTypeMemberValue::Method(value) => {
                    Ok(value)
                }
            }
        }

        // x is a non-variant typename - we are accessing a member of that type
        // e.g. calling an interface method by its type-qualified name
        Value::Type(ty, _span) => {
            typecheck_type_member(ty, &member_ident, expect_ty, member_span, ctx)
        }

        // x is a value - we are accessing a member of that value
        Value::Typed(base_val) => {
            typecheck_member_value(
                &base_expr,
                &base_val.ty,
                base_val.value_kind,
                &member_ident,
                ctx,
            )
        }

        Value::Invocation(invocation) => {
            typecheck_member_value(
                &base_expr,
                invocation.result_type(),
                ValueKind::Temporary,
                &member_ident,
                ctx,
            )
        }

        Value::Namespace(path, _) => {
            let mut full_path = path.clone();
            full_path.push(member_ident.clone());

            match ctx.find_path(&full_path) {
                Some(member) => {
                    let value = member_annotation(&member, member_ident.span.clone(), ctx);

                    Ok(value)
                }
                None => {
                    let err = NameError::value_member_not_found(
                        base_expr.annotation(),
                        member_ident.clone(),
                    );

                    Err(TypeError::from_name_err(err, member_ident.span.clone()))
                }
            }
        }

        _ => {
            // eprintln!("{}: {:#?}", base_expr, base_expr.annotation());
            let err = NameError::value_member_not_found(
                base_expr.annotation(),
                member_ident.clone(),
            );
            Err(TypeError::from_name_err(err, member_ident.span.clone()))
        }
    }
}

fn typecheck_member_value(
    lhs: &Expr,
    base_ty: &Type,
    value_kind: ValueKind,
    member_ident: &Ident,
    ctx: &Context,
) -> TypeResult<Value> {
    let member_span = member_ident.span.clone();

    let member = ctx
        .find_member(&lhs.annotation().ty(), &member_ident)
        .map_err(|err| TypeError::from_name_err(err, member_span.clone()))?;

    let annotation = match member {
        InstanceMember::Method { iface_ty, self_ty, method } => {
            let sig = method.func_decl.sig();

            // eprintln!("{}.{}: iface_ty = {iface_ty}, self_ty = {self_ty}", lhs, member_ident);

            let (method_index, iface_method) = iface_ty
                .find_method(method.func_decl.ident(), &sig, ctx)
                .ok()
                .flatten()
                .unwrap_or_else(|| {
                    panic!(
                        "find_instance_member should only return methods that exist - no match for {}.{} : {}",
                        iface_ty,
                        method.func_decl.ident(),
                        sig,
                    )
                });

            let method = OverloadValue::method(
                self_ty,
                iface_ty,
                method_index,
                lhs.clone(),
                iface_method,
                member_span,
            );

            Value::from(method)
        }

        InstanceMember::UFCSCall { func_name, decl, visibility } => {
            // to be resolved later, presumably in a subsequent call
            Value::from(UfcsValue::new(
                func_name,
                visibility,
                lhs.clone(),
                decl.clone(),
                member_span,
            ))
        }

        InstanceMember::Overloaded { candidates } => {
            Value::from(OverloadValue::new(
                candidates,
                Some(Box::new(lhs.clone())),
                member_span,
            ))
        }

        InstanceMember::Field { ty, access, decl, decl_index } => {
            if base_ty.get_current_access(ctx) < access {
                return Err(TypeError::TypeMemberInaccessible {
                    member: member_ident.clone(),
                    ty: base_ty.clone(),
                    access,
                    span: member_span,
                });
            }

            /* class members are always mutable because a mutable class ref is only
            a mutable *reference*. record and variant members are accessed by readonly value */
            let value_kind = match base_ty {
                Type::Class(..) => ValueKind::Mutable,
                _ => value_kind,
            };

            let decl_ident = decl.idents[decl_index].clone();
            let decl_path = match ty.full_path() {
                Some(ty_path) => ty_path.into_owned().child(decl_ident),
                None => IdentPath::from(decl_ident),
            };

            Value::from(TypedValue {
                ty: ty.clone(),
                span: member_span,
                value_kind,
                decl: Some(decl_path),
                semantic_hint: SemanticHint::Property,
            })
        }
    };

    Ok(annotation)
}

pub type UnaryOp = ast::UnaryOp<Value>;

pub fn typecheck_unary_op(
    unary_op: &ast::UnaryOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<UnaryOp> {
    let operand_expect_ty = match unary_op.op {
        Operator::Add | Operator::Sub | Operator::Not => expect_ty.clone(),
        Operator::AddressOf => match expect_ty {
            // value of the operator expr is the pointer to the deref type, so the operand
            // is of the deref type
            Type::Pointer(deref_ty) => (**deref_ty).clone(),
            _ => Type::Nothing,
        },

        // always evaluates to the same type as its operand
        Operator::BitNot => expect_ty.clone(),

        Operator::Caret => expect_ty.clone().ptr(),
        _ => Type::Nothing,
    };

    let span = unary_op.span().clone();

    // do *not* evaluate the expression here - if it isn't already an addressable value,
    // evaluating it can only produce a temporary value anyway
    let operand = typecheck_expr(&unary_op.operand, &operand_expect_ty, ctx)?;

    let operand_ty = operand.annotation().ty();

    let typed_val = match unary_op.op {
        Operator::AddressOf => {
            let ty = operand.annotation().ty();
            let value_kind = operand.annotation().value_kind();

            let kind_addressable = match operand.annotation().value_kind() {
                None
                | Some(ValueKind::Temporary)
                | Some(ValueKind::Immutable)
                | Some(ValueKind::Uninitialized) => {
                    false
                }

                Some(ValueKind::Mutable) => true,
            };

            let val = match (kind_addressable, ty.as_ref()) {
                (false, _) | (true, Type::Nothing | Type::Nil | Type::Function(..)) => {
                    Err(TypeError::NotAddressable {
                        ty: ty.into_owned(),
                        value_kind,
                        span,
                    })
                }

                (
                    true,
                    Type::Class(..)
                    | Type::Interface(..)
                    | Type::DynArray { .. }
                    | Type::Array { .. }
                    | Type::MethodSelf { .. }
                    | Type::Variant(..)
                    | Type::GenericParam(..),
                ) if !ctx.allow_unsafe() => Err(TypeError::UnsafeAddressOfNotAllowed {
                    ty: ty.into_owned(),
                    span,
                }),

                _ => Ok(TypedValue::temp(ty.into_owned().ptr(), span)),
            }?;

            Some(val)
        }

        Operator::Add | Operator::Sub => {
            if let Some(result_ty) = operand_ty.arithmetic_op_result(unary_op.op, &operand_ty) {
                Some(TypedValue::temp(result_ty.into_owned(), span))
            } else {
                None
            }
        }

        Operator::Caret => {
            let deref_ty = operand
                .annotation()
                .ty()
                .deref_ty()
                .cloned()
                .ok_or_else(|| TypeError::NotDerefable {
                    ty: operand.annotation().ty().into_owned(),
                    span: span.clone(),
                })?;

            let value_kind = ValueKind::Mutable;

            Some(TypedValue {
                ty: deref_ty,
                value_kind,
                span,
                decl: None,
                semantic_hint: SemanticHint::None,
            })
        }

        Operator::Not => {
            operand
                .annotation()
                .expect_value(&Type::Primitive(Primitive::Boolean))?;

            Some(TypedValue::temp(Type::Primitive(Primitive::Boolean), span))
        }

        Operator::BitNot => {
            let valid_ty = match operand.annotation().ty().as_ref() {
                Type::Primitive(p) => p.is_integer(),
                Type::Set(..) => true,
                _ => false,
            };

            if !valid_ty {
                None
            } else {
                Some(TypedValue::temp(operand.annotation().ty().into_owned(), span))
            }
        }

        _ => {
            None
        }
    };

    let annotation = typed_val.ok_or_else(|| {
        TypeError::InvalidUnaryOp {
            op: unary_op.op,
            operand: operand.annotation().ty().into_owned(),
            span: unary_op.annotation.clone(),
        }
    })?;

    Ok(UnaryOp {
        operand,
        pos: unary_op.pos,
        op: unary_op.op,
        op_span: unary_op.op_span.clone(),
        annotation: Value::from(annotation).into(),
    })
}

pub fn typecheck_indexer(
    bin_op: &ast::BinOp<Span>,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let span = &bin_op.annotation;

    let base = &bin_op.lhs;
    let index = &bin_op.rhs;

    // todo: other index types
    let index_ty = Type::Primitive(Primitive::Int32);
    let index = typecheck_expr(index, &index_ty, ctx)?;

    index.annotation().expect_value(&index_ty)?;

    let base = typecheck_expr(base, &Type::Nothing, ctx)?;

    check_array_bound_static(&base, &index, ctx)?;

    let base_ty = base.annotation().ty();

    let (el_ty, value_kind) = match (base_ty.element_ty(), base.annotation().value_kind()) {
        (Some(el_ty), Some(base_value_kind)) => {
            let base_value_kind = if base.annotation().ty().is_by_ref() {
                // on heap e.g. dynamic array, always mutable
                ValueKind::Mutable
            } else {
                // inherit mutability from owning variable
                base_value_kind
            };

            (el_ty.clone(), base_value_kind)
        }

        // not indexable
        _ => {
            return Err(TypeError::InvalidIndexer {
                index_ty: index_ty.clone(),
                base: Box::new(base.clone()),
                span: span.clone(),
            })
        }
    };

    let annotation = Value::from(TypedValue {
        value_kind,
        ty: el_ty,
        span: span.clone(),
        decl: None,
        semantic_hint: SemanticHint::None,
    });

    Ok(Expr::from(BinOp {
        lhs: base,
        rhs: index,
        op: Operator::Index,
        op_span: bin_op.op_span.clone(),
        annotation,
    }))
}

pub fn typecheck_in_set_operator(
    bin_op: &ast::BinOp<Span>,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    // type inference can work both ways here:
    // if the expression on the right is a collection constructor, we already know typechecking
    // would normally fail, because without a hint, we will infer it as an array. in that case,
    // check the item expr first and use its type to hint that a set is expected.
    // in any other case, check the set expr first since it must refer to a known set type,
    // which can be used to infer the item expr type if needed
    let (item_expr, set_expr) = match &bin_op.rhs {
        ast::Expr::CollectionCtor(ctor) => {
            let item_expr = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;

            item_expr.annotation().expect_any_value()?;
            let item_type = item_expr.annotation().ty().into_owned();

            let elements = collection_ctor_elements(ctor, &item_type, ctx)?;

            let item_exprs: Vec<_> = elements
                .into_iter()
                .map(|el| el.value)
                .collect();

            let set_type = SetDecl::items_to_set_type(None, &item_exprs, &ctor.annotation, ctx)?;
            let set_expr = typecheck_expr(&bin_op.rhs, &Type::set(set_type), ctx)?;

            match set_expr.annotation().ty().as_ref() {
                Type::Set(set_type) => {
                    if set_type.item_type != *item_expr.annotation().ty() {
                        return Err(TypeError::InvalidBinOp {
                            lhs: item_expr.annotation().ty().into_owned(),
                            rhs: set_expr.annotation().ty().into_owned(),
                            span: bin_op.annotation.clone(),
                            op: Operator::In,
                        });
                    }
                }

                invalid => {
                    return Err(TypeError::InvalidBinOp {
                        lhs: item_expr.annotation().ty().into_owned(),
                        rhs: invalid.clone(),
                        span: bin_op.annotation.clone(),
                        op: Operator::In,
                    })
                }
            }

            (item_expr, set_expr)
        }

        _ => {
            let set_expr = typecheck_expr(&bin_op.rhs, &Type::Nothing, ctx)?;
            let set_expr_type = set_expr.annotation().ty();

            let item_expect_ty = match set_expr_type.as_ref() {
                Type::Set(set_type) => {
                    &set_type.item_type
                }

                invalid => {
                    let invalid_lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
                    return Err(TypeError::InvalidBinOp {
                        lhs: invalid_lhs.annotation().ty().into_owned(),
                        rhs: invalid.clone(),
                        span: bin_op.annotation.clone(),
                        op: Operator::In,
                    });
                }
            };

            let item_expr = typecheck_expr(&bin_op.lhs, item_expect_ty, ctx)?;
            item_expr.annotation().expect_value(&item_expect_ty)?;

            (item_expr, set_expr)
        }
    };

    let value = TypedValue::temp(Type::Primitive(Primitive::Boolean), bin_op.annotation.clone());

    let bin_op = BinOp {
        lhs: item_expr,
        rhs: set_expr,
        op: Operator::In,
        op_span: bin_op.op_span.clone(),
        annotation: Value::from(value),
    };

    Ok(Expr::BinOp(Box::new(bin_op)))
}

fn check_array_bound_static(base: &Expr, index: &Expr, ctx: &mut Context) -> TypeResult<()> {
    fn out_of_range(dim: usize, index: IntConstant) -> bool {
        index.as_i128() < 0 || index.as_i128() >= dim as i128
    }

    match (
        base.annotation().ty().as_ref(),
        const_eval_integer(index, ctx),
    ) {
        (Type::Array(array_ty), Ok(index_const)) if out_of_range(array_ty.dim, index_const.value) => {
            Err(TypeError::IndexOutOfBounds {
                index: index_const.value,
                base_ty: Box::new(base.annotation().ty().into_owned()),
                span: index.span().clone(),
            })
        }

        _ => Ok(()),
    }
}
