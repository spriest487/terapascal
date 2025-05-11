#[cfg(test)]
mod test;

use crate::ast;
use crate::ast::FunctionDeclKind;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Literal;
use crate::ast::MethodOwner;
use crate::ast::SetDeclRange;
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::InterfaceMethodDecl;
use crate::typ::set::SetType;
use crate::typ::typecheck_type;
use crate::typ::ConstValue;
use crate::typ::Context;
use crate::typ::Def;
use crate::typ::FunctionSig;
use crate::typ::InvalidTypeParamsDeclKind;
use crate::typ::MismatchedImplementation;
use crate::typ::MissingImplementation;
use crate::typ::NameError;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::MAX_FLAGS_BITS;
use crate::IntConstant;
use common::span::Span;
use common::span::Spanned;
use std::borrow::Cow;
use std::rc::Rc;

pub type StructDef = ast::StructDecl<Value>;
pub type StructMemberDecl = ast::StructMemberDecl<Value>;
pub type FieldDecl = ast::FieldDecl<Value>;
pub type MethodDecl = ast::MethodDecl<Value>;
pub type InterfaceDecl = ast::InterfaceDecl<Value>;
pub type VariantDef = ast::VariantDecl<Value>;
pub type AliasDecl = ast::AliasDecl<Value>;
pub type EnumDecl = ast::EnumDecl<Value>;
pub type EnumDeclItem = ast::EnumDeclItem<Value>;
pub type SetDecl = ast::SetDecl<Value>;

pub const VARIANT_TAG_TYPE: Type = Type::Primitive(Primitive::Int32);
pub const SET_DEFAULT_VALUE_TYPE: Type = Type::Primitive(Primitive::UInt8);

impl VariantDef {
    pub fn find_method<'a>(&'a self, name: &'a Ident, sig: &FunctionSig) -> Option<&'a MethodDecl> {
        self.find_methods(name)
            .find_map(move |(_,m)|{
                if m.func_decl.sig() == *sig {
                    Some(m)
                } else {
                    None
                }
            })
    }
}

pub fn typecheck_struct_decl(
    name: Symbol,
    struct_def: &ast::StructDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<StructDef> {
    let self_ty = match struct_def.kind {
        StructKind::Record => Type::record(name.clone()),
        StructKind::Class => Type::class(name.clone()),
    };

    let mut implements = Vec::new();
    for implements_name in &struct_def.implements {
        let implements_ty = typecheck_type(implements_name, ctx)?;

        match implements_ty {
            iface_ty @ Type::Interface(..) => implements.push(iface_ty),

            invalid_base_ty => return Err(TypeError::InvalidBaseType {
                invalid_base_ty,
                span: implements_name.span().clone(),
                ty: self_ty.clone(),
            }),
        }
    }

    let implements_span = Span::range(&struct_def.implements)
        .unwrap_or_else(|| struct_def.name.span.clone());

    ctx.declare_type(
        struct_def.name.ident.clone(),
        self_ty.clone(),
        visibility,
        true,
    )?;

    let mut fields = Vec::new();
    for src_field in &struct_def.fields {
        let field = typecheck_field(src_field, ctx)?;
        fields.push(field);
    }
    
    let methods = typecheck_methods(
        &self_ty,
        &struct_def.methods,
        &implements,
        &implements_span,
        ctx
    )?;

    Ok(StructDef {
        kind: struct_def.kind,
        packed: struct_def.packed,
        name,
        span: struct_def.span.clone(),
        implements,
        fields,
        methods,
        forward: struct_def.forward,
    })
}

pub fn typecheck_methods(
    owning_type: &Type,
    decl_methods: &[ast::MethodDecl],
    implements: &[Type],
    implements_span: &Span,
    ctx: &mut Context
) -> TypeResult<Vec<MethodDecl>> {
    let mut dtor_span = None;

    let mut methods: Vec<MethodDecl> = Vec::new();
    for method in decl_methods {
        let decl = typecheck_method(&method.func_decl, ctx)?;
        if decl.kind == FunctionDeclKind::Destructor {
            if !matches!(owning_type, Type::Class(..)) {
                return Err(TypeError::InvalidDtorOwningType {
                    ty: owning_type.clone(),
                    span: decl.span.clone(),
                }) 
            }
            
            if let Some(prev_dtor) = dtor_span {
                return Err(TypeError::TypeHasMultipleDtors {
                    owning_type: owning_type.clone(),
                    new_dtor: decl.span.clone(),
                    prev_dtor,
                })
            }
            dtor_span = Some(decl.span.clone());
            
            if decl.params.len() != 1 {
                return Err(TypeError::DtorCannotHaveParams { span: decl.span.clone() })
            }
            if !decl.type_params.is_none() {
                return Err(TypeError::DtorCannotHaveTypeParams { span: decl.span.clone() })
            }
        }

        let existing: Vec<_> = methods
            .iter()
            .filter(|m| m.func_decl.ident() == decl.ident())
            .map(|m| m.func_decl.clone())
            .collect();

        if !existing.is_empty() {
            if let Some(invalid) = decl.check_new_overload(existing.clone()) {
                return Err(TypeError::InvalidMethodOverload {
                    owning_type: owning_type.clone(),
                    prev_decls: existing
                        .into_iter()
                        .map(|decl| decl.ident().clone())
                        .collect(),
                    kind: invalid,
                    method: decl.name.ident().clone(),
                });
            }
        }

        methods.push(MethodDecl {
            access: method.access,
            func_decl: Rc::new(decl),
        });
    }

    for iface_ty in implements.iter() {
        let Type::Interface(iface_name) = &iface_ty else {
           unreachable!("already checked that only valid types are accepted")
        };

        let iface_def = ctx
            .instantiate_iface_def(iface_name)
            .map_err(|e| {
                TypeError::from_name_err(e, iface_name.span().clone())
            })?;

        let mut missing_methods = Vec::new();
        let mut mismatched_methods = Vec::new();
        
        let mut per_method_mismatches = Vec::new();

        'iface_method_loop: for iface_method in &iface_def.methods {
            let expect_sig = iface_method.decl.sig().with_self(owning_type);
            
            for impl_method in methods.iter() {
                if impl_method.func_decl.name.ident() != iface_method.ident() {
                    continue;
                }

                let actual_sig = impl_method.func_decl.sig();

                if actual_sig == expect_sig {
                    // if we have an exact match, we don't care about other methods that
                    // don't match
                    per_method_mismatches.clear();
                    continue 'iface_method_loop;
                } else {                    
                    per_method_mismatches.push(MismatchedImplementation {
                        impl_method_name: impl_method.func_decl.name.clone(),
                        iface_method_name: iface_method.decl.name.clone(),
                        expect_sig: expect_sig.clone(),
                        actual_sig,
                    });
                }
            }

            if per_method_mismatches.is_empty() {
                missing_methods.push(MissingImplementation {
                    method_name: iface_method.decl.name.clone(),
                    sig: expect_sig,
                });
            } else {
                mismatched_methods.append(&mut per_method_mismatches);
            }
        }

        if !missing_methods.is_empty() || !mismatched_methods.is_empty() {
            return Err(TypeError::InvalidImplementation {
                ty: owning_type.clone(),
                span: implements_span.clone(),
                missing: missing_methods,
                mismatched: mismatched_methods,
            });
        }
    }
    
    Ok(methods)
}

fn typecheck_method(decl: &ast::FunctionDecl, ctx: &mut Context) -> TypeResult<FunctionDecl> {
    let decl = FunctionDecl::typecheck(decl, false, ctx)?;    
    Ok(decl)
}

fn typecheck_field(
    field: &ast::FieldDecl,
    ctx: &mut Context
) -> TypeResult<FieldDecl> {
    let ty = typecheck_type(&field.ty, ctx)?.clone();

    ty.expect_sized(ctx, &field.span)?;

    let field = FieldDecl {
        ty,
        span: field.span.clone(),
        ident: field.ident.clone(),
        access: field.access,
    };
    
    Ok(field)
}

pub fn typecheck_iface(
    name: Symbol,
    iface: &ast::InterfaceDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<InterfaceDecl> {
    // declare Self type - type decls are always in their own scope so we don't need to push
    // another one
    ctx.declare_self_ty(Type::MethodSelf, iface.name.span().clone())?;
    ctx.declare_type(
        iface.name.ident.clone(),
        Type::interface(name.full_path.clone()),
        visibility,
        true
    )?;

    let mut methods: Vec<InterfaceMethodDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods
            .iter()
            .find(|other| other.decl.name.ident == method.decl.name.ident)
        {
            let method_path = name
                .full_path
                .clone()
                .child(method.decl.name.ident().clone());

            return Err(TypeError::NameError {
                err: NameError::AlreadyDefined {
                    ident: method_path,
                    existing: existing.span().clone(),
                },
                span: method.decl.name.span().clone(),
            });
        }

        let method_decl = FunctionDecl::typecheck(&method.decl, false, ctx)?;

        methods.push(ast::InterfaceMethodDecl { decl: Rc::new(method_decl) });
    }
    
    let iface_decl = InterfaceDecl {
        name,
        forward: iface.forward,
        span: iface.span.clone(),
        methods,
    };
    
    Ok(iface_decl)
}

pub fn typecheck_variant(
    name: Symbol,
    variant_def: &ast::VariantDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<VariantDef> {
    if variant_def.cases.is_empty() {
        return Err(TypeError::EmptyVariantDecl(Box::new(variant_def.clone())));
    }
    
    let mut implements = Vec::new();
    for implements_ty in &variant_def.implements {
        implements.push(typecheck_type(implements_ty, ctx)?);
    }

    let implements_span = Span::range(&variant_def.implements)
        .unwrap_or_else(|| variant_def.name.span.clone());
    
    let variant_ty = Type::variant(name.clone());

    ctx.declare_type(
        variant_def.name.ident.clone(),
        variant_ty.clone(),
        visibility,
        true
    )?;

    let mut cases = Vec::with_capacity(variant_def.cases.len());
    for case in &variant_def.cases {
        let data_ty = match &case.data_ty {
            Some(data_ty) => Some(typecheck_type(data_ty, ctx)?),
            None => None,
        };

        cases.push(ast::VariantCase {
            ident: case.ident.clone(),
            span: case.span.clone(),
            data_ty,
        });
    }
    
    let methods = typecheck_methods(
        &variant_ty,
        &variant_def.methods,
        &implements,
        &implements_span,
        ctx
    )?;

    Ok(VariantDef {
        name: Rc::new(name),
        forward: variant_def.forward,
        cases,
        implements,
        methods,
        span: variant_def.span().clone(),
    })
}

pub fn typecheck_alias(
    name: Symbol,
    alias: &ast::AliasDecl<Span>,
    ctx: &mut Context,
) -> TypeResult<AliasDecl> {
    let ty = typecheck_type(&alias.ty, ctx)?;

    Ok(AliasDecl {
        name,
        ty: Box::new(ty),
        span: alias.span.clone(),
    })
}

pub fn typecheck_enum_decl(
    name: Symbol,
    enum_decl: &ast::EnumDecl<Span>,
    ctx: &mut Context,
) -> TypeResult<EnumDecl> {
    name.expect_no_type_params(InvalidTypeParamsDeclKind::Enum)?;
    assert!(name.type_args.is_none());

    let mut prev_item: Option<(Ident, i128)> = None;

    let mut items = Vec::with_capacity(enum_decl.items.len());
    for item in &enum_decl.items {
        let ord_val = match &item.value {
            Some(val_expr) => {
                let val_expr =
                    typecheck_expr(&val_expr, &Type::Primitive(Primitive::NativeInt), ctx)?;
                let item_ord_val = const_eval_integer(&val_expr, ctx)?.as_i128();

                if let Some((prev_ident, prev_ord_val)) = &prev_item {
                    if item_ord_val <= *prev_ord_val {
                        return Err(TypeError::EnumValuesMustBeAscending {
                            span: item.span().clone(),
                            prev_ident: prev_ident.clone(),
                            prev_val: *prev_ord_val,
                            next_ident: item.ident.clone(),
                            next_val: item_ord_val,
                        });
                    }
                }

                item_ord_val
            },

            None => prev_item
                .map(|(_prev_ident, prev_ord_val)| prev_ord_val + 1)
                .unwrap_or(0),
        };

        let item = EnumDeclItem {
            ident: item.ident.clone(),
            span: item.span.clone(),
            value: Some(IntConstant::from(ord_val)),
        };

        prev_item = Some((item.ident.clone(), ord_val));

        items.push(item);
    }

    let enum_decl = EnumDecl {
        name,
        items,
        span: enum_decl.span.clone(),
    };
    Ok(enum_decl)
}

impl SetDecl {
    pub fn typecheck(
        set_decl: &ast::SetDecl<Span>,
        name: Symbol,
        ctx: &mut Context,
    ) -> TypeResult<Self> {
        name.expect_no_type_params(InvalidTypeParamsDeclKind::Set)?;
        assert!(name.type_args.is_none());

        let range = match set_decl.range.as_ref() {
            SetDeclRange::Range { from, to, span: range_span } => {
                let from = typecheck_expr(from, &Type::Primitive(Primitive::UInt8), ctx)?;
                let val_ty = from.annotation().ty().into_owned();
                let to = typecheck_expr(to, &val_ty, ctx)?;
                
                to.annotation().expect_value(&val_ty)?;
                
                let from_num = get_set_range_expr_val(&from, range_span, ctx)?;
                let to_num = get_set_range_expr_val(&to, range_span, ctx)?;
                
                if from_num.as_i128() > to_num.as_i128() {
                    return Err(TypeError::SetValuesMustBeSequential {
                        from: from_num,
                        to: to_num,
                        span: range_span.clone(),
                    });
                }

                let range_size = (to_num.as_i128() - from_num.as_i128()) as usize; 
                if range_size > MAX_FLAGS_BITS {
                    return Err(TypeError::TooManySetValues { 
                        span: range_span.clone(),
                        count: range_size,
                    });
                }
                
                let from_val = ConstValue {
                    ty: val_ty.clone(),
                    span: from.span().clone(),
                    decl: None,
                    value: Literal::Integer(from_num),
                };

                let to_val = ConstValue {
                    ty: val_ty.clone(),
                    span: to.span().clone(),
                    decl: None,
                    value: Literal::Integer(to_num),
                };
                
                SetDeclRange::Range {
                    from: Expr::Literal(Literal::Integer(from_num), Value::from(from_val)),
                    to: Expr::Literal(Literal::Integer(to_num), Value::from(to_val)),
                    span: range_span.clone(),
                }
            }
            
            SetDeclRange::Type { ty, .. } => {
                let range_ty = typecheck_type(ty, ctx)?;

                // just do this here to raise an error if it's invalid
                _ = get_set_type_range(&range_ty, ty.span(), ctx)?;
                
                SetDeclRange::Type { 
                    ty: range_ty,
                    span: ty.span().clone(),
                }
            }
        };

        let set_decl = SetDecl {
            span: set_decl.span.clone(),
            range: Box::new(range),
            name,
        };

        Ok(set_decl)
    }
    
    pub fn value_type(&self) -> Cow<Type> {
        match self.range.as_ref() {
            SetDeclRange::Type { ty, .. } => Cow::Borrowed(ty),
            SetDeclRange::Range { from, .. } => from.annotation().ty(),
        }
    }
    
    pub fn to_set_type(&self, ctx: &Context) -> TypeResult<SetType> {
        let name = self.name.full_path.clone();

        let (from_const, to_const) = match self.range.as_ref() {
            SetDeclRange::Type { ty, span, .. } => {
                get_set_type_range(ty, span, ctx)?
            },

            SetDeclRange::Range { from, to, .. } => {
                let from_const = from.annotation()
                    .as_const()
                    .and_then(|val| val.value.clone().try_into_int())
                    .expect("set range values may only be const");
                let to_const = to.annotation()
                    .as_const()
                    .and_then(|val| val.value.clone().try_into_int())
                    .expect("set range values may only be const integers");

                (from_const, to_const)
            }
        };

        Ok(SetType {
            min: from_const,
            max: to_const,
            name: Some(name),
            item_type: self.value_type().into_owned(),
        })
    }
    
    pub fn items_to_set_type(name: Option<IdentPath>, items: &[Expr], at: &Span, ctx: &Context) -> TypeResult<SetType> {
        if items.is_empty() {
            return Err(TypeError::EmptySetDecl {
                span: at.clone(),
                name,
            });
        }
        
        let mut max = None;
        let mut min = None;

        for item in items {
            let int_val = const_eval_integer(item, ctx)?.as_i128();

            max = Some(max.map_or(int_val, |val| i128::max(val, int_val)));
            min = Some(min.map_or(int_val, |val| i128::min(val, int_val)));
        }
        
        let min = min.unwrap();
        let max = max.unwrap();
        let range = max - min;

        if range > MAX_FLAGS_BITS as i128 {
            return Err(TypeError::TooManySetValues {
                count: range as usize,
                span: at.clone(),
            })
        }

        Ok(SetType {
            name,
            min: IntConstant::from(min),
            max: IntConstant::from(max),
            item_type: items[0].annotation().ty().into_owned(),
        })
    }
}

fn get_set_type_range(range_ty: &Type, at: &Span, ctx: &Context) -> TypeResult<(IntConstant, IntConstant)> {
    match &range_ty {
        Type::Primitive(primitive) => {
            let Some((min_const, max_const)) = primitive.integer_range() else {
                return Err(TypeError::InvalidSetValueType {
                    actual: range_ty.clone(),
                    span: at.clone(),
                });
            };

            let range = max_const.as_i128() - min_const.as_i128();
            if range > MAX_FLAGS_BITS as i128 {
                return Err(TypeError::TooManySetValues {
                    count: range as usize,
                    span: at.clone(),
                });
            }
            
            Ok((min_const, max_const))
        }

        Type::Enum(enum_path) => {
            let Some(Def::Enum(enum_decl)) = ctx.find_type_def(enum_path) else {
                return Err(TypeError::name_not_found(enum_path.as_ref().clone(), at.clone()));
            };

            if enum_decl.items.is_empty() {
                return Err(TypeError::InvalidSetValueType {
                    actual: range_ty.clone(),
                    span: at.clone(),
                });
            }

            let mut min = None;
            let mut max = None;
            for item in &enum_decl.items {
                let item_val = item.value.as_ref().unwrap().as_i128();
                min = Some(min.map_or(item_val, |val| i128::min(item_val, val)));
                max = Some(max.map_or(item_val, |val| i128::max(item_val, val)));
            }

            let min = min.unwrap();
            let max = max.unwrap();
            let range = max - min;

            if range > MAX_FLAGS_BITS as i128 {
                return Err(TypeError::TooManySetValues {
                    count: range as usize,
                    span: at.clone(),
                });
            }
            
            Ok((IntConstant::from(min), IntConstant::from(max)))
        }

        _ => {
            Err(TypeError::InvalidSetValueType {
                actual: range_ty.clone(),
                span: at.clone(),
            })
        }
    }
}

fn get_set_range_expr_val(val: &Expr, at: &Span, ctx: &Context) -> TypeResult<IntConstant> {
    let Some(val_lit) = val.const_eval(ctx) else {
        return Err(TypeError::InvalidConstExpr { expr: Box::new(val.clone()) });
    };
    
    val_lit.try_into_int().ok_or_else(|| {
        TypeError::InvalidSetValueType {
            actual: val.annotation().ty().into_owned(),
            span: at.clone(),
        }
    })
}
