#[cfg(test)]
mod test;

use crate::ast;
use crate::ast::FunctionDeclKind;
use crate::ast::MemberDeclSection;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Literal;
use crate::ast::MethodOwner;
use crate::ast::SetDeclRange;
use crate::ast::FunctionName;
use crate::ast::TypeMemberDeclRef;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_object_ctor_args;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::InterfaceMethodDecl;
use crate::typ::ast::TypeDeclItemInfo;
use crate::typ::set::SetType;
use crate::typ::typecheck_type;
use crate::typ::ConstValue;
use crate::typ::Context;
use crate::typ::Def;
use crate::typ::FunctionSig;
use crate::typ::InvalidBaseTypeReason;
use crate::typ::InvalidTagReason;
use crate::typ::InvalidTypeParamsDeclKind;
use crate::typ::MismatchedImplementation;
use crate::typ::MissingImplementation;
use crate::typ::NameError;
use crate::typ::Primitive;
use crate::typ::Specializable;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::MAX_FLAGS_BITS;
use crate::IntConstant;
use std::borrow::Cow;
use std::mem;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type StructDef = ast::StructDecl<Value>;
pub type StructMemberDecl = ast::TypeMemberDecl<Value>;
pub type StructDeclSection = ast::StructDeclSection<Value>;
pub type MethodDeclSection = ast::MethodDeclSection<Value>;
pub type FieldDecl = ast::FieldDecl<Value>;
pub type MethodDecl = ast::MethodDecl<Value>;
pub type InterfaceDecl = ast::InterfaceDecl<Value>;
pub type VariantDef = ast::VariantDecl<Value>;
pub type AliasDecl = ast::AliasDecl<Value>;
pub type EnumDecl = ast::EnumDecl<Value>;
pub type EnumDeclItem = ast::EnumDeclItem<Value>;
pub type SetDecl = ast::SetDecl<Value>;

pub type Tag = ast::tag::Tag<Value>;
pub type TagItem = ast::tag::TagItem<Value>;

pub const VARIANT_TAG_TYPE: Type = Type::Primitive(Primitive::Int32);
pub const SET_DEFAULT_VALUE_TYPE: Type = Type::Primitive(Primitive::UInt8);

const ENUM_ORD_TYPE: Type = Type::Primitive(Primitive::NativeInt);

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

// "of" type list (implements clause for a concrete type or supers clause for an interface)
fn typecheck_base_types(
    base_types: &[ast::TypeName],
    self_ty: &Type,
    ctx: &mut Context
) -> TypeResult<Vec<Type>> {
    let mut base_tys = Vec::new();

    for base_ty_name in base_types {
        let implements_ty = typecheck_type(base_ty_name, ctx)?;

        match implements_ty {
            Type::Interface(iface_name) => {
                let iface_def = ctx.find_iface_def(&iface_name.full_path)
                    .map_err(|err| TypeError::from_name_err(err, base_ty_name.span().clone()))?;
                let iface_ty = Type::Interface(iface_name);
                
                if iface_def.forward {
                    return Err(TypeError::InvalidBaseType {
                        ty: self_ty.clone(),
                        invalid_base_ty: iface_ty,
                        reason: InvalidBaseTypeReason::Forward,
                        span: base_ty_name.span().clone(),
                    })
                }
                
                base_tys.push(iface_ty)
            },

            invalid_base_ty => return Err(TypeError::InvalidBaseType {
                ty: self_ty.clone(),
                invalid_base_ty,
                reason: InvalidBaseTypeReason::NotInterface,
                span: base_ty_name.span().clone(),
            }),
        }
    }
    
    Ok(base_tys)
}

impl Tag {
    // this takes a &mut Context, but it should never actually modify the context! 
    // the expressions parsed within should be typename paths and literals only
    pub fn typecheck(src_tag: &ast::tag::Tag, ctx: &mut Context) -> TypeResult<Tag> {
        let mut items = Vec::new();
        
        // use a disposable branch context to ensure invalid statements/expressions that aren't
        // just const evals here don't affect the outside context
        let mut ctx = ctx.clone();

        for item in &src_tag.items {
            let item = TagItem::typecheck(item, &mut ctx)?;
            items.push(item);
        }

        Ok(Tag {
            span: src_tag.span.clone(),
            items,
        })
    }

    pub fn typecheck_tags(src_tags: &[ast::tag::Tag], ctx: &mut Context) -> TypeResult<Vec<Tag>> {
        let mut tags = Vec::new();

        for tag in src_tags {
            tags.push(Tag::typecheck(tag, ctx)?)
        }

        Ok(tags)
    }
}

impl TagItem {
    pub fn typecheck(
        tag_item: &ast::tag::TagItem,
        ctx: &mut Context
    ) -> TypeResult<Self> {
        let tag_type = typecheck_type(&tag_item.tag_type, ctx)?;
        let span = tag_item.span.clone();
        
        if tag_type.is_unspecialized_generic() {
            return Err(TypeError::InvalidTagItem {
                reason: InvalidTagReason::InvalidType(tag_type.clone()),
                span,
            });
        }
        
        let sized= tag_type.is_sized(ctx)
            .map_err(|e| TypeError::from_name_err(e, tag_item.span.clone()))?; 
        if !sized {
            return Err(TypeError::InvalidTagItem { 
                reason: InvalidTagReason::UnsizedType(tag_type.clone()),
                span,
            })
        }
        
        if !matches!(tag_type, Type::Class(..)) {
            return Err(TypeError::InvalidTagItem {
                reason: InvalidTagReason::InvalidType(tag_type.clone()),
                span,
            });
        }
        
        let mut args = typecheck_object_ctor_args(&tag_type, &span, &tag_item.args, ctx)?;

        // all values used to initialize a tag must be const
        for member in args.members.iter_mut() {
            let Some(const_val_expr) = member.value.const_eval(ctx) else {
                return Err(TypeError::InvalidConstExpr {
                    expr: Box::new(member.value.clone()),
                });
            };

            let member_ty = member.value.annotation().ty().into_owned();
            let member_span = member.value.span().clone();

            let mut lit_expr = Expr::literal(const_val_expr, Value::new_temp_val(member_ty, member_span));

            mem::swap(&mut lit_expr, &mut member.value);
        }
        
        Ok(Self {
            tag_type,
            span,
            args,
        })
    }
}

pub fn typecheck_struct_decl(
    info: TypeDeclItemInfo,
    struct_def: &ast::StructDecl<Span>,
    ctx: &mut Context,
) -> TypeResult<StructDef> {
    assert!(struct_def.tags.is_empty() || !struct_def.forward);
    let tags = Tag::typecheck_tags(&struct_def.tags, ctx)?;
    
    let self_ty = Type::from_struct_type(info.name.clone(), struct_def.kind);

    let implements = typecheck_base_types(&struct_def.implements, &self_ty, ctx)?;
    let implements_span = Span::range(&struct_def.implements)
        .unwrap_or_else(|| struct_def.name.span.clone());

    ctx.declare_type(
        struct_def.name.ident.clone(),
        self_ty.clone(),
        info.visibility,
        true,
    )?;
    
    let sections = typecheck_members(
        &self_ty,
        &struct_def.sections,
        &implements,
        &implements_span,
        info.name.span(),
        ctx
    )?;

    Ok(StructDef {
        kw_span: struct_def.kw_span.clone(),
        kind: struct_def.kind,
        name: info.name,
        where_clause: info.where_clause,
        tags,
        packed: struct_def.packed,
        span: struct_def.span.clone(),
        implements,
        sections,
        forward: struct_def.forward,
    })
}

pub fn typecheck_members<Section>(
    owning_type: &Type,
    sources: &[Section::Source],
    implements: &[Type],
    implements_span: &Span,
    name_span: &Span,
    ctx: &mut Context,
) -> TypeResult<Vec<Section>>
where
    Section: ast::MemberDeclSection<Value>,
{
    let mut owning_type = Cow::Borrowed(owning_type);
    // methods impls are typechecked within the body of their enclosing type, so the type
    // arguments exist as GenericParam types there
    if let Some(owning_ty_params) = owning_type.type_params() {
        let owning_ty_args = owning_ty_params.clone().into_type_args();
        let inner_owning_ty = owning_type
            .specialize(&owning_ty_args, ctx)
            .map_err(|err| TypeError::from_generic_err(err, name_span.clone()))?
            .into_owned();

        owning_type = Cow::Owned(inner_owning_ty);
    }
    
    let mut dtor_span = None;

    let mut sections: Vec<Section> = Vec::new();
    
    for src_section in sources {
        let mut section = Section::clone_empty(src_section);

        for member in src_section.members() {
            match member {
                TypeMemberDeclRef::Field(field) => {
                    let field = typecheck_field(field, ctx)?;
                    if !section.add_field(field) {
                        panic!("section type is incompatible with fields (parser should not produce this)");
                    }
                }

                TypeMemberDeclRef::Method(method) => {
                    let func_decl = typecheck_method(&method.func_decl, ctx)?;
                    if func_decl.kind == FunctionDeclKind::Destructor {
                        if !matches!(owning_type.as_ref(), Type::Class(..)) {
                            return Err(TypeError::InvalidDtorOwningType {
                                ty: owning_type.into_owned(),
                                span: func_decl.span.clone(),
                            })
                        }

                        if let Some(prev_dtor) = dtor_span {
                            return Err(TypeError::TypeHasMultipleDtors {
                                owning_type: owning_type.into_owned(),
                                new_dtor: func_decl.span.clone(),
                                prev_dtor,
                            })
                        }
                        dtor_span = Some(func_decl.span.clone());

                        if func_decl.params.len() != 1 {
                            return Err(TypeError::DtorCannotHaveParams { span: func_decl.span.clone() })
                        }
                        if !func_decl.name.type_params.is_none() {
                            return Err(TypeError::DtorCannotHaveTypeParams { span: func_decl.span.clone() })
                        }
                    }

                    let existing: Vec<_> = sections
                        .iter()
                        .flat_map(|section| section.methods())
                        .filter(|m| m.func_decl.ident() == func_decl.ident())
                        .map(|m| m.func_decl.clone())
                        .collect();

                    if !existing.is_empty() {
                        if let Some(invalid) = func_decl.check_new_overload(existing.clone()) {
                            return Err(TypeError::InvalidMethodOverload {
                                owning_type: owning_type.into_owned(),
                                prev_decls: existing
                                    .into_iter()
                                    .map(|decl| decl.ident().clone())
                                    .collect(),
                                kind: invalid,
                                method: func_decl.name.ident().clone(),
                            });
                        }
                    }
                    let method_decl = MethodDecl {
                        access: method.access,
                        func_decl: Arc::new(func_decl),
                    }; 

                    if !section.add_method(method_decl) {
                        panic!("section type is incompatible with methods (parser should not produce this)");
                    }
                }
            }
        }
        
        sections.push(section);
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
            let expect_sig = iface_method.decl
                .sig()
                .with_self(owning_type.as_ref());
            
            let all_methods = sections
                .iter()
                .flat_map(|section| section.methods());
            
            for impl_method in all_methods {
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
                    eprintln!("here: actual = {:#?}\nvs\nexpect = {:#?}", actual_sig, expect_sig);
                    
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
                ty: owning_type.into_owned(),
                span: implements_span.clone(),
                missing: missing_methods,
                mismatched: mismatched_methods,
            });
        }
    }
    
    Ok(sections)
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
    info: TypeDeclItemInfo,
    iface: &ast::InterfaceDecl<Span>,
    ctx: &mut Context,
) -> TypeResult<InterfaceDecl> {
    assert!(iface.tags.is_empty() || !iface.forward);
    let tags = Tag::typecheck_tags(&iface.tags, ctx)?;

    let iface_ty = Type::interface(info.name.clone());

    // declare Self type - type decls are always in their own scope so we don't need to push
    // another one
    ctx.declare_self_ty(Type::MethodSelf, iface.name.span().clone())?;
    ctx.declare_type(iface.name.ident.clone(), iface_ty.clone(), info.visibility, true)?;
    
    let supers = typecheck_base_types(&iface.supers, &iface_ty, ctx)?;

    let mut methods: Vec<InterfaceMethodDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods
            .iter()
            .find(|other| other.decl.name.ident == method.decl.name.ident)
        {
            let method_path = info.name
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

        methods.push(ast::InterfaceMethodDecl { 
            decl: Arc::new(method_decl),
        });
    }
    
    let iface_decl = InterfaceDecl {
        name: info.name,
        where_clause: info.where_clause,
        tags,
        supers,
        forward: iface.forward,
        span: iface.span.clone(),
        methods,
    };
    
    Ok(iface_decl)
}

pub fn typecheck_variant(
    info: TypeDeclItemInfo,
    variant_def: &ast::VariantDecl<Span>,
    ctx: &mut Context,
) -> TypeResult<VariantDef> {
    assert!(variant_def.tags.is_empty() || !variant_def.forward);
    let tags = Tag::typecheck_tags(&variant_def.tags, ctx)?;

    if variant_def.cases.is_empty() {
        return Err(TypeError::EmptyVariantDecl(Box::new(variant_def.clone())));
    }

    let variant_ty = Type::variant(info.name.clone());

    let implements = typecheck_base_types(&variant_def.implements, &variant_ty, ctx)?;
    let implements_span = Span::range(&variant_def.implements)
        .unwrap_or_else(|| variant_def.name.span.clone());

    ctx.declare_type(
        variant_def.name.ident.clone(),
        variant_ty.clone(),
        info.visibility,
        true
    )?;

    let mut cases = Vec::with_capacity(variant_def.cases.len());
    for case in &variant_def.cases {
        let data = match &case.data {
            Some(data) => {
                Some(ast::VariantCaseData {
                    ty: typecheck_type(&data.ty, ctx)?,
                    span: data.span.clone(),
                })
            }

            None => None,
        };

        cases.push(ast::VariantCase {
            ident: case.ident.clone(),
            span: case.span.clone(),
            data,
        });
    }
    
    let sections = typecheck_members(
        &variant_ty,
        &variant_def.sections,
        &implements,
        &implements_span,
        info.name.span(),
        ctx
    )?;

    Ok(VariantDef {
        kw_span: variant_def.kw_span.clone(),
        name: info.name.into(),
        where_clause: info.where_clause,
        tags,
        forward: variant_def.forward,
        cases,
        implements,
        sections,
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
        ty_span: alias.ty_span.clone(),
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
    
    let enum_ty = Type::Enum(Arc::new(name.full_path.clone()));
    
    let mut prev_item: Option<(Ident, IntConstant)> = None;

    let mut items = Vec::with_capacity(enum_decl.items.len());
    for item in &enum_decl.items {
        let (ord_val, ord_expr) = match &item.value {
            Some(val_expr) => {
                let val_expr = typecheck_expr(&val_expr, &ENUM_ORD_TYPE, ctx)?;
                let ord_expr = const_eval_integer(&val_expr, ctx)?;

                if let Some((prev_ident, prev_ord_val)) = &prev_item {
                    if ord_expr.value.as_i128() <= prev_ord_val.as_i128() {
                        return Err(TypeError::EnumValuesMustBeAscending {
                            span: item.span().clone(),
                            prev_ident: prev_ident.clone(),
                            prev_val: prev_ord_val.as_i128(),
                            next_ident: item.ident.clone(),
                            next_val: ord_expr.value.as_i128(),
                        });
                    }
                }

                (ord_expr.value, Some(ord_expr))
            },

            None => {
                let ord = prev_item
                    .map(|(_prev_ident, prev_ord_val)| prev_ord_val.as_i128() + 1)
                    .unwrap_or(0);

                (IntConstant::from(ord), None)
            },
        };
        
        let item_decl = name.full_path.clone().child(item.ident.clone());

        let item = EnumDeclItem {
            ident: item.ident.clone(),
            annotation: Value::from(ConstValue {
                value: Literal::Integer(ord_val),
                span: item.span().clone(),
                decl: Some(item_decl),
                ty: enum_ty.clone(),
            }),
            value: ord_expr,
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
                    from: Expr::literal(Literal::Integer(from_num), Value::from(from_val)),
                    to: Expr::literal(Literal::Integer(to_num), Value::from(to_val)),
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
            let int_val = const_eval_integer(item, ctx)?.value.as_i128();

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
                let item_val = item.value.as_ref().unwrap().value.as_i128();
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
