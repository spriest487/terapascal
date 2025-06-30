use crate::ast;
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::InvalidTypeParamsDeclKind;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::ast::SetDecl;
use crate::typ::ast::TypeDeclItemInfo;
use crate::typ::ast::WhereClause;
use crate::typ::ast::typecheck_alias;
use crate::typ::ast::typecheck_enum_decl;
use crate::typ::ast::typecheck_iface;
use crate::typ::ast::typecheck_struct_decl;
use crate::typ::ast::typecheck_variant;
use std::sync::Arc;
use terapascal_common::span::Span;

pub type TypeDecl = ast::TypeDecl<Value>;
pub type TypeDeclItem = ast::TypeDeclItem<Value>;

impl ast::TypeDecl {
    pub fn typecheck(&self, visibility: Visibility, ctx: &mut Context) -> TypeResult<TypeDecl> {
        let mut items = Vec::with_capacity(self.items.len());

        for type_decl_item in &self.items {
            let item = typecheck_type_decl_item(type_decl_item, visibility, ctx)?;
            items.push(item);
        }

        Ok(TypeDecl {
            kw_span: self.kw_span.clone(),
            items,
            span: self.span.clone(),
        })
    }
}

fn typecheck_type_decl_item(
    type_decl: &ast::TypeDeclItem,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<TypeDeclItem> {
    let where_clause = match type_decl.constraint_clause() {
        Some(clause) => Some(WhereClause::typecheck(clause, ctx)?),
        None => None,
    };

    let full_name = Symbol::from_local_decl_name(&type_decl.name(), where_clause.as_ref(), ctx)?;

    let item_info = TypeDeclItemInfo {
        name: full_name,
        where_clause,
        visibility,
    };

    match type_decl {
        // except aliases, we can skip the rest of the type decl code for them
        ast::TypeDeclItem::Alias(alias_decl) => {
            item_info.expect_not_generic(InvalidTypeParamsDeclKind::Alias, &alias_decl.span)?;

            let alias = typecheck_alias(item_info.name, alias_decl, ctx);

            ctx.declare_type(
                alias.name.ident().clone(),
                alias.target.ty().clone(),
                visibility,
                false,
            );

            Ok(TypeDeclItem::Alias(Arc::new(alias)))
        },

        ast::TypeDeclItem::Struct(def) => match def.kind {
            StructKind::Class => {
                let ty = Type::class(item_info.name.clone());
                typecheck_type_decl_item_with_def(item_info, ty, type_decl, ctx)
            },
            StructKind::Record => {
                let ty = Type::record(item_info.name.clone());
                typecheck_type_decl_item_with_def(item_info, ty, type_decl, ctx)
            },
        },

        ast::TypeDeclItem::Interface(_) => {
            let ty = Type::interface(item_info.name.clone());
            typecheck_type_decl_item_with_def(item_info, ty, type_decl, ctx)
        },
        ast::TypeDeclItem::Variant(_) => {
            let ty = Type::variant(item_info.name.clone());
            typecheck_type_decl_item_with_def(item_info, ty, type_decl, ctx)
        },
        ast::TypeDeclItem::Enum(enum_decl) => {
            item_info.expect_not_generic(InvalidTypeParamsDeclKind::Enum, &enum_decl.span)?;

            let ty = Type::enumeration(item_info.name.full_path.clone());
            typecheck_type_decl_item_with_def(item_info, ty, type_decl, ctx)
        },
        ast::TypeDeclItem::Set(set_decl) => {
            item_info.expect_not_generic(InvalidTypeParamsDeclKind::Set, &set_decl.span)?;

            typecheck_set_decl_item(set_decl, item_info.name, visibility, ctx)
        },
    }
}

fn typecheck_set_decl_item(
    set_decl: &ast::SetDecl,
    full_name: Symbol,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<TypeDeclItem> {
    let set_decl = Arc::new(SetDecl::typecheck(set_decl, full_name, ctx)?);

    ctx.declare_set(&set_decl, visibility);

    Ok(TypeDeclItem::Set(set_decl))
}

// for all cases other than aliases and sets
fn typecheck_type_decl_item_with_def(
    info: TypeDeclItemInfo,
    ty: Type,
    type_decl: &ast::TypeDeclItem,
    ctx: &mut Context,
) -> TypeResult<TypeDeclItem> {
    // type decls have an inner scope
    let ty_scope = ctx.push_scope(Environment::TypeDecl { ty });

    if let Some(ty_params) = &info.name.type_params {
        ctx.declare_type_params(&ty_params);
    }

    let visibility = info.visibility;
    let type_decl = typecheck_type_decl_body(info, type_decl, ctx)?;

    ctx.pop_scope(ty_scope);

    match &type_decl {
        TypeDeclItem::Interface(iface) => {
            ctx.declare_iface(iface.clone(), visibility)?;
        },

        TypeDeclItem::Variant(variant) => {
            ctx.declare_variant(variant.clone(), visibility)?;
        },

        TypeDeclItem::Struct(class) => {
            ctx.declare_struct(class.clone(), visibility)?;
        },

        TypeDeclItem::Enum(enum_decl) => {
            ctx.declare_enum(enum_decl.clone(), visibility)?;
        },

        TypeDeclItem::Set(..) => {
            unreachable!("handled separately")
        },

        TypeDeclItem::Alias(_) => unreachable!(),
    }

    Ok(type_decl)
}

fn typecheck_type_decl_body(
    info: TypeDeclItemInfo,
    type_decl: &ast::TypeDeclItem<Span>,
    ctx: &mut Context,
) -> TypeResult<TypeDeclItem> {
    let type_decl = match type_decl {
        ast::TypeDeclItem::Struct(class) => {
            let class = typecheck_struct_decl(info, class, ctx);
            ast::TypeDeclItem::Struct(Arc::new(class))
        },

        ast::TypeDeclItem::Interface(iface) => {
            let iface = typecheck_iface(info, iface, ctx);
            ast::TypeDeclItem::Interface(Arc::new(iface))
        },

        ast::TypeDeclItem::Variant(variant) => {
            let variant = typecheck_variant(info, variant, ctx);
            ast::TypeDeclItem::Variant(Arc::new(variant))
        },

        ast::TypeDeclItem::Alias(alias) => {
            let alias = typecheck_alias(info.name, alias, ctx);
            ast::TypeDeclItem::Alias(Arc::new(alias))
        },

        ast::TypeDeclItem::Enum(enum_decl) => {
            let enum_decl = typecheck_enum_decl(info.name, enum_decl, ctx)?;
            ast::TypeDeclItem::Enum(Arc::new(enum_decl))
        },

        ast::TypeDeclItem::Set(set_decl) => {
            let set_decl = SetDecl::typecheck(set_decl, info.name, ctx)?;
            ast::TypeDeclItem::Set(Arc::new(set_decl))
        },
    };

    Ok(type_decl)
}
