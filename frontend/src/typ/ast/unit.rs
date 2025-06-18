use std::path::PathBuf;
use crate::ast;
use crate::ast::{BindingDeclKind, UnitBindingItemInitializer};
use crate::ast::FunctionName;
use crate::ast::IdentPath;
use crate::ast::StructKind;
use crate::ast::TypeAnnotation;
use crate::ast::Visibility;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::expr::expect_stmt_initialized;
use crate::typ::ast::{typecheck_alias, FunctionDeclContext};
use crate::typ::ast::typecheck_enum_decl;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_func_def;
use crate::typ::ast::typecheck_iface;
use crate::typ::ast::typecheck_stmt;
use crate::typ::ast::typecheck_struct_decl;
use crate::typ::ast::typecheck_variant;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::SetDecl;
use crate::typ::ast::WhereClause;
use crate::typ::typecheck_type;
use crate::typ::Binding;
use crate::typ::ConstValue;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::ExpectedKind;
use crate::typ::GenericError;
use crate::typ::InvalidTypeParamsDeclKind;
use crate::typ::ModuleUnit;
use crate::typ::NameError;
use crate::typ::Named;
use crate::typ::ScopeMemberRef;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::ValueKind;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type Unit = ast::Unit<Value>;
pub type UnitDecl = ast::UnitDecl<Value>;
pub type GlobalBinding = ast::UnitBinding<Value>;
pub type GlobalBindingItem = ast::UnitBindingItem<Value>;
pub type TypeDecl = ast::TypeDecl<Value>;
pub type TypeDeclItem = ast::TypeDeclItem<Value>;
pub type InitBlock = ast::InitBlock<Value>;

#[derive(Debug)]
pub struct TypeDeclItemInfo {
    pub name: Symbol,
    pub where_clause: Option<WhereClause>,
    pub visibility: Visibility,
}

impl TypeDeclItemInfo {
    pub fn expect_not_generic(&self, kind: InvalidTypeParamsDeclKind, item_span: &Span) -> TypeResult<()> {
        if self.name.type_params.is_some() {
            return Err(TypeError::InvalidDeclWithTypeParams {
                kind,
                span: item_span.clone(),
            });
        }

        if self.name.type_params.is_some() {
            return Err(TypeError::from_generic_err(
                GenericError::UnexpectedConstraintList,
                item_span.clone())
            );
        }
        
        Ok(())
    }
}

fn typecheck_unit_decl(
    decl: &ast::UnitDecl<Span>,
    ctx: &mut Context,
    visibility: Visibility,
) -> TypeResult<UnitDecl> {
    match decl {
        ast::UnitDecl::Uses { decl: uses } => {
            for use_item in &uses.units {
                typecheck_unit_uses_decl(use_item, ctx)?;
            }

            Ok(ast::UnitDecl::Uses { decl: uses.clone() })
        },

        ast::UnitDecl::FunctionDef { def: func_def } => {
            typecheck_unit_func_def(func_def, visibility, ctx)
        },

        ast::UnitDecl::FunctionDecl { decl: func_decl } => {
            typecheck_unit_func_decl(func_decl, visibility, ctx)
        },

        ast::UnitDecl::Type { decl: type_decl } => {
            typecheck_unit_type_decl(type_decl, visibility, ctx)
        },

        ast::UnitDecl::Binding { decl } => {
            let decl = typecheck_global_binding(decl, visibility, ctx)?;

            Ok(ast::UnitDecl::Binding { decl })
        }
    }
}

fn typecheck_unit_uses_decl(use_item: &ast::UseDeclItem, ctx: &mut Context) -> TypeResult<()> {
    match ctx.find_path(&use_item.ident) {
        // path refers to a known unit path (by alias or directly by its canon name)
        Some(ScopeMemberRef::Scope { path }) => {
            let unit_canon_ident = IdentPath::from_parts(path.keys().cloned());

            ctx.use_namespace(&unit_canon_ident);
        },

        // path refers to some other decl
        Some(ScopeMemberRef::Decl { value, .. }) => {
            let unexpected = Named::Decl(value.clone());
            let err = NameError::Unexpected {
                ident: use_item.ident.clone(),
                actual: unexpected,
                expected: ExpectedKind::Namespace,
            };
            return Err(TypeError::from_name_err(err, use_item.ident.path_span()));
        },

        // path does not exist
        None => {
            return Err(TypeError::from_name_err(
                NameError::not_found(use_item.ident.clone()),
                use_item.ident.path_span(),
            ));
        },
    }

    Ok(())
}

fn typecheck_unit_func_def(
    func_def: &ast::FunctionDef<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<UnitDecl> {
    let func_decl = FunctionDecl::typecheck(&func_def.decl, true, ctx)?;
    let func_name = &func_decl.name;
    
    // free functions may not already have a declaration in scope if they weren't forward
    // declared, do that now
    if func_decl.name.context == FunctionDeclContext::FreeFunction && !ctx.is_function_declared(&func_decl) {
        ctx.declare_function(func_name.ident().clone(), Arc::new(func_decl.clone()), visibility)?;
    }

    let func_def = Arc::new(typecheck_func_def(func_decl.clone(), func_def, ctx)?);
    match func_decl.method_declaring_type() {
        Some(ty) => {
            ctx.define_method(ty.clone(), func_def.clone())?;
        }

        None => {
            ctx.define_function(func_name.ident().clone(), func_def.clone())?;
        }
    }

    Ok(UnitDecl::FunctionDef { def: func_def })
}

fn typecheck_unit_func_decl(
    func_decl: &ast::FunctionDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<UnitDecl> {
    let name = func_decl.name.clone();
    let func_decl = Arc::new(FunctionDecl::typecheck(func_decl, false, ctx)?);

    assert_eq!(func_decl.name.context, FunctionDeclContext::FreeFunction, "not yet implemented: can't forward-declare methods");

    ctx.declare_function(name.ident().clone(), func_decl.clone(), visibility)?;

    Ok(UnitDecl::FunctionDecl { decl: func_decl })
}

pub fn typecheck_unit_type_decl(
    type_decl: &ast::TypeDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<UnitDecl> {
    let decl = typecheck_type_decl(type_decl, visibility, ctx)?;

    Ok(UnitDecl::Type { decl })
}

fn typecheck_type_decl(
    type_decl: &ast::TypeDecl,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<TypeDecl> {
    let mut items = Vec::with_capacity(type_decl.items.len());

    for type_decl_item in &type_decl.items {
        let item = typecheck_type_decl_item(type_decl_item, visibility, ctx)?;
        items.push(item);
    }

    Ok(TypeDecl {
        kw_span: type_decl.kw_span.clone(),
        items,
        span: type_decl.span.clone(),
    })
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

    let full_name = Symbol::from_local_decl_name(
        &type_decl.name(),
        where_clause.as_ref(),
        ctx
    )?;
    
    let item_info = TypeDeclItemInfo {
        name: full_name,
        where_clause,
        visibility,
    };

    match type_decl {
        // except aliases, we can skip the rest of the type decl code for them
        ast::TypeDeclItem::Alias(alias_decl) => {
            item_info.expect_not_generic(InvalidTypeParamsDeclKind::Alias, &alias_decl.span)?;

            let alias = typecheck_alias(item_info.name, alias_decl, ctx)?;

            ctx.declare_type(
                alias.name.ident().clone(),
                (*alias.ty).clone(),
                visibility,
                false
            )?;

            Ok(TypeDeclItem::Alias(Arc::new(alias)))
        }

        ast::TypeDeclItem::Struct(def) => match def.kind {
            StructKind::Class => {
                let ty = Type::class(item_info.name.clone());
                typecheck_type_decl_item_with_def(item_info, ty, type_decl, ctx)
            },
            StructKind::Record => {
                let ty = Type::record(item_info.name.clone());
                typecheck_type_decl_item_with_def(item_info, ty, type_decl, ctx)
            },
        }

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
        }
    }
}

fn typecheck_set_decl_item(
    set_decl: &ast::SetDecl,
    full_name: Symbol,
    visibility: Visibility,
    ctx: &mut Context
) -> TypeResult<TypeDeclItem> {
    let set_decl = Arc::new(SetDecl::typecheck(set_decl, full_name, ctx)?);
    
    ctx.declare_set(&set_decl, visibility)?;

    Ok(TypeDeclItem::Set(set_decl))
}

// for all cases other than aliases and sets
fn typecheck_type_decl_item_with_def(
    info: TypeDeclItemInfo,
    ty: Type,
    type_decl: &ast::TypeDeclItem,
    ctx: &mut Context
) -> TypeResult<TypeDeclItem> {
    // type decls have an inner scope
    let ty_scope = ctx.push_scope(Environment::TypeDecl {
        ty,
    });

    if let Some(ty_params) = &info.name.type_params {
        ctx.declare_type_params(&ty_params)?;
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
        }

        TypeDeclItem::Set(..) => {
            unreachable!("handled separately")
        }
        
        TypeDeclItem::Alias(_) => unreachable!()
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
            let class = typecheck_struct_decl(info, class, ctx)?;
            ast::TypeDeclItem::Struct(Arc::new(class))
        },

        ast::TypeDeclItem::Interface(iface) => {
            let iface = typecheck_iface(info, iface, ctx)?;
            ast::TypeDeclItem::Interface(Arc::new(iface))
        },

        ast::TypeDeclItem::Variant(variant) => {
            let variant = typecheck_variant(info, variant, ctx)?;
            ast::TypeDeclItem::Variant(Arc::new(variant))
        },

        ast::TypeDeclItem::Alias(alias) => {
            let alias = typecheck_alias(info.name, alias, ctx)?;
            ast::TypeDeclItem::Alias(Arc::new(alias))
        },

        ast::TypeDeclItem::Enum(enum_decl) => {
            let enum_decl = typecheck_enum_decl(info.name, enum_decl, ctx)?;
            ast::TypeDeclItem::Enum(Arc::new(enum_decl))
        }

        ast::TypeDeclItem::Set(set_decl) => {
            let set_decl = SetDecl::typecheck(set_decl, info.name, ctx)?;
            ast::TypeDeclItem::Set(Arc::new(set_decl))
        }
    };

    Ok(type_decl)
}

fn typecheck_global_binding(
    binding: &ast::UnitBinding<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<GlobalBinding> {
    let mut items = Vec::with_capacity(binding.items.len());

    for const_decl_item in &binding.items {
        let item = typecheck_global_binding_item(binding.kind, const_decl_item, visibility, ctx)?;
        items.push(item);
    }

    Ok(GlobalBinding {
        kw_span: binding.kw_span.clone(),
        kind: binding.kind,
        items,
        span: binding.span.clone(),
    })
}

fn typecheck_global_binding_item(
    kind: BindingDeclKind,
    item: &ast::UnitBindingItem<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<GlobalBindingItem> {
    let decl_span = item.span().clone();

    let (ty, val) = match kind {
        BindingDeclKind::Const => {
            let (ty, mut const_val_init) = match (&item.ty, &item.init) {
                (_, None) => {
                    return Err(TypeError::ConstDeclWithNoValue { span: decl_span });
                }

                (unknown_ty, Some(init)) if !unknown_ty.is_known() => {
                    // infer from provided value expr
                    let init_expr = typecheck_expr(&init.expr, &Type::Nothing, ctx)?;
                    let ty = init_expr.annotation().ty().into_owned();
                    
                    (ty, UnitBindingItemInitializer {
                        expr: Box::new(init_expr),
                        eq_span: init.eq_span.clone(),
                    })
                }

                (explicit_ty, Some(init)) => {
                    // use explicitly provided type
                    let ty = typecheck_type(explicit_ty, ctx)?;
                    let const_val_expr = typecheck_expr(&init.expr, &ty, ctx)?;

                    (ty, UnitBindingItemInitializer {
                        expr: Box::new(const_val_expr),
                        eq_span: init.eq_span.clone(),
                    })
                }
            };
            
            let val_span = const_val_init.expr.span().clone();

            let const_val_literal = match const_val_init.expr.const_eval(ctx) {
                Some(const_val) => Ok(const_val),
                None => Err(TypeError::InvalidConstExpr {
                    expr: const_val_init.expr,
                }),
            }?;
            
            assert_eq!(1, item.idents.len(), "parser should not produce multi-item const bindings");
            let const_ident = item.idents[0].clone();

            ctx.declare_global_const(
                const_ident.clone(),
                const_val_literal.clone(),
                ty.clone(),
                visibility,
                const_ident.span.clone(),
            )?;

            let value = ConstValue {
                value: const_val_literal.clone(),
                span: val_span,
                decl: Some(ctx.namespace().child(const_ident)),
                ty: ty.clone(),
            };

            const_val_init.expr = Box::new(Expr::literal(const_val_literal.clone(), value));
            (ty, Some(const_val_init))
        }

        BindingDeclKind::Var => {
            let (ty, val) = match (&item.ty, &item.init) {
                (unknown_ty, None) if !unknown_ty.is_known() => {
                    return Err(TypeError::BindingWithNoType {
                        binding_names: item.idents.clone(),
                        span: item.span.clone(),
                        value: None,
                    })
                },

                (unknown_ty, Some(init)) if !unknown_ty.is_known() => {
                    let val_expr = typecheck_expr(&init.expr, &Type::Nothing, ctx)?;
                    let actual_ty = val_expr.annotation().ty().into_owned();

                    let init = UnitBindingItemInitializer {
                        expr: Box::new(val_expr),
                        eq_span: init.eq_span.clone(),
                    };
                    (actual_ty, Some(init))
                }

                (explicit_ty, Some(init)) => {
                    let explicit_ty = typecheck_type(explicit_ty, ctx)?;
                    let val_expr = typecheck_expr(&init.expr, &explicit_ty, ctx)?;

                    let init = UnitBindingItemInitializer {
                        expr: Box::new(val_expr),
                        eq_span: init.eq_span.clone(),
                    };

                    (explicit_ty, Some(init))
                },

                (explicit_ty, None) => {
                    let explicit_ty = typecheck_type(explicit_ty, ctx)?;
                    (explicit_ty, None)
                }
            };

            // global bindings must always be initialized or be of a default-able type
            if val.is_none() {
                let has_default = ty
                    .has_default(ctx)
                    .map_err(|e| TypeError::from_name_err(e, item.span.clone()))?;

                if !has_default {
                    return Err(TypeError::UninitGlobalBinding {
                        binding_names: item.idents.clone(),
                        span: item.span.clone(),
                        ty,
                    });
                }
            }
            
            for ident in &item.idents {
                ctx.declare_global_var(ident.clone(), Binding {
                    ty: ty.clone(),
                    def: Some(ident.clone()),
                    kind: ValueKind::Mutable,
                }, visibility)?;
            }

            (ty, val)
        }
    };

    if ty == Type::Nothing {
        return Err(TypeError::BindingWithNoType {
            binding_names: item.idents.clone(),
            span: decl_span,
            value: val.map(|init| init.expr.annotation().clone()),
        });
    }

    Ok(GlobalBindingItem {
        ty,
        ty_span: item.ty_span.clone(),
        idents: item.idents.clone(),
        init: val,
        span: decl_span,
    })
}

pub fn typecheck_unit(unit_path: &PathBuf, unit: &ast::Unit, ctx: &mut Context) -> TypeResult<ModuleUnit> {
    ctx.unit_scope(unit.ident.clone(), |ctx| {
        let iface_decls = typecheck_section(&unit.iface_decls, Visibility::Interface, ctx)?;
        let impl_decls = typecheck_section(&unit.impl_decls, Visibility::Implementation, ctx)?;

        let init = match &unit.init {
            Some(init_block) => {
                Some(typecheck_init_block(&init_block, ctx)?)
            },

            None => None,
        };

        let undefined = ctx.undefined_syms();
        if !undefined.is_empty() {
            return Err(TypeError::UndefinedSymbols {
                unit: unit.ident.clone(),
                undefined,
            });
        }

        let unit_ctx = ctx.clone();

        let unit = Unit {
            unit_kw: unit.unit_kw.clone(),
            kind: unit.kind,
            ident: unit.ident.clone(),
            init,
            iface_kw: unit.iface_kw.clone(),
            iface_decls,
            impl_kw: unit.impl_kw.clone(),
            impl_decls,
            end_kw: unit.end_kw.clone(),
        };

        Ok(ModuleUnit {
            context: unit_ctx,
            path: Arc::new(unit_path.clone()),
            unit,
        })
    })
}

fn typecheck_init_block(init_block: &ast::InitBlock, ctx: &mut Context) -> TypeResult<InitBlock> {
    // init stmt is implicitly a block
    let init_env = Environment::Block {
        allow_unsafe: false,
    };

    let result = ctx.scope(init_env, |ctx| {
        let mut body = Vec::new();
        for stmt in &init_block.body {
            let stmt = typecheck_stmt(stmt, &Type::Nothing, ctx)?;
            expect_stmt_initialized(&stmt, ctx)?;

            body.push(stmt);
        }

        Ok(InitBlock {
            body,
            keyword_span: init_block.keyword_span.clone(),
            end_span: init_block.end_span.clone(),
        })
    })?;

    Ok(result)
}

fn typecheck_section(
    src_decls: &[ast::UnitDecl<Span>],
    visibility: Visibility,
    ctx: &mut Context
) -> TypeResult<Vec<UnitDecl>> {
    let mut decls = Vec::new();

    for decl in src_decls {
        decls.push(typecheck_unit_decl(decl, ctx, visibility)?);
    }
    
    Ok(decls)
}
