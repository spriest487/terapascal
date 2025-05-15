use crate::ast;
use crate::ast::const_eval::ConstEval;
use crate::ast::BindingDeclKind;
use crate::ast::FunctionName;
use crate::ast::IdentPath;
use crate::ast::StructKind;
use crate::ast::TypeAnnotation;
use crate::ast::Visibility;
use crate::typ::ast::expr::expect_stmt_initialized;
use crate::typ::ast::typecheck_alias;
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
use crate::typ::typecheck_type;
use crate::typ::Binding;
use crate::typ::ConstValue;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::ExpectedKind;
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
use common::span::Span;
use common::span::Spanned;
use std::rc::Rc;

pub type Unit = ast::Unit<Value>;
pub type UnitDecl = ast::UnitDecl<Value>;
pub type GlobalBinding = ast::UnitBinding<Value>;
pub type GlobalBindingItem = ast::UnitBindingItem<Value>;
pub type TypeDecl = ast::TypeDecl<Value>;
pub type TypeDeclItem = ast::TypeDeclItem<Value>;
pub type InitBlock = ast::InitBlock<Value>;

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
    if func_decl.name.owning_ty.is_none() && !ctx.is_function_declared(&func_decl) {
        ctx.declare_function(func_name.ident().clone(), Rc::new(func_decl.clone()), visibility)?;
    }

    let func_def = Rc::new(typecheck_func_def(func_decl.clone(), func_def, ctx)?);
    match &func_decl.name.owning_ty {
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
    let func_decl = Rc::new(FunctionDecl::typecheck(func_decl, false, ctx)?);

    assert!(
        func_decl.name.owning_ty.is_none(),
        "not yet implemented: can't forward-declare method impls"
    );

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
        items,
        span: type_decl.span.clone(),
    })
}

fn typecheck_type_decl_item(
    type_decl: &ast::TypeDeclItem,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<TypeDeclItem> {
    let full_name = Symbol::from_local_decl_name(&type_decl.name(), ctx)?;

    match type_decl {
        // except aliases, we can skip the rest of the type decl code for them
        ast::TypeDeclItem::Alias(alias_decl) => {
            let alias = typecheck_alias(full_name, alias_decl, ctx)?;

            ctx.declare_type(
                alias.name.ident().clone(),
                (*alias.ty).clone(),
                visibility,
                false
            )?;

            Ok(TypeDeclItem::Alias(Rc::new(alias)))
        }

        ast::TypeDeclItem::Struct(def) => match def.kind {
            StructKind::Class => {
                let ty = Type::class(full_name.clone());
                typecheck_type_decl_item_with_def(full_name, ty, type_decl, visibility, ctx)
            },
            StructKind::Record => {
                let ty = Type::record(full_name.clone());
                typecheck_type_decl_item_with_def(full_name, ty, type_decl, visibility, ctx)
            },
        }

        ast::TypeDeclItem::Interface(_) => {
            let ty = Type::interface(full_name.clone());
            typecheck_type_decl_item_with_def(full_name, ty, type_decl, visibility, ctx)
        },
        ast::TypeDeclItem::Variant(_) => {
            let ty = Type::variant(full_name.clone());
            typecheck_type_decl_item_with_def(full_name, ty, type_decl, visibility, ctx)
        },
        ast::TypeDeclItem::Enum(_) => {
            full_name.expect_no_type_params(InvalidTypeParamsDeclKind::Enum)?;
            
            let ty = Type::enumeration(full_name.full_path.clone());
            typecheck_type_decl_item_with_def(full_name, ty, type_decl, visibility, ctx)
        },
        ast::TypeDeclItem::Set(set_decl) => {
            typecheck_set_decl_item(set_decl, full_name, visibility, ctx)
        }
    }
}

fn typecheck_set_decl_item(
    set_decl: &ast::SetDecl,
    full_name: Symbol,
    visibility: Visibility,
    ctx: &mut Context
) -> TypeResult<TypeDeclItem> {
    let set_decl = Rc::new(SetDecl::typecheck(set_decl, full_name, ctx)?);
    
    ctx.declare_set(&set_decl, visibility)?;

    Ok(TypeDeclItem::Set(set_decl))
}

// for all cases other than aliases and sets
fn typecheck_type_decl_item_with_def(
    full_name: Symbol,
    ty: Type,
    type_decl: &ast::TypeDeclItem,
    visibility: Visibility,
    ctx: &mut Context
) -> TypeResult<TypeDeclItem> {
    // type decls have an inner scope
    let ty_scope = ctx.push_scope(Environment::TypeDecl {
        ty,
    });

    if let Some(ty_params) = &full_name.type_params {
        ctx.declare_type_params(&ty_params)?;
    }

    let type_decl = typecheck_type_decl_body(full_name, type_decl, visibility, ctx)?;

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
    name: Symbol,
    type_decl: &ast::TypeDeclItem<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<TypeDeclItem> {    
    let type_decl = match type_decl {
        ast::TypeDeclItem::Struct(class) => {
            let class = typecheck_struct_decl(name, class, visibility, ctx)?;
            ast::TypeDeclItem::Struct(Rc::new(class))
        },

        ast::TypeDeclItem::Interface(iface) => {
            let iface = typecheck_iface(name, iface, visibility, ctx)?;
            ast::TypeDeclItem::Interface(Rc::new(iface))
        },

        ast::TypeDeclItem::Variant(variant) => {
            let variant = typecheck_variant(name, variant, visibility, ctx)?;
            ast::TypeDeclItem::Variant(Rc::new(variant))
        },

        ast::TypeDeclItem::Alias(alias) => {
            let alias = typecheck_alias(name, alias, ctx)?;
            ast::TypeDeclItem::Alias(Rc::new(alias))
        },

        ast::TypeDeclItem::Enum(enum_decl) => {
            let enum_decl = typecheck_enum_decl(name, enum_decl, ctx)?;
            ast::TypeDeclItem::Enum(Rc::new(enum_decl))
        }

        ast::TypeDeclItem::Set(set_decl) => {
            let set_decl = SetDecl::typecheck(set_decl, name, ctx)?;
            ast::TypeDeclItem::Set(Rc::new(set_decl))
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
    let span = item.span().clone();

    let (ty, val) = match kind {
        BindingDeclKind::Const => {
            let (ty, const_val_expr) = match (&item.ty, &item.val) {
                (_, None) => {
                    return Err(TypeError::ConstDeclWithNoValue { span });
                }

                (unknown_ty, Some(val)) if !unknown_ty.is_known() => {
                    // infer from provided value expr
                    let init_expr = typecheck_expr(val, &Type::Nothing, ctx)?;
                    let ty = init_expr.annotation().ty().into_owned();
                    (ty, init_expr)
                }

                (explicit_ty, Some(val)) => {
                    // use explicitly provided type
                    let ty = typecheck_type(explicit_ty, ctx)?;
                    let const_val_expr = typecheck_expr(val, &ty, ctx)?;
                    (ty, const_val_expr)
                }
            };

            let const_val_literal = match const_val_expr.const_eval(ctx) {
                Some(const_val) => Ok(const_val),
                None => Err(TypeError::InvalidConstExpr {
                    expr: Box::new(const_val_expr),
                }),
            }?;

            ctx.declare_global_const(
                item.ident.clone(),
                const_val_literal.clone(),
                ty.clone(),
                visibility,
                span.clone(),
            )?;

            let annotation = ConstValue {
                value: const_val_literal.clone(),
                span: span.clone(),
                decl: Some(ctx.namespace().child(item.ident.clone())),
                ty: ty.clone(),
            };

            let val = Expr::Literal(const_val_literal.clone(), annotation.into());
            (ty, Some(Box::new(val)))
        }

        BindingDeclKind::Var => {
            let (ty, val) = match (&item.ty, &item.val) {
                (unknown_ty, None) if !unknown_ty.is_known() => {
                    return Err(TypeError::BindingWithNoType {
                        binding_name: item.ident.clone(),
                        span: item.span.clone(),
                    })
                },

                (unknown_ty, Some(val_expr)) if !unknown_ty.is_known() => {
                    let val_expr = typecheck_expr(&val_expr, &Type::Nothing, ctx)?;
                    let actual_ty = val_expr.annotation().ty().into_owned();

                    (actual_ty, Some(Box::new(val_expr)))
                }

                (explicit_ty, Some(val_expr)) => {
                    let explicit_ty = typecheck_type(explicit_ty, ctx)?;
                    let val_expr = typecheck_expr(&val_expr, &explicit_ty, ctx)?;

                    (explicit_ty, Some(Box::new(val_expr)))
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
                        ident: item.ident.clone(),
                        ty,
                    });
                }
            }
            
            ctx.declare_global_var(item.ident.clone(), Binding {
                ty: ty.clone(),
                def: Some(item.ident.clone()),
                kind: ValueKind::Mutable,
            }, visibility)?;

            (ty, val)
        }
    };

    if ty == Type::Nothing {
        return Err(TypeError::BindingWithNoType {
            binding_name: item.ident.clone(),
            span,
        });
    }

    Ok(GlobalBindingItem {
        ty,
        ident: item.ident.clone(),
        val,
        span,
    })
}

pub fn typecheck_unit(unit: &ast::Unit<Span>, ctx: &mut Context) -> TypeResult<ModuleUnit> {
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
            kind: unit.kind,
            ident: unit.ident.clone(),
            init,
            iface_decls,
            impl_decls,
        };

        Ok(ModuleUnit {
            context: unit_ctx,
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
