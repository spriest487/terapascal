mod task;
mod unit_typedecl;

pub use self::unit_typedecl::*;
use crate::ast;
use crate::ast::BindingDeclKind;
use crate::ast::FunctionName;
use crate::ast::IdentPath;
use crate::ast::SemanticHint;
use crate::ast::UnitBindingItemInitializer;
use crate::ast::UnitDeclSection;
use crate::ast::Visibility;
use crate::result::ErrorContinue;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::expr::expect_stmt_initialized;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_stmt;
use crate::typ::ast::unit::task::UnitDeclTask;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDeclContext;
use crate::typ::ast::WhereClause;
use crate::typ::typecheck_typename;
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
use crate::typ::TypeName;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::ValueKind;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type Unit = ast::Unit<Value>;
pub type UnitDecl = ast::UnitDecl<Value>;
pub type GlobalBinding = ast::UnitBinding<Value>;
pub type GlobalBindingItem = ast::UnitBindingItem<Value>;
pub type InitBlock = ast::InitBlock<Value>;

#[derive(Debug)]
pub struct TypeDeclItemInfo {
    pub name: Symbol,
    pub where_clause: Option<WhereClause>,
    pub visibility: Visibility,
}

impl TypeDeclItemInfo {
    pub fn expect_not_generic(
        &self,
        kind: InvalidTypeParamsDeclKind,
        item_span: &Span,
    ) -> TypeResult<()> {
        if self.name.type_params.is_some() {
            return Err(TypeError::InvalidDeclWithTypeParams {
                kind,
                span: item_span.clone(),
            });
        }

        if self.name.type_params.is_some() {
            return Err(TypeError::from_generic_err(
                GenericError::UnexpectedConstraintList,
                item_span.clone(),
            ));
        }

        Ok(())
    }
}

fn typecheck_unit_decl(
    decl: &ast::UnitDecl<Span>,
    ctx: &mut Context,
    visibility: Visibility,
) -> TypeResult<UnitDeclTask> {
    match decl {
        ast::UnitDecl::Uses { decl: uses } => {
            for use_item in &uses.units {
                typecheck_unit_uses_decl(use_item, ctx)?;
            }

            let item = ast::UnitDecl::Uses { decl: uses.clone() };

            Ok(UnitDeclTask::Done(item))
        },

        ast::UnitDecl::FunctionDef { def: func_def } => {
            typecheck_unit_func_def(func_def, visibility, ctx)
        },

        ast::UnitDecl::FunctionDecl { decl: func_decl } => {
            let item = typecheck_unit_func_decl(func_decl, visibility, ctx)?;

            Ok(UnitDeclTask::Done(item))
        },

        ast::UnitDecl::Type { decl: type_decl } => {
            let item = typecheck_unit_type_decl(type_decl, visibility, ctx)?;

            Ok(UnitDeclTask::Done(item))
        },

        ast::UnitDecl::Binding { decl } => {
            let decl = typecheck_global_binding(decl, visibility, ctx)?;
            let item = ast::UnitDecl::Binding { decl };

            Ok(UnitDeclTask::Done(item))
        },
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
) -> TypeResult<UnitDeclTask> {
    let func_decl = FunctionDecl::typecheck(&func_def.decl, true, ctx);
    let func_name = &func_decl.name;

    // free functions may not already have a declaration in scope if they weren't forward
    // declared, do that now
    if func_decl.name.context == FunctionDeclContext::FreeFunction
        && !ctx.is_function_declared(&func_decl)
    {
        ctx.declare_function(
            func_name.ident().clone(),
            Arc::new(func_decl.clone()),
            visibility,
        )?;
    }

    let task = UnitDeclTask::DeferredFuncDef {
        src_def: func_def.clone(),
        body_ctx: ctx.branch(),
        func_decl,
    };

    Ok(task)
}

fn typecheck_unit_func_decl(
    func_decl: &ast::FunctionDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<UnitDecl> {
    let name = func_decl.name.clone();
    let func_decl = Arc::new(FunctionDecl::typecheck(func_decl, false, ctx));

    // this is always true in valid code - only free functions can be declared without definition
    // at the unit level - but we might be typechecking partially parsed code
    if matches!(func_decl.name.context, FunctionDeclContext::FreeFunction) {
        ctx.declare_function(name.ident().clone(), func_decl.clone(), visibility)?;
    }

    Ok(UnitDecl::FunctionDecl { decl: func_decl })
}

pub fn typecheck_unit_type_decl(
    type_decl: &ast::TypeDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<UnitDecl> {
    let decl = type_decl.typecheck(visibility, ctx)?;

    Ok(UnitDecl::Type { decl })
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
            let (typename, mut const_val_init) = match (&item.ty, &item.init) {
                (_, None) => {
                    return Err(TypeError::ConstDeclWithNoValue { span: decl_span });
                },

                (unknown_ty, Some(init)) if !unknown_ty.is_known() => {
                    // infer from provided value expr
                    let init_expr = typecheck_expr(&init.expr, &Type::Nothing, ctx)?;
                    let ty = init_expr.annotation().ty().into_owned();

                    (TypeName::inferred(ty), UnitBindingItemInitializer {
                        expr: Box::new(init_expr),
                        eq_span: init.eq_span.clone(),
                    })
                },

                (explicit_ty, Some(init)) => {
                    // use explicitly provided type
                    let ty = typecheck_typename(explicit_ty, ctx)?;
                    let const_val_expr = typecheck_expr(&init.expr, &ty, ctx)?;

                    (ty, UnitBindingItemInitializer {
                        expr: Box::new(const_val_expr),
                        eq_span: init.eq_span.clone(),
                    })
                },
            };

            let val_span = const_val_init.expr.span().clone();

            let const_val_literal = match const_val_init.expr.const_eval(ctx) {
                Some(const_val) => Ok(const_val),
                None => Err(TypeError::InvalidConstExpr {
                    expr: const_val_init.expr,
                }),
            }?;

            assert_eq!(
                1,
                item.idents.len(),
                "parser should not produce multi-item const bindings"
            );
            let const_ident = item.idents[0].clone();

            ctx.declare_global_const(
                const_ident.clone(),
                const_val_literal.clone(),
                typename.ty().clone(),
                visibility,
                const_ident.span.clone(),
            )?;

            let value = ConstValue {
                value: const_val_literal.clone(),
                span: val_span,
                decl: Some(ctx.namespace().child(const_ident)),
                ty: typename.ty().clone(),
            };

            const_val_init.expr = Box::new(Expr::literal(const_val_literal.clone(), value));
            (typename, Some(const_val_init))
        },

        BindingDeclKind::Var => {
            let (typename, val) = match (&item.ty, &item.init) {
                (unknown_ty, None) if !unknown_ty.is_known() => {
                    return Err(TypeError::BindingWithNoType {
                        binding_names: item.idents.clone(),
                        span: item.span.clone(),
                        value: None,
                    });
                },

                (unknown_ty, Some(init)) if !unknown_ty.is_known() => {
                    let val_expr = typecheck_expr(&init.expr, &Type::Nothing, ctx)?;
                    let actual_ty = val_expr.annotation().ty().into_owned();

                    let init = UnitBindingItemInitializer {
                        expr: Box::new(val_expr),
                        eq_span: init.eq_span.clone(),
                    };
                    (TypeName::inferred(actual_ty), Some(init))
                },

                (explicit_ty, Some(init)) => {
                    let explicit_ty = typecheck_typename(explicit_ty, ctx)?;
                    let val_expr = typecheck_expr(&init.expr, &explicit_ty, ctx)?;

                    let init = UnitBindingItemInitializer {
                        expr: Box::new(val_expr),
                        eq_span: init.eq_span.clone(),
                    };

                    (explicit_ty, Some(init))
                },

                (explicit_ty, None) => {
                    let explicit_ty = typecheck_typename(explicit_ty, ctx)?;
                    (explicit_ty, None)
                },
            };

            // global bindings must always be initialized or be of a default-able type
            if val.is_none() {
                let has_default = typename
                    .has_default(ctx)
                    .map_err(|e| TypeError::from_name_err(e, item.span.clone()))?;

                if !has_default {
                    return Err(TypeError::UninitGlobalBinding {
                        binding_names: item.idents.clone(),
                        span: item.span.clone(),
                        ty: typename.ty().clone(),
                    });
                }
            }

            for ident in &item.idents {
                ctx.declare_global_var(
                    ident.clone(),
                    Binding {
                        ty: typename.ty().clone(),
                        def: Some(ident.clone()),
                        kind: ValueKind::Mutable,
                        semantic_hint: SemanticHint::Variable,
                    },
                    visibility,
                )?;
            }

            (typename, val)
        },
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

pub fn typecheck_unit(
    unit_path: &PathBuf,
    unit: &ast::Unit,
    ctx: &mut Context,
) -> TypeResult<ModuleUnit> {
    let mut module_unit = ctx.unit_scope(unit.ident.clone(), |ctx| {
        let iface_decls = typecheck_section(&unit.iface_section.decls, Visibility::Interface, ctx);
        let impl_decls =
            typecheck_section(&unit.impl_section.decls, Visibility::Implementation, ctx);

        let init = match &unit.init {
            Some(init_block) => Some(typecheck_init_block(&init_block, ctx)),

            None => None,
        };

        let undefined = ctx.undefined_syms();
        if !undefined.is_empty() {
            ctx.error(TypeError::UndefinedSymbols {
                unit: unit.ident.clone(),
                undefined,
            });
        }

        let unit_ctx = ctx.branch();

        let unit = Unit {
            unit_kw: unit.unit_kw.clone(),
            kind: unit.kind,
            ident: unit.ident.clone(),
            init,
            iface_section: UnitDeclSection {
                kw_span: unit.iface_section.kw_span.clone(),
                decls: iface_decls,
            },
            impl_section: UnitDeclSection {
                kw_span: unit.impl_section.kw_span.clone(),
                decls: impl_decls,
            },
            end_kw: unit.end_kw.clone(),
        };

        Ok(ModuleUnit {
            context: unit_ctx,
            path: Arc::new(unit_path.clone()),
            unit,
        })
    })?;

    ctx.end_branch(&mut module_unit.context);

    Ok(module_unit)
}

fn typecheck_init_block(init_block: &ast::InitBlock, ctx: &mut Context) -> InitBlock {
    // init stmt is implicitly a block
    let init_env = Environment::Block {
        allow_unsafe: false,
    };

    let result = ctx.scope(init_env, |ctx| {
        let mut body = Vec::new();

        for stmt in &init_block.body {
            if let Some(stmt) = typecheck_stmt(stmt, &Type::Nothing, ctx).ok_or_continue(ctx) {
                expect_stmt_initialized(&stmt, ctx).or_continue(ctx, ());

                body.push(stmt);
            }
        }

        InitBlock {
            body,
            keyword_span: init_block.keyword_span.clone(),
            end_span: init_block.end_span.clone(),
        }
    });

    result
}

fn typecheck_section(
    src_decls: &[ast::UnitDecl<Span>],
    visibility: Visibility,
    ctx: &mut Context,
) -> Vec<UnitDecl> {
    let mut decl_tasks = Vec::with_capacity(src_decls.len());

    for decl in src_decls {
        if let Some(decl) = typecheck_unit_decl(decl, ctx, visibility).ok_or_continue(ctx) {
            decl_tasks.push(decl);
        }
    }
    
    UnitDeclTask::run_parallel(decl_tasks, ctx)
}
