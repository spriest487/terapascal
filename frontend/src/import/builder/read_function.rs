use crate::ast::Access;
use crate::ast::FunctionDeclKind;
use crate::ast::FunctionParamItem;
use crate::ast::FunctionParamMod;
use crate::ast::FunctionParamModDecl;
use crate::ast::Visibility;
use crate::codegen::FunctionDeclKey;
use crate::codegen::FunctionInstance;
use crate::codegen::MethodDeclKey;
use crate::import::ImportBuilder;
use crate::import::ImportError;
use crate::import::ImportResult;
use crate::ir;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDeclContext;
use crate::typ::ast::FunctionDeclMod;
use crate::typ::ast::FunctionName;
use crate::typ::ast::FunctionParamGroup;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::Tag;
use crate::typ::EvaluatedConstExpr;
use crate::typ::ScopeID;
use crate::typ::Type;
use crate::typ::TypeName;
use std::collections::BTreeMap;
use std::sync::Arc;
use terapascal_common::ident::Ident;

impl ImportBuilder<'_> {
    pub fn read_params(
        &mut self,
        params: &[ir::FunctionParamInfo],
    ) -> ImportResult<Vec<FunctionParamGroup>> {
        let mut param_groups = Vec::with_capacity(params.len());
        for (i, param_info) in params.iter().enumerate() {
            let (param_type, modifier) = match &param_info.param_type {
                // reference params are a per-param modifier in Pascal, and a separate type in IR
                ir::Type::TempRef(deref_type) => {
                    let param_type = self.read_type(deref_type)?;

                    let has_out_tag = self.out_param_tag_info
                        .map(|out_info| param_info.tags
                            .iter()
                            .any(|t| t.class_id == out_info.class_id))
                        .unwrap_or(false);

                    // out params are marked by Terapascal with a tag
                    let param_mod = if has_out_tag {
                        FunctionParamMod::Out
                    } else {
                        FunctionParamMod::Var
                    };

                    let mod_decl = FunctionParamModDecl {
                        span: self.span(),
                        param_mod,
                    };

                    (param_type, Some(mod_decl))
                }

                t => {
                    let param_type = self.read_type(t)?;
                    (param_type, None)
                }
            };

            param_groups.push(FunctionParamGroup {
                param_items: vec![FunctionParamItem {
                    name: Arc::new(format!("arg{i}")),
                    is_implicit_self: false,
                    name_span: None,
                }],
                ty: TypeName::Unspecified(param_type),
                modifier,
                span: None,
            });
        }

        Ok(param_groups)
    }

    pub fn read_function(
        &mut self,
        func_id: ir::FunctionID,
        func_info: &ir::FunctionInfo,
    ) -> ImportResult<()> {
        let tags = self.read_tags(&func_info.tags)?;

        let result_type = self.read_type(&func_info.result_type)?;
        let param_groups = self.read_params(&func_info.params)?;

        match &func_info.identity {
            ir::FunctionIdentity::Global(func_name) => {
                self.read_free_function(func_id, func_info, tags, result_type, param_groups, &func_name)?;
            }

            ir::FunctionIdentity::Method { declaring_type, id, name, type_params } => {
                self.read_method(func_id, tags, result_type, param_groups, declaring_type, *id, name, type_params)?;
            }

            ir::FunctionIdentity::Destructor { declaring_type, id, name} => {
                self.read_method(func_id, tags, result_type, param_groups, declaring_type, *id, name, &[])?;
            }

            ir::FunctionIdentity::Internal { .. } => {
                // ignored
            }
        }

        Ok(())
    }

    fn read_method(&mut self,
        func_id: ir::FunctionID,
        tags: Vec<Tag>,
        result_type: Type,
        param_groups: Vec<FunctionParamGroup>,
        declaring_type: &ir::Type,
        method_id: ir::MethodID,
        name: &str,
        type_params: &[ir::TypeParam],
    ) -> ImportResult<()> {
        // the type used as the declaring type differs slightly between the IR and Pascal
        // data: in IR it's the generic name (path parameterized with generic placeholders),
        // in Pascal it's the unspecialized definition name (path with params but empty args)
        let declaring_type = match self.read_type(declaring_type)? {
            Type::Record(name) => {
                Type::record(name.as_ref().clone().with_type_args(None))
            }

            Type::Class(name) => {
                Type::class(name.as_ref().clone().with_type_args(None))
            }

            Type::Variant(name) => {
                Type::variant(name.as_ref().clone().with_type_args(None))
            }

            Type::Interface(name) => {
                Type::interface(name.as_ref().clone().with_type_args(None))
            }

            ty => ty.clone(),
        };

        let decl_key = FunctionDeclKey::Method(MethodDeclKey {
            self_ty: declaring_type.clone(),
            method_index: method_id.0,
        });

        if self.imported_funcs.contains_key(&decl_key) {
            return Ok(());
        }

        let type_params = self.read_def_params(type_params)?;

        let func_decl = Arc::new(FunctionDecl {
            span: self.span(),
            name: FunctionName {
                ident: Ident::new(name, self.span()),
                span: None,
                context: FunctionDeclContext::MethodDef {
                    declaring_type: TypeName::Unspecified(declaring_type.clone()),
                },
                type_params,
            },
            kind: FunctionDeclKind::Function,
            tags,
            mods: Vec::new(),
            result_ty: TypeName::Unspecified(result_type),
            param_groups,
            kw_span: None,
            where_clause: None,
        });

        let decl_sig = Arc::new(func_decl.sig());

        let method_list = self.type_methods
            .entry(declaring_type.clone())
            .or_insert_with(|| BTreeMap::new());
        method_list.insert(method_id.0, MethodDecl {
            func_decl,
            access: Access::Published, // TODO: access modifiers in IR
        });

        self.imported_funcs.insert(decl_key, FunctionInstance {
            id: func_id,
            published: true,
            src_sig: decl_sig,
        });
        Ok(())
    }

    fn read_free_function(
        &mut self,
        func_id: ir::FunctionID,
        func_info: &ir::FunctionInfo,
        tags: Vec<Tag>,
        result_type: Type,
        param_groups: Vec<FunctionParamGroup>,
        func_name: &ir::DeclPath,
    ) -> ImportResult<()> {
        let Some(lib_func) = self.library.functions.get(&func_id) else {
            return Err(ImportError::MissingFuncDef(func_id));
        };

        let func_name = self.read_decl_path(func_name)?;

        // no early returns after this! we need to always close the scope
        let scope = match func_name.full_path.parent() {
            Some(unit_path) => {
                self.open_unit(unit_path)?
            }
            None => {
                ScopeID(0)
            },
        };

        let decl_mods = match lib_func {
            ir::Function::External(extern_ref) => {
                vec![FunctionDeclMod::External {
                    span: self.span(),
                    kw_span: self.span(),
                    src: EvaluatedConstExpr::create_string(extern_ref.src.clone(), self.span()),
                }]
            }
            ir::Function::Local(..) => {
                Vec::new()
            },
        };

        let func_ident = func_name.full_path.last().clone();

        let func_decl = FunctionDecl {
            span: self.span(),
            name: FunctionName {
                ident: func_ident.clone(),
                span: None,
                context: FunctionDeclContext::FreeFunction,
                type_params: func_name.type_params.clone(),
            },
            kind: FunctionDeclKind::Function,
            tags,
            mods: decl_mods,
            result_ty: TypeName::Unspecified(result_type),
            param_groups,
            kw_span: None,
            where_clause: None,
        };

        let decl_sig = Arc::new(func_decl.sig());

        let decl_key = FunctionDeclKey::Function {
            sig: decl_sig.clone(),
            name: func_name.full_path,
        };

        if !self.imported_funcs.contains_key(&decl_key) {
            let func_decl = Arc::new(func_decl);

            if let Some(ctx) = self.root_ctx.as_mut() {
                let visibility = Visibility::Interface; // TODO: access modifiers in IR

                ctx.declare_external_func(func_decl, visibility)?;
            }

            self.imported_funcs.insert(decl_key, FunctionInstance {
                id: func_id,
                published: func_info.runtime_name.is_some(), // TODO: access modifiers in IR
                src_sig: decl_sig
            });
        }

        if let Some(ctx) = self.root_ctx.as_mut() {
            ctx.pop_scope(scope);
        }

        Ok(())
    }
}