use crate::ast::Access;
use crate::ast::FunctionDeclKind;
use crate::ast::FunctionParamItem;
use crate::ast::FunctionParamMod;
use crate::ast::FunctionParamModDecl;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Visibility;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::library_builder::MethodDeclKey;
use crate::codegen::FunctionInstance;
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

impl ImportBuilder<'_> {
    pub fn read_function(
        &mut self,
        func_id: ir::FunctionID,
        func_info: &ir::FunctionInfo,
    ) -> ImportResult<()> {
        let tags = self.read_tags(&func_info.tags)?;

        let result_type = self.read_type(&func_info.sig.result_type)?;

        let mut param_groups = Vec::with_capacity(func_info.sig.param_types.len());
        for (i, param_type) in func_info.sig.param_types.iter().enumerate() {
            let (param_type, modifier) = match param_type {
                ir::Type::TempRef(deref_type) => {
                    let param_type = self.read_type(deref_type)?;
                    let mod_decl = FunctionParamModDecl {
                        span: self.span(),
                        param_mod: FunctionParamMod::Var,
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

        match &func_info.identity {
            ir::FunctionIdentity::Path(path) => {
                self.read_free_function(func_id, func_info, tags, result_type, param_groups, &path)?;
            }

            ir::FunctionIdentity::Method { declaring_type, id, name, type_args } => {
                self.read_method(func_id, tags, result_type, param_groups, declaring_type, *id, name, type_args)?;
            }

            ir::FunctionIdentity::Destructor { declaring_type, id, name} => {
                self.read_method(func_id, tags, result_type, param_groups, declaring_type, *id, name, &[])?;
            }

            ir::FunctionIdentity::Internal(..) => {
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
        type_args: &[ir::Type],
    ) -> ImportResult<()> {
        let declaring_type = self.read_type(declaring_type)?;

        // primitive and pointer methods are hardcoded and don't need to be imported
        if matches!(declaring_type, Type::Primitive(..) | Type::Pointer(..)) {
            return Ok(());
        }

        let decl_key = FunctionDeclKey::Method(MethodDeclKey {
            self_ty: declaring_type.clone(),
            method_index: method_id.0,
        });

        let type_params = self.read_def_type_params(type_args)?;

        let func_decl = Arc::new(FunctionDecl {
            span: self.span(),
            name: FunctionName {
                ident: Ident::new(name, self.span()),
                span: None,
                context: FunctionDeclContext::MethodDecl {
                    enclosing_type: declaring_type.clone(),
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

    fn read_free_function(&mut self,
        func_id: ir::FunctionID,
        func_info: &ir::FunctionInfo,
        tags: Vec<Tag>,
        result_type: Type,
        param_groups: Vec<FunctionParamGroup>,
        path: &ir::NamePath
    ) -> ImportResult<()> {
        let scope = match path.parent() {
            Some(unit_path) => {
                self.open_unit(self.read_ident_path(&unit_path))?
            }
            None => {
                ScopeID(0)
            },
        };

        let Some(lib_func) = self.library.functions.get(&func_id) else {
            return Err(ImportError::MissingFuncDef(func_id));
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

        let type_params = self.read_def_type_params(&path.type_args)?;

        let func_path = IdentPath::from_parts(path.path
            .iter()
            .map(|part| Ident::new(part, self.span())));

        let func_ident = func_path.last().clone();

        let func_decl = FunctionDecl {
            span: self.span(),
            name: FunctionName {
                ident: func_ident.clone(),
                span: None,
                context: FunctionDeclContext::FreeFunction,
                type_params,
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

        if let Some(ctx) = self.root_ctx.as_mut() {
            ctx.declare_function(func_ident.clone(), Arc::new(func_decl), Visibility::Interface)?;

            ctx.pop_scope(scope);
        }

        let decl_key = FunctionDeclKey::Function {
            sig: decl_sig.clone(),
            name: func_path,
        };

        self.imported_funcs.insert(decl_key, FunctionInstance {
            id: func_id,
            published: func_info.runtime_name.is_some(), // TODO: access modifiers in IR
            src_sig: decl_sig
        });

        Ok(())
    }
}