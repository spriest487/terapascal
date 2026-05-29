use crate::ast::FunctionDeclKind;
use crate::ast::FunctionParamItem;
use crate::ast::FunctionParamMod;
use crate::ast::FunctionParamModDecl;
use crate::ast::Ident;
use crate::ast::Visibility;
use crate::digest::DigestBuilder;
use crate::digest::DigestError;
use crate::digest::DigestResult;
use crate::ir;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDeclContext;
use crate::typ::ast::FunctionDeclMod;
use crate::typ::ast::FunctionName;
use crate::typ::ast::FunctionParamGroup;
use crate::typ::EvaluatedConstExpr;
use crate::typ::ScopeID;
use crate::typ::TypeName;
use crate::typ::TypeParam;
use crate::typ::TypeParamList;
use std::sync::Arc;
use terapascal_ir::Function;

impl DigestBuilder<'_> {
    pub fn digest_function(
        &mut self,
        func_id: ir::FunctionID,
        func_info: &ir::FunctionInfo,
    ) -> DigestResult<()> {
        match &func_info.identity {
            ir::FunctionIdentity::Path(path) => {
                let scope = match path.parent() {
                    Some(unit_path) => {
                        self.open_unit(&unit_path)?
                    }
                    None => {
                        ScopeID(0)
                    },
                };

                let Some(lib_func) = self.library.functions.get(&func_id) else {
                    return Err(DigestError::MissingFuncDef(func_id));
                };

                let decl_mods = match lib_func {
                    Function::External(extern_ref) => {
                        vec![FunctionDeclMod::External {
                            span: self.span(),
                            kw_span: self.span(),
                            src: EvaluatedConstExpr::create_string(extern_ref.src.clone(), self.span()),
                        }]
                    }
                    Function::Local(..) => {
                        Vec::new()
                    },
                };

                let type_params = if !path.type_args.is_empty() {
                    let mut params = Vec::with_capacity(path.type_args.len());

                    for type_arg in &path.type_args {
                        let ir::Type::Generic(param_name) = type_arg else {
                            return Err(DigestError::InvalidData);
                        };

                        params.push(TypeParam {
                            span: self.span(),
                            name: Ident::new(param_name, self.span()),
                            constraint: None,
                        });
                    }

                    Some(TypeParamList::new(params, self.span()))
                } else {
                    None
                };

                let tags = self.digest_tags(&func_info.tags)?;

                let func_ident = Ident::new(path.name(), self.span());

                let result_type = self.digest_type(&func_info.sig.result_type)?;

                let mut param_groups = Vec::with_capacity(func_info.sig.param_types.len());
                for (i, param_type) in func_info.sig.param_types.iter().enumerate() {
                    let (param_type, modifier) = match param_type {
                        ir::Type::TempRef(deref_type) => {
                            let param_type = self.digest_type(deref_type)?;
                            let mod_decl = FunctionParamModDecl {
                                span: self.span(),
                                param_mod: FunctionParamMod::Var,
                            };
                            (param_type, Some(mod_decl))
                        }

                        t => {
                            let param_type = self.digest_type(t)?;
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

                let func_decl = FunctionDecl {
                    span: self.span(),
                    name: FunctionName {
                        ident: func_ident.clone(),
                        span: self.span(),
                        context: FunctionDeclContext::FreeFunction,
                        type_params,
                    },
                    kind: FunctionDeclKind::Function,
                    tags,
                    mods: decl_mods,
                    result_ty: TypeName::Unspecified(result_type),
                    param_groups,
                    kw_span: self.span(),
                    where_clause: None,
                };

                if let Some(ctx) = self.root_ctx.as_mut() {
                    ctx.declare_function(func_ident, Arc::new(func_decl), Visibility::Interface)?;

                    ctx.pop_scope(scope);
                }
            }

            ir::FunctionIdentity::Method { .. } => {
                // TODO
            }

            ir::FunctionIdentity::Destructor { .. } | ir::FunctionIdentity::Internal(..) => {
                // ignored
            }
        }

        Ok(())
    }
}