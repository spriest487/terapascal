use crate::ast;
use crate::codegen::build_func_def;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::var_param::OutParamTagInfo;
use crate::codegen::FunctionInstance;
use crate::ir;
use crate::typ;
use crate::typ::builtin_ident;
use crate::typ::free_mem_sig;
use crate::typ::get_mem_sig;
use crate::typ::MethodDef;
use crate::typ::SYSTEM_UNIT_NAME;
use ir::MetadataSource;
use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::ident::IdentPath;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDeclKey {
    Function {
        name: IdentPath,
        sig: Arc<typ::FunctionSig>,
    },
    Method(MethodDeclKey),
    VirtualMethod(VirtualMethodKey),
}

impl FunctionDeclKey {
    pub fn namespace(&self) -> Cow<'_, IdentPath> {
        match self {
            FunctionDeclKey::Function { name, .. } => name
                .parent()
                .map(Cow::Owned)
                .expect("all functions must be declared within a namespace!"),

            FunctionDeclKey::VirtualMethod(key) => {
                key.iface_ty.full_path().expect("types used as interfaces should never be unnamed")
            },

            FunctionDeclKey::Method(key) => key
                .self_ty
                .full_path()
                .expect("types with method implementations should never be unnamed"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDeclKey {
    pub self_ty: typ::Type,
    pub method_index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VirtualMethodKey {
    pub iface_ty: typ::Type,
    pub iface_method_index: usize,

    pub impl_method: MethodDeclKey,
}

impl<'a> LibraryBuilder<'a> {
    pub(crate) fn instantiate_func(&mut self, key: &FunctionDeclKey) -> FunctionInstance {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        if let Some(imported_func) = self.find_imported_func(key) {
            return imported_func.clone();
        }

        match key {
            FunctionDeclKey::Function { name, sig } => {
                match self.root_ctx.find_func_def(&name, sig.clone()).cloned() {
                    Some(typ::Def::Function(func_def)) => {
                        self.instantiate_func_def(&func_def, key.clone())
                    },

                    Some(typ::Def::External(extern_decl)) => {
                        self.instantiate_func_external(&extern_decl, key.clone())
                    },

                    Some(other) => {
                        panic!("wrong kind of source def for function: {}", other.ident())
                    }

                    None => {
                        panic!("missing source def for function {}", name)
                    },
                }
            },

            FunctionDeclKey::Method(method_key) => {
                self.instantiate_method(method_key)
            },

            FunctionDeclKey::VirtualMethod(virtual_key) => {
                self.instantiate_virtual_method(virtual_key)
            }
        }
    }

    fn find_imported_func(&self, key: &FunctionDeclKey) -> Option<&FunctionInstance> {
        for lib_ref in self.references {
            if let Some(imported_func) = lib_ref.imported_funcs.get(key) {
                return Some(imported_func);
            }
        }

        None
    }

    fn instantiate_system_func(&mut self, name: &str, sig: Arc<typ::FunctionSig>) -> ir::FunctionID {
        let ident_path = IdentPath::new(builtin_ident(name), [
            builtin_ident(SYSTEM_UNIT_NAME),
        ]);

        let instance = self.instantiate_func(&FunctionDeclKey::Function {
            name: ident_path,
            sig,
        });

        instance.id
    }

    pub(crate) fn instantiate_get_mem_func(&mut self) -> ir::FunctionID {
        match self.get_mem_func {
            Some(id) => id,
            None => {
                let id = self.instantiate_system_func("GetMem", Arc::new(get_mem_sig()));
                self.get_mem_func = Some(id);
                id
            }
        }
    }

    pub(crate) fn instantiate_free_mem_func(&mut self) -> ir::FunctionID {
        match self.free_mem_func {
            Some(id) => id,
            None => {
                let id = self.instantiate_system_func("FreeMem", Arc::new(free_mem_sig()));
                self.free_mem_func = Some(id);
                id
            }
        }
    }

    fn instantiate_func_def(
        &mut self,
        func_def: &typ::ast::FunctionDef,
        key: FunctionDeclKey,
    ) -> FunctionInstance {
        // eprintln!("instantiate_func_def: {}", func_def.decl.name);

        let ns = key.namespace().into_owned();

        let debug_name = func_def.decl.name.to_debug_string(None);
        let published = func_def.decl.is_published();

        let src_sig = Arc::new(func_def.decl.sig());

        let id = self.declare_func(&func_def.decl, ns);

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let func_instance = FunctionInstance {
            id,
            src_sig,

            published,
        };

        self.translated_funcs.insert(key, func_instance.clone());

        let def = build_func_def(
            self,
            &func_def.decl.param_groups,
            &func_def.decl.result_ty,
            &func_def.body,
            &func_def.locals,
            false,
            Some(debug_name),
        );

        self.functions.insert(id, ir::Function::Local(def));

        func_instance
    }

    fn instantiate_func_external(
        &mut self,
        extern_decl: &typ::ast::FunctionDecl,
        key: FunctionDeclKey,
    ) -> FunctionInstance {
        let decl_namespace = key.namespace().into_owned();
        let id = self.declare_func(&extern_decl, decl_namespace);

        let sig = extern_decl.sig();
        let result_type = self.translate_type(&sig.result_ty);

        let param_types: Vec<_> = sig.params
            .iter()
            .map(|sig_param| {
                let param_ty = self.translate_type(&sig_param.ty);
                match sig_param.modifier {
                    None => {
                        param_ty
                    },
                    Some(ast::FunctionParamMod::Var | ast::FunctionParamMod::Out) => {
                        param_ty.temp_ref()
                    },
                }
            })
            .collect();

        let cached_func = FunctionInstance {
            id,
            src_sig: Arc::new(sig),

            published: extern_decl.is_published(),
        };

        let extern_src = extern_decl
            .external_src()
            .expect("function with external def must have an extern src");

        self.functions.insert(
            id,
            ir::Function::External(ir::ExternalFunctionRef {
                src: extern_src.value.clone(),
                symbol: extern_decl.name.ident.name.clone(),
                sig: Rc::new(ir::FunctionSig::new(param_types, result_type))
            }),
        );

        self.translated_funcs.insert(key, cached_func.clone());

        cached_func
    }

    fn instantiate_method(&mut self, method_key: &MethodDeclKey) -> FunctionInstance {
        // eprintln!("instantiate_method: {:#?}", method_key);

        // if the self type is a parameterized generic, we'll need to instantiate the specialized
        // def here, since only the original declared version will be in the definition map
        let decl_self_ty = match &method_key.self_ty {
            typ::Type::Class(sym) if sym.type_args.is_some() => {
                typ::Type::class(sym.as_ref().clone().with_type_args(None))
            }

            typ::Type::Record(sym) if sym.type_args.is_some() => {
                typ::Type::record(sym.as_ref().clone().with_type_args(None))
            }

            typ::Type::Variant(sym) if sym.type_args.is_some() => {
                typ::Type::variant(sym.as_ref().clone().with_type_args(None))
            }

            // nothing to do if the type isn't parameterized
            _ => {
                assert_eq!(
                    typ::TypeArgsResult::NotGeneric,
                    method_key.self_ty.type_args(),
                    "expected non-generic self type for instantiation of {} method {} without type args",
                    method_key.self_ty,
                    method_key.method_index
                );

                method_key.self_ty.clone()
            }
        };

        let method_decl = decl_self_ty
            .get_method(method_key.method_index, &self.root_ctx)
            .unwrap_or_else(|e| {
                panic!("instantiate_method: failed to get method metadata for {} method {}: {}", decl_self_ty, method_key.method_index, e)
            });

        let method_sig = Arc::new(method_decl.func_decl.sig());

        let is_instance_method = !method_decl.func_decl.kind.is_static_method();

        let method_def = self.root_ctx
            .find_method_def(&decl_self_ty, method_decl.func_decl.ident(), &method_sig)
            .cloned()
            .unwrap_or_else(|| {
                panic!("instantiate_method: missing method def: {} (method {})", method_decl.func_decl, method_key.method_index)
            });

        match method_def {
            MethodDef::Function(func_def) => {
                let id = self.declare_method(&method_decl.func_decl, method_key.method_index);
                let sig = Arc::new(method_decl.func_decl.sig());

                // cache the function before translating the instantiation, because
                // it may recurse and instantiate itself in its own body
                let func_instance = FunctionInstance {
                    id,
                    src_sig: sig,
                    published: method_decl.is_published(),
                };

                let key = FunctionDeclKey::Method(method_key.clone());
                self.translated_funcs.insert(key, func_instance.clone());

                let debug_name = func_def.decl.name.to_debug_string(None);

                let def = build_func_def(
                    self,
                    &func_def.decl.param_groups,
                    &func_def.decl.result_ty,
                    &func_def.body,
                    &func_def.locals,
                    is_instance_method,
                    Some(debug_name),
                );
                self.functions.insert(id, ir::Function::Local(def));

                func_instance
            }

            MethodDef::External(decl) => {
                // different to the instance key (self type is unspecialized)
                let decl_key = FunctionDeclKey::Method(MethodDeclKey {
                    method_index: method_key.method_index,
                    self_ty: decl_self_ty.clone(),
                });

                // we shouldn't be trying to instantiate it - it should already be in the cache
                self.find_imported_func(&decl_key)
                    .unwrap_or_else(|| {
                        eprintln!("{:#?}", decl_self_ty);
                        panic!("instantiate_method: reference to external method `{}` which is not imported", decl.name)
                    })
                    .clone()
            }
        }
    }

    fn instantiate_virtual_method(&mut self, virtual_key: &VirtualMethodKey) -> FunctionInstance {
        let impl_method = &virtual_key.impl_method;

        // virtual methods can't be generic
        let self_ty = self.translate_type(&impl_method.self_ty);

        // virtual calls can't have type args yet
        let impl_key = FunctionDeclKey::Method(impl_method.clone());
        let impl_func = self.instantiate_func(&impl_key);

        let iface_type = self.translate_type(&virtual_key.iface_ty);
        let ir::Type::Object(ir::ObjectID::Interface(iface_ref)) = iface_type else {
            panic!("instantiate_virtual_method: interface type {} did not translate to an interface", virtual_key.iface_ty);
        };

        let iface_method_decl = virtual_key.iface_ty
            .get_method(virtual_key.iface_method_index, &self.root_ctx)
            .unwrap_or_else(|e| {
                panic!("instantiate_virtual_method: failed to get {} method {}: {}", impl_method.self_ty, impl_method.method_index, e)
            });

        let method_name = iface_method_decl.func_decl.ident().to_string();

        self.metadata.impl_method(iface_ref, self_ty, method_name, impl_func.id);

        let key = FunctionDeclKey::VirtualMethod(virtual_key.clone());

        self.translated_funcs.insert(key.clone(), impl_func.clone());

        impl_func
    }

    pub fn declare_func(
        &mut self,
        func_decl: &typ::ast::FunctionDecl,
        namespace: IdentPath,
    ) -> ir::FunctionID {
        let decl_type_params = func_decl.name.type_params.as_ref();

        let identity = match &func_decl.name.context {
            typ::ast::FunctionDeclContext::FreeFunction if func_decl.is_overload() => {
                let type_params = self.translate_type_param_list(decl_type_params);
                ir::FunctionIdentity::internal(func_decl.to_string(), type_params)
            }

            typ::ast::FunctionDeclContext::FreeFunction => {
                let ns: Vec<_> = namespace
                    .iter()
                    .map(|part| Arc::new(part.to_string()))
                    .collect();
                let name = func_decl.name.ident.name.clone();

                let mut func_name = ir::DeclPath::new(ns, name);

                let type_params = self.translate_type_param_list(decl_type_params);
                func_name.type_params = type_params;

                ir::FunctionIdentity::Global(func_name)
            }

            typ::ast::FunctionDeclContext::MethodDef { .. }
            | typ::ast::FunctionDeclContext::MethodDecl { ..} => {
                panic!("declare_func: {} is a method", func_decl.name)
            }
        };

        self.insert_func_decl(identity, func_decl)
    }

    pub fn declare_method(
        &mut self,
        method_decl: &typ::ast::FunctionDecl,
        method_index: usize,
    ) -> ir::FunctionID {
        let identity = match &method_decl.name.context {
            typ::ast::FunctionDeclContext::MethodDecl { enclosing_type } => {
                self.method_identity(method_decl, enclosing_type, method_index)
            }

            typ::ast::FunctionDeclContext::MethodDef { declaring_type } => {
                self.method_identity(method_decl, declaring_type.ty(), method_index)
            }

            _ => {
                panic!("declare_method: {} is not a method", method_decl.name)
            }
        };

        self.insert_func_decl(identity, method_decl)
    }

    pub fn translate_param_groups(
        &mut self,
        groups: &[typ::ast::FunctionParamGroup],
    ) -> Vec<ir::FunctionParamInfo> {
        let mut param_infos = Vec::with_capacity(groups.len());

        for group in groups {
            let group_type = self.translate_type(&group.ty);

            for item in &group.param_items {
                let mut tags = Vec::new();

                let param_type = match group.modifier
                    .as_ref()
                    .map(|decl| decl.param_mod)
                {
                    Some(ast::FunctionParamMod::Var) => {
                        group_type.temp_ref()
                    }

                    Some(ast::FunctionParamMod::Out) => {
                        if let Some(out_tag_info) = OutParamTagInfo::find_in_metadata(&self.metadata) {
                            tags.push(ir::TagInfo::new(out_tag_info.class_id));
                        }

                        group_type.temp_ref()
                    }

                    _ => {
                        group_type.clone()
                    },
                };

                param_infos.push(ir::FunctionParamInfo {
                    name: Some(item.name.clone()),
                    param_type,
                    tags,
                });
            }
        }

        param_infos
    }

    fn insert_func_decl(
        &mut self,
        identity: ir::FunctionIdentity,
        func_decl: &typ::ast::FunctionDecl,
    ) -> ir::FunctionID {
        let tags = self.translate_tag_groups(&func_decl.tags);

        let params = self.translate_param_groups(&func_decl.param_groups);
        let result_type = self.translate_type(&func_decl.result_ty);

        self.metadata.insert_func(identity, params, result_type, self.opts.rtti, tags)
    }

    fn method_identity(
        &mut self,
        func_decl: &typ::ast::FunctionDecl,
        declaring_type: &typ::Type,
        method_index: usize,
    ) -> ir::FunctionIdentity {
        // if the declaring type is a generic type, use its generic name in its identity
        // e.g. the declaring type is Box[T], not Box with unspecified type params
        // TODO: method translation should already use generic names here
        let declaring_type = match declaring_type {
            typ::Type::Variant(name) => typ::Type::variant(name.to_generic_name()),
            typ::Type::Class(name) => typ::Type::class(name.to_generic_name()),
            typ::Type::Record(name) => typ::Type::record(name.to_generic_name()),
            typ::Type::Interface(name) => typ::Type::interface(name.to_generic_name()),
            other => other.clone(),
        };

        let name = func_decl.name.ident.name.clone();
        let declaring_type = self.translate_type(&declaring_type);

        match func_decl.kind {
            ast::FunctionDeclKind::Destructor => {
                assert!(func_decl.name.type_params.is_none(), "method_identity: destructor method must not have type params");

                ir::FunctionIdentity::Destructor {
                    declaring_type,
                    id: ir::MethodID(method_index),
                    name
                }
            }

            _ => {
                let type_params = self.translate_type_param_list(func_decl.name.type_params.as_ref());

                ir::FunctionIdentity::Method {
                    declaring_type,
                    id: ir::MethodID(method_index),
                    name,
                    type_params,
                }
            }
        }
    }

    pub fn insert_function(&mut self, id: ir::FunctionID, function: ir::Function) {
        assert!(
            self.metadata.get_function_info(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.functions.insert(id, function);
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        func_sig: Arc<typ::FunctionSig>,
    ) -> FunctionInstance {
        let key = FunctionDeclKey::Function { name: func_name, sig: func_sig };

        self.instantiate_func(&key)
    }
}