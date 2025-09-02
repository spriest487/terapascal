mod rtti;
mod init;
mod dyn_array;
mod function;

use crate::ast::Access::Published;
use crate::ast::FunctionParamMod;
use crate::ast::IdentPath;
use crate::ast::MethodOwner;
use crate::ast::StructKind;
use crate::codegen::build_closure_function_def;
use crate::codegen::build_func_def;
use crate::codegen::build_func_static_closure_def;
use crate::codegen::build_static_closure_impl;
use crate::codegen::builder::Builder;
use crate::codegen::expr::expr_to_val;
use crate::codegen::library_builder::dyn_array::gen_dyn_array_funcs;
use crate::codegen::library_builder::dyn_array::gen_dyn_array_runtime_type;
use crate::codegen::library_builder::init::gen_tags_init;
use crate::codegen::library_builder::rtti::gen_release_func;
use crate::codegen::library_builder::rtti::gen_retain_func;
use crate::codegen::metadata::translate_closure_struct;
use crate::codegen::metadata::translate_iface;
use crate::codegen::metadata::translate_name;
use crate::codegen::metadata::translate_sig;
use crate::codegen::metadata::translate_struct_def;
use crate::codegen::metadata::translate_variant_def;
use crate::codegen::metadata::ClosureInstance;
use crate::codegen::metadata::NamePathExt;
use crate::codegen::stmt::translate_stmt;
use crate::codegen::typ;
use crate::codegen::CodegenOpts;
use crate::codegen::FunctionInstance;
use crate::codegen::SetFlagsType;
use crate::ir;
use crate::typ::ast::apply_func_decl_named_ty_args;
use crate::typ::ast::FunctionDeclContext;
use crate::typ::builtin_funcinfo_name;
use crate::typ::builtin_ident;
use crate::typ::builtin_methodinfo_name;
use crate::typ::builtin_string_name;
use crate::typ::builtin_typeinfo_name;
use crate::typ::free_mem_sig;
use crate::typ::get_mem_sig;
use crate::typ::layout::StructLayout;
use crate::typ::layout::StructLayoutMember;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::GenericContext;
use crate::typ::Specializable;
use crate::typ::TypeArgResolver;
use crate::typ::TypeArgsResult;
use crate::typ::TypeParamContainer;
use crate::typ::Value;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::ast::Ident;
pub use function::*;
use linked_hash_map::LinkedHashMap;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug)]
pub struct LibraryBuilder<'a> {
    src_metadata: &'a typ::Context,
    
    opts: CodegenOpts,
    
    type_cache: LinkedHashMap<typ::Type, ir::Type>,
    cached_types: LinkedHashMap<ir::Type, typ::Type>,

    // key is size (bits)
    set_flags_type_info: BTreeMap<usize, SetFlagsType>,

    translated_funcs: HashMap<FunctionDefKey, FunctionInstance>,
    
    tags: HashMap<ir::TagLocation, Vec<typ::ast::TagItem>>,

    function_types_by_sig: HashMap<typ::FunctionSig, ir::TypeDefID>,

    variables_by_name: HashMap<IdentPath, ir::VariableID>,
    next_variable_id: usize,

    // looked up on first use
    free_mem_func: Option<ir::FunctionID>,
    get_mem_func: Option<ir::FunctionID>,
    
    library: ir::Library,
}

thread_local! {
    pub static BUILTIN_CLASS_NAMES: [(typ::Symbol, ir::TypeDefID); 4] = [
        (builtin_string_name(), ir::STRING_ID),
        (builtin_typeinfo_name(), ir::TYPEINFO_ID),
        (builtin_methodinfo_name(), ir::METHODINFO_ID),
        (builtin_funcinfo_name(), ir::FUNCINFO_ID),
    ];
}

impl<'a> LibraryBuilder<'a> {
    pub fn new(src_metadata: &'a typ::Context, mut metadata: ir::Metadata, opts: CodegenOpts) -> Self {
        let builtin_classes = BUILTIN_CLASS_NAMES.with(|names| names.to_vec());

        for (_, builtin_class_id) in &builtin_classes {
            metadata.reserve_struct(*builtin_class_id);
        }
        
        let cached_types = builtin_classes
            .iter()
            .map(|(name, id)| (ir::Type::class_ptr(*id), typ::Type::class(name.clone())))
            .collect();

        let type_cache = builtin_classes
            .into_iter()
            .map(|(name, id)| (typ::Type::class(name), ir::Type::class_ptr(id)))
            .collect();
        
        let library = ir::Library::new(metadata);

        let builder = LibraryBuilder {
            library,

            opts,
            src_metadata,

            type_cache,
            cached_types,

            tags: HashMap::new(),
            
            set_flags_type_info: BTreeMap::new(),
            
            translated_funcs: HashMap::new(),
            
            function_types_by_sig: HashMap::new(),

            next_variable_id: 0,
            variables_by_name: HashMap::new(),
            
            // placeholders
            free_mem_func: None,
            get_mem_func: None,
        };

        builder
    }

    pub fn finish(mut self) -> ir::Library {
        // for all types defined in this module, ensure RTTI info is generated, even if they
        // were unused
        let mut defined_types: Vec<_> = self.src_metadata.defined_types();

        // exclude generic types
        let empty_generic_ctx = GenericContext::empty();

        defined_types.retain(|ty| !ty.is_unspecialized_generic()
            && !ty.contains_unresolved_params(&self.src_metadata));

        for defined_type in defined_types {
            self.translate_type(&defined_type, &empty_generic_ctx);
        }

        // add optional RTTI info like class and method names
        // can maybe disable this with a compile option to reduce startup time/build size
        self.populate_all_runtime_type_info();

        self.gen_static_closure_init();
        self.gen_static_type_init();

        // builtin classes are added manually to the type cache but their methods (and therefore 
        // any destructors) are expected to be defined in code, so we need to find those in the
        // source if they exist
        for (src_name, id) in BUILTIN_CLASS_NAMES.with(|names| names.to_vec()) {
            if let Ok(def) = self.src_metadata.instantiate_struct_def(&src_name, StructKind::Class) {
                let src_ty = typ::Type::class(src_name);
                self.insert_type_dtor(id, src_ty, def.as_ref());
            }
        }

        self.library.metadata.sort_type_defs_by_deps();
        self.library
    }
    
    pub fn opts(&self) -> &CodegenOpts {
        &self.opts
    }

    pub fn metadata(&self) -> &ir::Metadata {
        &self.library.metadata
    }    
    
    pub fn metadata_mut(&mut self) -> &mut ir::Metadata {
        &mut self.library.metadata
    }

    pub fn translate_unit(&mut self, unit: &typ::ast::Unit) {
        for (_, var) in unit.var_decl_items() {
            let id = ir::VariableID(self.next_variable_id);
            self.next_variable_id += 1;

            let var_ty = self.translate_type(&var.ty, &GenericContext::empty());

            for ident in &var.idents {
                let var_name = unit.ident.clone().child(ident.clone());

                self.variables_by_name.insert(var_name, id);
                self.library.variables.insert(id, var_ty.clone());
                if let Some(var_init) = &var.init {
                    let mut var_init_builder = Builder::new(self);

                    let expr = expr_to_val(&var_init.expr, &mut var_init_builder);
                    var_init_builder.mov(ir::GlobalRef::Variable(id), expr);

                    let init_body = &mut var_init_builder.finish();
                    self.library.init.append(init_body);
                }
            }
        }
        
        if let Some(init_block) = &unit.init {
            let mut init_builder = Builder::new(self);
            
            for stmt in &init_block.body {
                translate_stmt(stmt, &mut init_builder);
            }
            
            let unit_init = init_builder.finish();
            
            let debug_name = if self.opts.debug {
                Some(format!("{}.<init>", unit.ident))
            } else {
                None
            };

            let init_func = ir::FunctionDef {
                body: unit_init,
                sig: ir::FunctionSig {
                    param_tys: Vec::new(),
                    return_ty: ir::Type::Nothing
                },
                debug_name,
            };

            let init_func_id = self.library.metadata.insert_func(None);
            self.library.functions.insert(init_func_id, ir::Function::Local(init_func));

            self.library.init.push(ir::Instruction::Call {
                function: ir::Ref::Global(ir::GlobalRef::Function(init_func_id)).into(),
                args: Vec::new(),
                out: None,
            });
        }
    }

    pub(crate) fn instantiate_func(&mut self, key: &FunctionDefKey) -> FunctionInstance {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match &key.decl_key {
            FunctionDeclKey::Function { name, sig } => {
                match self.src_metadata.find_func_def(&name, sig.clone()).cloned() {
                    Some(typ::Def::Function(func_def)) => {
                        self.instantiate_func_def(&func_def, key.clone())
                    },

                    Some(typ::Def::External(extern_decl)) => {
                        self.instantiate_func_external(&extern_decl, key.clone())
                    },
                    
                    Some(other) => {
                        panic!("wrong kind of source def for function: {}", other.ident())
                    }

                    None => panic!("missing source def for function {}", name),
                }
            },

            FunctionDeclKey::Method(method_key) => {
                self.instantiate_method(method_key, key.type_args.as_ref())
            },

            FunctionDeclKey::VirtualMethod(virtual_key) => {
                self.instantiate_virtual_method(virtual_key)
            }
        }
    }

    pub fn get_method(&self, ty: &typ::Type, index: usize) -> typ::ast::MethodDecl {
        let method =  ty.get_method(index, &self.src_metadata);
        
        method.unwrap_or_else(|e| {
            panic!("get_method: failed to get method {index} of {ty}: {e}")
        })
    }
    
    pub fn find_method_index(&self, ty: &typ::Type, name: &Ident, sig: &typ::FunctionSig) -> usize {
        let (index, _) = ty
            .find_method(name, &sig, &self.src_metadata)
            .ok()
            .flatten()
            .unwrap_or_else(|| panic!("method {} of type {} must exist", name, ty));

        index
    }

    pub fn find_type_seq_support(&self, src_ty: &typ::Type) -> Option<TypeSequenceSupport> {
        TypeSequenceSupport::try_from_type(src_ty, &self.src_metadata).ok()
    }
    
    pub fn get_set_flags_type_info(&mut self, bits: usize) -> SetFlagsType {
        let existing = self.set_flags_type_info.get(&bits).cloned();
        if let Some(set_flags_ty) = existing {
            return set_flags_ty;
        }

        let set_flags_type = SetFlagsType::define_new(self, bits);
        self.set_flags_type_info.insert(bits, set_flags_type);

        self.gen_runtime_type(&ir::Type::Struct(set_flags_type.struct_id));

        set_flags_type
    }

    fn instantiate_system_func(&mut self, name: &str, sig: Arc<typ::FunctionSig>) -> ir::FunctionID {
        let ident_path = IdentPath::new(builtin_ident(name), [
            builtin_ident(SYSTEM_UNIT_NAME),
        ]);

        let instance = self.instantiate_func(&mut FunctionDefKey {
            decl_key: FunctionDeclKey::Function {
                name: ident_path,
                sig,
            },
            type_args: None,
        });
        instance.id
    }
    
    pub fn instantiate_get_mem_func(&mut self) -> ir::FunctionID {
        match self.get_mem_func {
            Some(id) => id,
            None => {
                let id = self.instantiate_system_func("GetMem", Arc::new(get_mem_sig()));
                self.get_mem_func = Some(id);
                id
            }
        }
    }

    pub fn instantiate_free_mem_func(&mut self) -> ir::FunctionID {
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
        key: FunctionDefKey,
    ) -> FunctionInstance {
        // eprintln!("instantiate_func_def: {}", func_def.decl.name);
        
        let generic_ctx = match key.type_args.as_ref() {
            Some(type_args) => {
                let type_params = func_def.decl.name.type_params
                    .as_ref()
                    .expect("instantiate_func_def: function referenced with type args must have type params");

                typ::GenericContext::new(type_params, type_args)
            }
            None => {
                typ::GenericContext::empty()
            }
        };
        
        let specialized_decl = apply_func_decl_named_ty_args((*func_def.decl).clone(), &generic_ctx, &generic_ctx);

        let sig = specialized_decl.sig();
        let ns = key.decl_key.namespace().into_owned();

        let id = self.declare_func(&specialized_decl, ns);

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Arc::new(sig),
        };

        let debug_name = if self.opts.debug {
            Some(specialized_decl.name.to_debug_string(key.type_args.as_ref()))
        } else {
            None
        };

        self.translated_funcs.insert(key, cached_func.clone());

        let ir_func = build_func_def(
            self,
            generic_ctx,
            &specialized_decl.param_groups,
            &specialized_decl.result_ty,
            &func_def.locals,
            &func_def.body,
            false,
            debug_name,
        );

        self.library.functions.insert(id, ir::Function::Local(ir_func));

        let tags_loc = ir::TagLocation::Function(id);
        self.add_tags(tags_loc, &func_def.decl.tags);

        cached_func
    }

    fn instantiate_func_external(
        &mut self,
        extern_decl: &typ::ast::FunctionDecl,
        key: FunctionDefKey,
    ) -> FunctionInstance {
        assert!(
            key.type_args.is_none(),
            "external function must not be generic"
        );

        let generic_ctx = typ::GenericContext::empty();

        let decl_namespace = key.decl_key.namespace().into_owned();
        let id = self.declare_func(&extern_decl, decl_namespace);

        let sig = extern_decl.sig();
        let return_ty = self.translate_type(&sig.result_ty, &generic_ctx);

        let param_tys = sig.params
            .iter()
            .map(|sig_param| {
                let param_ty = self.translate_type(&sig_param.ty, &generic_ctx);
                match sig_param.modifier {
                    None => param_ty,
                    Some(FunctionParamMod::Var | FunctionParamMod::Out) => param_ty.ptr(),
                }
            })
            .collect();

        let cached_func = FunctionInstance {
            id,
            sig: Arc::new(sig),
        };

        let extern_src = extern_decl
            .external_src()
            .expect("function with external def must have an extern src");

        self.library.functions.insert(
            id,
            ir::Function::External(ir::ExternalFunctionRef {
                src: extern_src.value.clone(),
                symbol: (*extern_decl.name.ident.name).clone(),

                sig: ir::FunctionSig { return_ty, param_tys },
            }),
        );

        self.translated_funcs.insert(key, cached_func.clone());

        cached_func
    }

    fn instantiate_method(
        &mut self,
        method_key: &MethodDeclKey,
        type_args: Option<&typ::TypeArgList>,
    ) -> FunctionInstance {
        // eprintln!("instantiate_method: {:#?}", method_key);

        let mut generic_ctx = typ::GenericContext::empty();
        
        // if the self type is a parameterized generic, we'll need to instantiate the specialized
        // def here, since only the original declared version will be in the definition map
        let decl_self_ty = match &method_key.self_ty {
            typ::Type::Class(sym) if sym.type_args.is_some() => {
                let type_params = sym.type_params.as_ref().unwrap();
                let type_args = sym.type_args.as_ref().unwrap();
    
                generic_ctx.push(type_params, type_args);
                typ::Type::class(sym.as_ref().clone().with_ty_args(None))
            }

            typ::Type::Record(sym) if sym.type_args.is_some() => {
                let type_params = sym.type_params.as_ref().unwrap();
                let type_args = sym.type_args.as_ref().unwrap();
    
                generic_ctx.push(type_params, type_args);
                typ::Type::record(sym.as_ref().clone().with_ty_args(None))
            }

            typ::Type::Variant(sym) if sym.type_args.is_some() => {
                let type_params = sym.type_params.as_ref().unwrap();
                let type_args = sym.type_args.as_ref().unwrap();
    
                generic_ctx.push(type_params, type_args);
                typ::Type::variant(sym.as_ref().clone().with_ty_args(None))
            }

            // nothing to do if the type isn't parameterized
            _ => {
                assert_eq!(
                    TypeArgsResult::NotGeneric,
                    method_key.self_ty.type_args(),
                    "expected non-generic self type for instantiation of {} method {} without type args",
                    method_key.self_ty,
                    method_key.method_index
                );

                method_key.self_ty.clone()
            }
        };

        let method_decl = decl_self_ty
            .get_method(method_key.method_index, &self.src_metadata)
            .unwrap_or_else(|e| {
                panic!("instantiate_method: failed to get method metadata for {} method {}: {}", decl_self_ty, method_key.method_index, e)
            });
        
        let method_sig = Arc::new(method_decl.func_decl.sig());

        let generic_method_def = self
            .src_metadata
            .find_method(
                &decl_self_ty,
                &method_decl.func_decl.ident(),
                &method_sig,
            )
            .cloned()
            .unwrap_or_else(|| {
                panic!("instantiate_method: missing method def: {} method {}", decl_self_ty, method_key.method_index)
            });
        let generic_method_decl = generic_method_def.decl.as_ref();

        // the definition we found should already be correctly specialized - you can't pass
        // type args when calling an interface method, so the only thing that would change the method
        // being generated here is the self-type, which we already specialized
        if let Some(ty_args) = type_args {
            let decl_ty_params = generic_method_decl.name.type_params
                .as_ref()
                .expect("instantiate_method: method called with type args must have type params");

            generic_ctx.push(decl_ty_params, ty_args);
        };

        let ns = method_key
            .self_ty
            .full_path()
            .expect("instantiate_method: methods should only be generated for named types")
            .into_owned();

        // TODO: refactor - this is a duplicate of specialize_method_decl
        let mut specialized_decl = apply_func_decl_named_ty_args(
            generic_method_decl.clone(),
            &generic_ctx,
            &generic_ctx);

        specialized_decl.name.context = FunctionDeclContext::MethodDecl {
            enclosing_type: method_key.self_ty.clone(), 
        };

        let id = self.declare_func(&specialized_decl, ns);

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let cached_func = FunctionInstance {
            id,
            sig: Arc::new(specialized_decl.sig()),
        };

        let key = FunctionDefKey {
            decl_key: FunctionDeclKey::Method(method_key.clone()),
            type_args: type_args.cloned(),
        };

        let debug_name = if self.opts.debug {
            Some(specialized_decl.name.to_debug_string(key.type_args.as_ref()))
        } else {
            None
        };
        
        self.translated_funcs.insert(key, cached_func.clone());

        let ir_func = build_func_def(
            self,
            generic_ctx,
            &generic_method_def.decl.param_groups,
            &generic_method_def.decl.result_ty,
            &generic_method_def.locals,
            &generic_method_def.body,
            true,
            debug_name,
        );
        self.library.functions.insert(id, ir::Function::Local(ir_func));

        if !method_decl.func_decl.tags.is_empty() {
            let ir_self_ty = self.translate_type(&decl_self_ty, &GenericContext::empty());
            
            let Some(tags_loc) = ir_self_ty
                .tags_loc()
                .and_then(|ty_tags_loc| ty_tags_loc.method_loc(method_key.method_index)) 
            else {
                panic!("produced tags for method {} with no valid tag location!", method_decl.func_decl.name)
            };

            self.add_tags(tags_loc, &method_decl.func_decl.tags);
        }

        cached_func
    }

    fn instantiate_virtual_method(&mut self, virtual_key: &VirtualMethodKey) -> FunctionInstance {
        let impl_method = &virtual_key.impl_method;

        // virtual methods can't be generic
        let generic_ctx = typ::GenericContext::empty();
        let self_ty = self.translate_type(&impl_method.self_ty, &generic_ctx);

        // virtual calls can't have type args yet
        let type_args = None;
        let impl_func = self.instantiate_method(&impl_method, type_args);

        let iface_ty_name = virtual_key
            .iface_ty
            .full_name()
            .expect("interface type must not be unnamed");

        let iface_method_decl = virtual_key.iface_ty
            .get_method(virtual_key.iface_method_index, &self.src_metadata)
            .unwrap_or_else(|e| {
                panic!("instantiate_virtual_method: failed to get {} method {}: {}", impl_method.self_ty, impl_method.method_index, e)
            });

        let method_name = iface_method_decl.func_decl.ident().to_string();

        let iface_id = self
            .find_iface_decl(&iface_ty_name)
            .unwrap_or_else(|| {
                let src_iface_def = self
                    .src_metadata
                    .instantiate_iface_def(&iface_ty_name)
                    .unwrap_or_else(|_err| panic!(
                        "instantiate_virtual_method: failed to get interface def {} referenced in metadata",
                        iface_ty_name,
                    ));

                let mut builder = Builder::new(self);
                let iface_meta = builder.translate_iface(&src_iface_def);
                self.library.metadata.define_iface(iface_meta)
            });

        self.library.metadata.impl_method(iface_id, self_ty, method_name, impl_func.id);

        let key = FunctionDefKey {
            decl_key: FunctionDeclKey::VirtualMethod(virtual_key.clone()),
            type_args: None,
        };

        self.translated_funcs.insert(key.clone(), impl_func.clone());

        impl_func
    }

    // statically reference a method and get a function ID. interface methods are all translated
    // at the end of compilation for a module anyway, but for methods that are referenced statically
    // this call reserves us a function ID
    pub fn translate_method(
        &mut self,
        self_ty: typ::Type, 
        self_ty_method_index: usize,
        type_args: Option<typ::TypeArgList>,
    ) -> FunctionInstance {
        let mut key = FunctionDefKey {
            decl_key: FunctionDeclKey::Method(MethodDeclKey {
                method_index: self_ty_method_index,
                self_ty,
            }),

            type_args: type_args.clone(),
        };

        // methods must always be present so make sure they're immediately instantiated
        self.instantiate_func(&mut key)
    }

    pub fn declare_func(
        &mut self,
        func_decl: &typ::ast::FunctionDecl,
        namespace: IdentPath,
    ) -> ir::FunctionID {
        let has_global_name = func_decl.name.context == FunctionDeclContext::FreeFunction
            && !func_decl.is_overload()
            && func_decl.name.type_params.is_none();

        let global_name = if has_global_name {
            let ns: Vec<_> = namespace
                .iter()
                .map(|part| part.to_string())
                .collect();
            let name = func_decl.name.ident.to_string();

            Some(ir::NamePath::new(ns, name))
        } else {
            None
        };

        self.library.metadata.insert_func(global_name)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        func_sig: Arc<typ::FunctionSig>,
        type_args: Option<typ::TypeArgList>,
    ) -> FunctionInstance {
        let mut key = FunctionDefKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name, sig: func_sig },
        };

        self.instantiate_func(&mut key)
    }

    pub fn insert_func(&mut self, id: ir::FunctionID, function: ir::Function) {
        assert!(
            self.library.metadata.get_function(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.library.functions.insert(id, function);
    }

    pub fn find_iface_decl(&mut self, iface_name: &typ::Symbol) -> Option<ir::InterfaceID> {
        let name = translate_name(iface_name, &GenericContext::empty(), self);

        self.library.metadata
            .ifaces()
            .find_map(|(id, decl)| {
                if decl.name == name {
                    Some(id)
                } else {
                    None
                }
            })
    }

    // interface methods may not be statically referenced for every type that implements them due to
    // dynamic dispatch, so we need to cover all possible combinations and generate function bodies for
    // every interface method implemented by a class at the end of codegen
    fn gen_iface_impls(&mut self, self_ty: &typ::Type) {
        let ifaces = self_ty
            .implemented_ifaces(&self.src_metadata)
            .unwrap_or_else(|err| {
                panic!("failed to retrieve implementation list for type {}: {}", self_ty, err)
            });

        for iface_ty in &ifaces {
            let iface_methods: Vec<_> = iface_ty
                .methods(&self.src_metadata)
                .unwrap()
                .into_iter()
                .collect();

            for (iface_method_index, iface_method) in iface_methods
                .into_iter()
                .enumerate()
            {
                let method_name = iface_method.func_decl.ident();

                let impl_sig = iface_method.func_decl.sig().with_self(&self_ty);
                let impl_index = self.find_method_index(&self_ty, &method_name, &impl_sig);

                let virtual_key = VirtualMethodKey {
                    iface_ty: iface_ty.clone(),
                    iface_method_index,

                    impl_method: MethodDeclKey {
                        self_ty: self_ty.clone(),
                        method_index: impl_index,
                    },
                };

                self.instantiate_func(&mut FunctionDefKey {
                    decl_key: FunctionDeclKey::VirtualMethod(virtual_key),
                    type_args: None,
                });
            }
        }
    }

    pub fn find_type(&mut self, src_ty: &typ::Type) -> ir::Type {
        match src_ty {
            typ::Type::Nothing => ir::Type::Nothing,
            typ::Type::Nil => ir::Type::Nothing.ptr(),

            typ::Type::Primitive(typ::Primitive::Boolean) => ir::Type::Bool,

            typ::Type::Primitive(typ::Primitive::Int8) => ir::Type::I8,
            typ::Type::Primitive(typ::Primitive::UInt8) => ir::Type::U8,
            typ::Type::Primitive(typ::Primitive::Int16) => ir::Type::I16,
            typ::Type::Primitive(typ::Primitive::UInt16) => ir::Type::U16,
            typ::Type::Primitive(typ::Primitive::Int32) => ir::Type::I32,
            typ::Type::Primitive(typ::Primitive::UInt32) => ir::Type::U32,
            typ::Type::Primitive(typ::Primitive::Int64) => ir::Type::I64,
            typ::Type::Primitive(typ::Primitive::UInt64) => ir::Type::U64,
            typ::Type::Primitive(typ::Primitive::NativeInt) => ir::Type::ISize,
            typ::Type::Primitive(typ::Primitive::NativeUInt) => ir::Type::USize,

            typ::Type::Primitive(typ::Primitive::Real32) => ir::Type::F32,

            typ::Type::Primitive(typ::Primitive::Pointer) => ir::Type::Nothing.ptr(),

            typ::Type::Weak(weak_ty) => self.find_type(weak_ty), 

            typ::Type::Pointer(target) => self.find_type(target).ptr(),

            typ::Type::Record(class) | typ::Type::Class(class) => {
                expect_no_unspec_args(&class, class.type_args.as_ref());

                let ty_name = ir::NamePath::from_decl(class.as_ref().clone(), self);
                let struct_id = match self.metadata().find_type_decl(&ty_name) {
                    Some(id) => id,
                    None => panic!("{} was not found in metadata (not instantiated)", class),
                };

                match src_ty {
                    typ::Type::Class(..) => {
                        let class_id = ir::VirtualTypeID::Class(struct_id);
                        ir::Type::RcPointer(class_id)
                    },

                    typ::Type::Record(..) => ir::Type::Struct(struct_id),

                    _ => unreachable!(),
                }
            },

            typ::Type::Interface(iface) => {
                expect_no_unspec_args(&iface, iface.type_args.as_ref());

                let iface_id = match self.find_iface_decl(&iface) {
                    Some(id) => id,
                    None => panic!("missing IR definition for interface {}", iface),
                };

                ir::Type::RcPointer(ir::VirtualTypeID::Interface(iface_id))
            },

            typ::Type::Array(array_ty) => {
                let element = self.find_type(&array_ty.element_ty);
                ir::Type::Array {
                    element: Rc::new(element),
                    dim: array_ty.dim,
                }
            },

            typ::Type::DynArray { element } => {
                let element = self.find_type(element.as_ref());

                let array_struct = match self.metadata().find_dyn_array_struct(&element) {
                    Some(id) => id,
                    None => panic!(
                        "missing dyn array IR struct definition for element type {}",
                        element
                    ),
                };

                ir::Type::RcPointer(ir::VirtualTypeID::Class(array_struct))
            },

            typ::Type::Variant(variant) => {
                expect_no_unspec_args(&variant, variant.type_args.as_ref());

                let ty_name = ir::NamePath::from_decl(variant.as_ref().clone(), self);

                match self.metadata().find_type_decl(&ty_name) {
                    Some(id) => ir::Type::Variant(id),
                    None => panic!("missing IR struct metadata for variant {}", variant),
                }
            },

            typ::Type::MethodSelf => panic!("Self is not a real type in this context"),

            typ::Type::GenericParam(param) => panic!(
                "{} is not a real type in this context",
                param,
            ),

            typ::Type::Function(sig) => match self.find_func_ty(sig) {
                Some(id) => ir::Type::Function(id),
                None => panic!("no type definition for function with sig {}", sig),
            },

            // TODO: enums may later be variably sized
            typ::Type::Enum(..) => ir::Type::ISize,
            
            // TODO: variable sized sets
            // we could decide to use a smaller or larger set flags type here depending on the range
            // of values, but for now all sets use the widest representation (256-bit)
            typ::Type::Set(set_ty) => {
                let name = set_ty.name
                    .as_ref()
                    .map(|ident_path| ir::NamePath::from_ident_path(ident_path, None))
                    .unwrap_or_else(|| {
                        panic!("can't find existing definition of unnamed set type for {}", src_ty)
                    });
                
                let Some((set_id, set_ty)) = self.library.metadata.find_set_def(&name) else {
                    panic!("flags type for {} is not defined", src_ty);
                };

                ir::Type::Flags(set_ty.flags_struct, set_id)
            },

            typ::Type::Any => ir::Type::RcPointer(ir::VirtualTypeID::Any),
        }
    }
    
    pub fn apply_ty_args<Generic>(&self,
        target: Generic,
        params: &impl TypeParamContainer,
        args: &impl TypeArgResolver
    ) -> Generic 
    where 
        Generic: Specializable,
    {
        target.apply_type_args(params, args)
    }
    
    fn insert_type_dtor(&mut self,
        type_id: ir::TypeDefID,
        src_ty: typ::Type,
        src_def: &impl MethodOwner<Value>
    ) {
        let Some(dtor_method_index) = src_def.find_dtor_index() else {
            return;
        };

        let dtor = self.instantiate_method(&MethodDeclKey {
            self_ty: src_ty,
            method_index: dtor_method_index,
        }, None);

        self.metadata_mut().insert_dtor(type_id, dtor.id);
    }

    pub fn translate_type(
        &mut self,
        src_ty: &typ::Type,
        generic_ctx: &GenericContext,
    ) -> ir::Type {
        let src_ty = generic_ctx.apply_to_type(src_ty.clone());
        if let Some(cached) = self.type_cache.get(&src_ty) {
            let ty = cached.clone();
            return ty;
        }

        // instantiate types which may contain generic params
        let ty = match &src_ty {
            typ::Type::Variant(variant) => {
                let variant_def = self.src_metadata.instantiate_variant_def(variant).unwrap();

                let id = self.library.metadata.reserve_new_struct();
                let ty = ir::Type::Variant(id);
                
                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty.clone());

                let name_path = translate_name(&variant, generic_ctx, self);
                self.library.metadata.declare_struct(id, &name_path, false);

                let variant_meta = translate_variant_def(&variant_def, generic_ctx, self);
                self.library.metadata.define_variant(id, variant_meta);

                self.insert_type_dtor(id, src_ty, variant_def.as_ref());

                self.add_tags(ir::TagLocation::TypeDef(id), &variant_def.tags);
                
                ty
            },

            typ::Type::Record(name) | typ::Type::Class(name) => {
                let kind = src_ty.struct_kind().unwrap();
                let def = self.src_metadata.instantiate_struct_def(name, kind).unwrap();

                let id = self.library.metadata.reserve_new_struct();
                let ty = match def.kind {
                    StructKind::Class => {
                        ir::Type::RcPointer(ir::VirtualTypeID::Class(id))
                    },
                    StructKind::Record => {
                        ir::Type::Struct(id)
                    },
                };

                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty.clone());

                let name_path = translate_name(&name, generic_ctx, self);
                self.library.metadata.declare_struct(id, &name_path, kind == StructKind::Class);

                let struct_meta = translate_struct_def(&def, generic_ctx, self);
                self.library.metadata.define_struct(id, struct_meta);

                self.insert_type_dtor(id, src_ty, def.as_ref());

                self.add_tags(ir::TagLocation::TypeDef(id), &def.tags);

                ty
            },
            
            typ::Type::Weak(weak_ty) => {
                match self.translate_type(weak_ty, generic_ctx) {
                    ir::Type::RcPointer(id) => ir::Type::RcWeakPointer(id),
                    other => unreachable!("only RC class types can be weak, found: {}", other),
                }
            }

            typ::Type::Interface(iface_sym) => {
                let iface_def = self.src_metadata.instantiate_iface_def(iface_sym).unwrap();

                let iface_name = translate_name(&iface_def.name, generic_ctx, self);
                let id = self.library.metadata.declare_iface(&iface_name);
                let ty = ir::Type::RcPointer(ir::VirtualTypeID::Interface(id));

                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty);

                let iface_meta = translate_iface(&iface_def, generic_ctx, self);
                let def_id = self.library.metadata.define_iface(iface_meta);
                assert_eq!(def_id, id);

                self.add_tags(ir::TagLocation::Interface(id), &iface_def.tags);

                ty
            },

            typ::Type::Array(array_ty) => {
                let elem_ty = self.translate_type(&array_ty.element_ty, generic_ctx);
                let ty = ir::Type::Array {
                    element: Rc::new(elem_ty),
                    dim: array_ty.dim,
                };

                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty);
                ty
            },

            typ::Type::DynArray { element } => {
                let id = self.translate_dyn_array_struct(&element, generic_ctx);

                let ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(id));

                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty);

                ty
            },

            typ::Type::Function(func_sig) => {
                if let Some(id) = self.find_func_ty(&func_sig) {
                    return ir::Type::Function(id);
                }

                let ir_sig = translate_sig(&func_sig, generic_ctx, self);
                let func_ty_id = self.define_func_ty((**func_sig).clone(), ir_sig);

                let ty = ir::Type::RcPointer(ir::VirtualTypeID::Closure(func_ty_id));

                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty);

                ty
            },
            
            typ::Type::Set(set_ty) => {
                let flags_ty = self.get_set_flags_type_info(set_ty.flags_type_bits());
                let name = set_ty.name
                    .as_ref()
                    .map(|ident_path| ir::NamePath::from_ident_path(ident_path, None));
    
                let set_id = self.metadata_mut()
                    .define_set_type(name, flags_ty.struct_id);
                let ty = ir::Type::Flags(flags_ty.struct_id, set_id);

                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty);
                
                ty
            }

            real_ty => {
                // nothing to be instantiated (must be a trivial/primitive type or this will panic)
                let ty = self.find_type(real_ty);
                
                self.type_cache.insert(src_ty.clone(), ty.clone());
                self.cached_types.insert(ty.clone(), src_ty);

                ty
            },
        };

        // ensure the runtime type info exists for all referenced types
        if self.metadata().is_defined(&ty) {
            self.gen_runtime_type(&ty);
        }

        ty
    }

    pub fn find_func_ty(&self, sig: &typ::FunctionSig) -> Option<ir::TypeDefID> {
        self.function_types_by_sig.get(&sig).cloned()
    }

    pub fn define_func_ty(
        &mut self,
        sig: typ::FunctionSig,
        func_ty: ir::FunctionSig,
    ) -> ir::TypeDefID {
        assert!(!self.function_types_by_sig.contains_key(&sig));

        let decl = ir::TypeDecl::Def(ir::TypeDef::Function(func_ty));

        let id = self.metadata_mut().insert_type_decl(decl);

        self.function_types_by_sig.insert(sig, id);

        id
    }

    pub fn find_global_var(&self, name_path: &IdentPath) -> Option<ir::VariableID> {
        self.variables_by_name.get(name_path).cloned()
    }

    // get or generate runtime type for a given type, which contains the function IDs etc
    // used for RC operations at runtime. the rest of the RTTI info will be filled in later 
    // in a separate pass
    pub fn gen_runtime_type(&mut self, ty: &ir::Type) -> Rc<ir::RuntimeType> {
        if let Some(existing) = self.library.metadata.get_runtime_type(&ty) {
            return existing;
        }

        assert!(self.metadata().is_defined(ty), "gen_runtime_type: type {} ({:?}) is not defined yet", self.metadata().pretty_ty_name(ty), ty);

        // type names and methods will be added after codegen
        let mut rtti = ir::RuntimeType::new(None);
        rtti.retain = gen_retain_func(self, ty);
        rtti.release = gen_release_func(self, ty);
        
        if self.opts.debug {
            rtti.debug_name = Some(self.metadata().pretty_ty_name(&ty).into_owned());
        }

        self.library.metadata.insert_runtime_type(ty.clone(), rtti)
    }
    
    // gen_runtime_type only populates the basics needed for codegen, this fills in reflection
    // values like type names and methods
    fn populate_all_runtime_type_info(&mut self) {
        // TODO: this might be overly defensive, maybe the type cache will never change here?
        // clone the type cache for iteration
        // the RTTI generation process can touch new parts of code and
        // cause the type cache to expand, so repeat until it stops growing
        let mut done_types = 0;
        let mut done_closures = 0;
        let mut done_dynarrays = 0;

        let mut populate_types = Vec::new();
        let mut populate_closures = Vec::new();
        let mut populate_dynarrays = Vec::new();

        loop {
            populate_types.extend(self.type_cache
                .iter()
                .skip(done_types)
                .map(|(src_ty, ty)| (src_ty.clone(), ty.clone())));

            populate_dynarrays.extend(self
                .metadata()
                .dyn_array_structs()
                .iter()
                .skip(done_dynarrays.clone())
                .map(|(elem_ty, struct_id)| (elem_ty.clone(), struct_id.clone())));
            
            populate_closures.extend(self.library.closure_types().skip(done_closures));

            for closure_id in populate_closures.drain(0..) {
                self.gen_runtime_type(&ir::Type::Struct(closure_id));
                self.gen_runtime_type(&ir::Type::RcPointer(ir::VirtualTypeID::Closure(closure_id)));
                self.gen_runtime_type(&ir::Type::RcWeakPointer(ir::VirtualTypeID::Closure(closure_id)));
                done_closures += 1;
            }

            for (elem_ty, struct_id) in populate_dynarrays.drain(0..) {
                gen_dyn_array_runtime_type(self, &elem_ty, struct_id);
                gen_dyn_array_funcs(self, &elem_ty, struct_id);

                done_dynarrays += 1;
            }

            for (src_ty, ty) in populate_types.drain(0..) {
                if src_ty.as_class().is_ok() {
                    gen_class_runtime_type(self, &ty);
                }
                
                if src_ty.as_iface().is_some() {
                    gen_iface_virtual_method_info(self, &src_ty);
                }

                self.gen_iface_impls(&src_ty);

                self.populate_runtime_type_info(src_ty, ty);
                done_types += 1;
            }

            if done_types == self.type_cache.len() {
                break;
            }
        }
    }

    fn populate_runtime_type_info(&mut self, src_ty: typ::Type, ty: ir::Type) {
        // should fetch from the cache at this point
        let mut rtti = (*self.gen_runtime_type(&ty)).clone();
        rtti.name = Some(self.library.metadata.find_or_insert_string(&src_ty.to_string()));

        match &src_ty {
            typ::Type::Record(name) | typ::Type::Class(name) => {
                let def = self.src_metadata
                    .instantiate_struct_def(name.as_ref(), src_ty.struct_kind().unwrap())
                    .unwrap();

                for (method_index, method) in def.methods().enumerate() {
                    if method.access >= Published && method.func_decl.name.type_params.is_none() {
                        rtti.methods.push(self.create_runtime_method(ty.clone(), &src_ty, method_index, false, &method.func_decl));
                    }
                }
            }
            
            typ::Type::Interface(name) => {
                let def = self.src_metadata
                    .instantiate_iface_def(name.as_ref())
                    .unwrap();

                for (method_index, method) in def.methods.iter().enumerate() {
                    if method.decl.name.type_params.is_none() {
                        rtti.methods.push(self.create_runtime_method(ty.clone(), &src_ty, method_index, true, &method.decl));
                    }
                }
            }

            _ => {
                // type has no publishable methods
            }
        };

        // replace the existing RTTI
        self.library.metadata.insert_runtime_type(ty, rtti);
    }

    fn create_runtime_method(&mut self,
        instance_ty: ir::Type,
        src_ty: &typ::Type,
        method_index: usize,
        is_abstract: bool,
        decl: &typ::ast::FunctionDecl,
    ) -> ir::RuntimeMethod {
        let method_name = decl.name.ident.as_str();
        let method_name_id = self.library.metadata.find_or_insert_string(method_name);

        let generic_ctx = GenericContext::empty();

        let params = decl
            .params()
            .map(|(param, _)| {
                // in RTTI, "self" params for interfaces become untyped pointers
                let mut param_ty = match param.ty.ty() {
                    typ::Type::MethodSelf => ir::Type::Nothing.ptr(),
                    ty => self.translate_type(ty, &generic_ctx)
                };

                if matches!(param.get_modifier(), Some(FunctionParamMod::Var | FunctionParamMod::Out)) {
                    param_ty = param_ty.ptr();
                }

                param_ty
            })
            .collect();
        
        let function = if is_abstract {
            None
        } else {
            let method_key = MethodDeclKey {
                self_ty: src_ty.clone(),
                method_index,
            };
            let method_instance = self.instantiate_method(&method_key, None);
            Some(method_instance.id)
        };

        ir::RuntimeMethod {
            function,
            instance_ty,
            name: method_name_id,
            params,
            result_ty: self.translate_type(&decl.result_ty, &generic_ctx),
        }
    }

    pub fn translate_dyn_array_struct(
        &mut self,
        src_element_ty: &typ::Type,
        generic_ctx: &typ::GenericContext,
    ) -> ir::TypeDefID {
        let element_ty = self.translate_type(src_element_ty, generic_ctx);

        match self.library.metadata.find_dyn_array_struct(&element_ty) {
            Some(id) => id,

            None => {
                self.library.metadata.define_dyn_array_struct(element_ty)
            }
        }
    }

    pub fn gen_bounds_check(
        &mut self,
        element_ty: &ir::Type,
    ) -> ir::FunctionID {
        let func_id = match self.metadata().get_bounds_check_func(element_ty) {
            Some(existing_id) => return existing_id,
            None => {
                self.metadata_mut().insert_func(None)
            }
        };

        let mut builder = Builder::new(self);
        builder.bind_param(ir::LocalID(0), ir::Type::I32, "len", false);
        builder.bind_param(ir::LocalID(1), ir::Type::I32, "index", false);
        
        let len_val = ir::Value::from(ir::Ref::Local(ir::LocalID(0)));
        let index_val = ir::Value::from(ir::Ref::Local(ir::LocalID(1)));

        builder.comment(&format!("bounds check for index={}, len={}", index_val, len_val));

        let bounds_ok_label = builder.alloc_label();

        // if index >= 0 and index < arr.len then goto "bounds_ok"
        let gte_zero = builder.gte_to_val(index_val.clone(), ir::Value::LiteralI32(0));
        let lt_len = builder.lt_to_val(index_val, len_val);
        let bounds_check_ok = builder.and_to_val(gte_zero, lt_len);
        builder.append(ir::Instruction::JumpIf {
            dest: bounds_ok_label,
            test: bounds_check_ok,
        });

        // otherwise: raise
        let err_str = builder.find_or_insert_string("array index out of bounds");
        builder.append(ir::Instruction::Raise {
            val: ir::Ref::Global(ir::GlobalRef::StringLiteral(err_str)),
        });

        builder.append(ir::Instruction::Label(bounds_ok_label));

        let body = builder.finish();

        let debug_name = if self.opts.debug {
            let element_name = self.metadata().pretty_ty_name(element_ty);
            Some(format!("bounds check for {element_name}"))
        } else {
            None
        };

        let func_def = ir::FunctionDef {
            body,
            sig: ir::FunctionSig {
                return_ty: ir::Type::Nothing,
                param_tys: vec![
                    ir::Type::I32,
                    ir::Type::I32,
                ],
            },
            debug_name,
        };

        self.insert_func(func_id, ir::Function::Local(func_def));

        func_id
    }

    pub fn aligned_struct_members<'s>(&self, struct_def: &'s typ::ast::StructDecl) -> Vec<StructLayoutMember<'s>> {
        let layout = if struct_def.packed {
            StructLayout::Packed
        } else {
            StructLayout::Auto
        };

        layout.members_of(&struct_def, &self.src_metadata).unwrap()
    }

    pub fn translate_func_ty(
        &mut self,
        func_sig: &typ::FunctionSig,
        generic_ctx: &typ::GenericContext,
    ) -> ir::TypeDefID {
        let func_ty_id = match self.find_func_ty(&func_sig) {
            Some(id) => id,
            None => {
                let ir_sig = translate_sig(func_sig, generic_ctx, self);
                self.define_func_ty(func_sig.clone(), ir_sig)
            },
        };

        func_ty_id
    }

    pub fn build_closure_instance(
        &mut self,
        func: &typ::ast::AnonymousFunctionDef,
        generic_ctx: &typ::GenericContext,
    ) -> ClosureInstance {
        let id = self.library.metadata.insert_func(None);

        // this is the signature of the *function type* of the closure, not the signature of
        // the real method implementing the closure, which has an extra type-erased parameter
        // for the closure itself
        let func_ty_sig = typ::FunctionSig::of_anonymous_func(func);

        let func_ty_id = self.translate_func_ty(&func_ty_sig, generic_ctx);

        let closure_identity = ir::ClosureIdentity {
            virt_func_ty: func_ty_id,
            id,
        };

        let closure_id = translate_closure_struct(
            closure_identity,
            &func.captures,
            generic_ctx,
            self
        );

        let debug_name = if self.opts.debug {
            Some("<anonymous function>".to_string())
        } else {
            None
        };

        let cached_func = FunctionInstance { id, sig: func_ty_sig };

        let ir_func = build_closure_function_def(self, &func, closure_id, debug_name);

        self.library.functions.insert(id, ir::Function::Local(ir_func));

        ClosureInstance {
            func_instance: cached_func,
            func_ty_id,
            closure_id,
        }
    }

    pub fn build_func_static_closure_instance(&mut self,
        func: &FunctionInstance,
        generic_ctx: &GenericContext
    ) -> &ir::StaticClosure {
        if let Some(existing) = self.library.metadata.get_static_closure(func.id) {
            return &self.library.static_closures[existing.0];
        }

        // function reference closures can never have a capture list or type args
        let captures = LinkedHashMap::default();

        let func_ty_id = self.translate_func_ty(func.sig.as_ref(), generic_ctx);

        let ir_func = self.library.functions
            .get(&func.id)
            .expect("function passed to build_function_closure_instance must have been previously translated")
            .clone();

        let closure_identity = ir::ClosureIdentity {
            virt_func_ty: func_ty_id,
            id: func.id,
        };

        let closure_id = translate_closure_struct(
            closure_identity,
            &captures,
            generic_ctx,
            self
        );

        // build the closure function, which is a thunk that just calls the global function
        let thunk_id = self.library.metadata.insert_func(None);
        let thunk_def = build_func_static_closure_def(self, func, &ir_func);

        self.library.functions.insert(thunk_id, ir::Function::Local(thunk_def));

        let closure = ClosureInstance {
            closure_id,
            func_ty_id,
            func_instance: FunctionInstance {
                id: thunk_id,
                sig: func.sig.clone(),
            },
        };

        let static_closure_id = self.build_static_closure_instance(closure).id;
        self.library.metadata.insert_static_closure(func.id, static_closure_id);

        &self.library.static_closures[static_closure_id.0]
    }

    pub fn build_static_closure_instance(&mut self, closure: ClosureInstance) -> &ir::StaticClosure {
        let existing_index = self.library.static_closures.iter()
            .enumerate()
            .filter_map(|(i, static_closure)| {
                if static_closure.closure_id == closure.closure_id {
                    Some(i)
                } else {
                    None
                }
            })
            .next();

        if let Some(existing_index) = existing_index {
            return &self.library.static_closures[existing_index];
        }

        let id = ir::StaticClosureID(self.library.static_closures.len());
        let instance = build_static_closure_impl(closure, id, self);

        self.library.static_closures.push(instance);

        &self.library.static_closures[self.library.static_closures.len() - 1]
    }

    /// Add static closure init function calls at top of init block
    fn gen_static_closure_init(&mut self) {
        let mut static_closures_init = Vec::new();
        for static_closure in &self.library.static_closures {
            static_closures_init.push(ir::Instruction::Call {
                function: ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::Function(static_closure.init_func))),
                args: Vec::new(),
                out: None,
            });
        }
        static_closures_init.append(&mut self.library.init);
        self.library.init = static_closures_init;
    }

    fn gen_static_type_init(&mut self) {
        let mut instructions = Vec::new();

        if let Some(tag_init_func) = gen_tags_init(self) {
            instructions.push(ir::Instruction::Call {
                args: Vec::new(),
                out: None,
                function: ir::Value::from(tag_init_func),
            });
        };
        
        instructions.append(&mut self.library.init);
        self.library.init = instructions;
    }
    
    #[allow(unused)]
    pub(crate) fn add_tag(&mut self, loc: ir::TagLocation, tag: typ::ast::TagItem) {
        let loc_tags = self.tags.entry(loc).or_insert_with(Vec::new);
        
        loc_tags.push(tag);
    }

    pub(crate) fn add_tags(&mut self, loc: ir::TagLocation, tags: &[typ::ast::Tag]) {
        let loc_tags = self.tags.entry(loc).or_insert_with(Vec::new);
        
        for tag in tags {
            loc_tags.extend(tag.items.iter().cloned());
        }

        let new_tag_count = loc_tags.len();

        self.metadata_mut().alloc_tag_array(loc, new_tag_count);
    }

    pub fn tags(&self) -> impl Iterator<Item=(ir::TagLocation, &[typ::ast::TagItem])> + use <'_> {
        self.tags
            .iter()
            .map(|(loc, tags)| (*loc, tags.as_slice()))
    }
}

fn gen_iface_virtual_method_info(lib: &mut LibraryBuilder, iface_ty: &typ::Type) {
    let iface_methods = iface_ty
        .methods(&lib.src_metadata)
        .unwrap();
    let iface_id = lib.find_iface_decl(iface_ty.full_name().unwrap().as_ref()).unwrap();
    
    for method_index in 0..iface_methods.len() {
        let method = &iface_methods[method_index];

        if !method.func_decl.tags.is_empty() {
            let tags_loc = ir::TagLocation::InterfaceMethod {
                iface_id,
                method_index,
            };

            lib.add_tags(tags_loc, &method.func_decl.tags);
        }
    }
}

// class types must generate cleanup code for their inner struct which isn't
// explicitly called in IR but must be called dynamically by the target to
// clean up the inner structs of class RC cells.
// for example, a class instance maybe be stored behind an `Any` reference,
// at which point rc instructions must discover the actual class type
// dynamically from the rc cell's class pointer/class ID
fn gen_class_runtime_type(lib: &mut LibraryBuilder, class_ty: &ir::Type) {
    let resource_struct = class_ty
        .rc_resource_class_id()
        .and_then(|class_id| class_id.as_class())
        .expect("resource class of translated class type was not a struct");

    let resource_ty = ir::Type::Struct(resource_struct);
    lib.gen_runtime_type(&resource_ty);
    lib.gen_runtime_type(&class_ty);

    let weak_ty = ir::Type::RcWeakPointer(ir::VirtualTypeID::Class(resource_struct));
    lib.gen_runtime_type(&weak_ty);
}

fn expect_no_unspec_args<T: fmt::Display>(target: &T, type_args: Option<&typ::TypeArgList>) {
    if let Some(type_args) = type_args {
        let any_generic_args = type_args.items.iter().any(|arg| arg.is_generic_param());
        assert!(
            !any_generic_args,
            "name of translated variant must not contain unspecialized generics: {}",
            target
        );
    }
}
