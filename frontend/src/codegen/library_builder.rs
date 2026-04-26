mod init;
mod function;
mod rc_methods;

use crate::ast::BindingDeclKind;
use crate::ast::FunctionParamMod;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::MethodOwner;
use crate::ast::StructKind;
use crate::codegen::builder::IRBuilder;
use crate::codegen::expr::expr_to_val;
use crate::codegen::expr::literal_to_val;
use crate::codegen::metadata::*;
use crate::codegen::stmt::translate_stmt;
use crate::codegen::typ;
use crate::codegen::CodegenOpts;
use crate::codegen::FunctionInstance;
use crate::codegen::SetFlagsType;
use crate::codegen::*;
use crate::ir;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::FunctionDeclContext;
use crate::typ::builtin_funcinfo_name;
use crate::typ::builtin_ident;
use crate::typ::builtin_methodinfo_name;
use crate::typ::builtin_string_name;
use crate::typ::builtin_typeinfo_name;
use crate::typ::free_mem_sig;
use crate::typ::get_mem_sig;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::TypeParamContainer;
use crate::typ::SYSTEM_UNIT_NAME;
pub use function::*;
use init::gen_tags_init;
use ir::InstructionBuilder as _;
use ir::MetadataSource as _;
use linked_hash_map::LinkedHashMap;
pub use rc_methods::RcMethodInfo;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::version::Version;
use terapascal_common::StripMode;

#[derive(Debug)]
pub struct LibraryBuilder<'a> {
    name: String,
    version: Version,

    references: Vec<String>,

    src_metadata: &'a typ::Context,
    
    opts: CodegenOpts,
    
    type_cache: LinkedHashMap<typ::Type, ir::Type>,
    cached_types: LinkedHashMap<ir::Type, typ::Type>,

    // key is size (bits)
    set_flags_type_info: BTreeMap<usize, SetFlagsType>,

    translated_funcs: LinkedHashMap<FunctionDeclKey, FunctionInstance>,

    functions: BTreeMap<ir::FunctionID, ir::Function>,
    function_types_by_sig: HashMap<typ::FunctionSig, ir::TypeDefID>,

    variables: BTreeMap<ir::VariableID, ir::Type>,
    variables_by_name: HashMap<IdentPath, ir::VariableID>,

    static_closures: BTreeMap<ir::VariableID, ir::StaticClosure>,
    
    // looked up on first use
    free_mem_func: Option<ir::FunctionID>,
    get_mem_func: Option<ir::FunctionID>,
    
    // generated funcs for retain/release operations
    rc_methods: HashMap<ir::Type, RcMethodInfo>,
    
    // user-defined destructors by class ID
    dtors: BTreeMap<ir::TypeDefID, ir::FunctionID>,

    init_code: ir::InstructionList,
    
    metadata: ir::MetadataBuilder,
}

#[derive(Clone, Debug)]
struct BuiltinClassInfo {
    name: typ::Symbol,
    id: ir::TypeDefID,
    rtti: bool,
}

thread_local! {
    pub static BUILTIN_CLASS_INFO: [BuiltinClassInfo; 4] = [
        BuiltinClassInfo { name: builtin_string_name(), id: ir::STRING_ID, rtti: false },
        BuiltinClassInfo { name: builtin_typeinfo_name(), id: ir::TYPEINFO_ID, rtti: true },
        BuiltinClassInfo { name: builtin_methodinfo_name(), id: ir::METHODINFO_ID, rtti: true },
        BuiltinClassInfo { name: builtin_funcinfo_name(), id: ir::FUNCINFO_ID, rtti: true },
    ];
}

impl<'a> LibraryBuilder<'a> {
    pub fn new(
        name: impl Into<String>,
        version: Version,
        src_metadata: &'a typ::Context,
        metadata_refs: impl IntoIterator<Item=Arc<ir::Metadata>>,
        opts: CodegenOpts
    ) -> Self {
        let builtin_classes = BUILTIN_CLASS_INFO
            .with(|class_infos| class_infos.to_vec());

        let mut metadata = ir::MetadataBuilder::with_refs(metadata_refs);

        for class_info in &builtin_classes {
            metadata.reserve_type(class_info.id);
        }

        let type_cache: LinkedHashMap<_, _> = builtin_classes
            .iter()
            .filter(|class_info| !class_info.rtti || opts.rtti)
            .map(|class_info| {
                let src_type = typ::Type::class(class_info.name.clone());
                let result_type = class_info.id.to_class_ptr_type([]);

                (src_type, result_type)
            })
            .collect();
        
        let cached_types = type_cache
            .iter()
            .map(|(src_ty, ty)| (ty.clone(), src_ty.clone()))
            .collect();

        let builder = LibraryBuilder {
            name: name.into(),
            references: Vec::new(),

            version,

            metadata,

            opts,
            src_metadata,

            type_cache,
            cached_types,

            set_flags_type_info: BTreeMap::new(),
            
            functions: BTreeMap::new(),
            translated_funcs: LinkedHashMap::new(),

            function_types_by_sig: HashMap::new(),

            variables: BTreeMap::new(),
            variables_by_name: HashMap::new(),
            
            static_closures: BTreeMap::new(),
            
            rc_methods: HashMap::new(),
            
            dtors: BTreeMap::new(),
            
            // placeholders
            free_mem_func: None,
            get_mem_func: None,
            
            init_code: ir::InstructionList::new(),
        };

        builder
    }

    pub fn finish(mut self) -> ir::Library {
        // for all types defined in this module, ensure RTTI info is generated, even if they
        // were unused
        for defined_type in self.src_metadata.defined_types() {
            // defined generic types need to be translated with their placeholders
            let translate_type = match &defined_type {
                typ::Type::Record(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.src_metadata).unwrap();
                    typ::Type::Record(Arc::new(sym.into_owned()))
                }

                typ::Type::Class(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.src_metadata).unwrap();
                    typ::Type::Class(Arc::new(sym.into_owned()))
                }

                typ::Type::Variant(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.src_metadata).unwrap();
                    typ::Type::Variant(Arc::new(sym.into_owned()))
                }

                typ::Type::Interface(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.src_metadata).unwrap();
                    typ::Type::Interface(Arc::new(sym.into_owned()))
                }

                _ => defined_type,
            };

            self.translate_type(&translate_type);
        }

        // builtin classes are added manually to the type cache but their methods (and therefore 
        // any destructors) are expected to be defined in code, so we need to find those in the
        // source if they exist
        for class_info in BUILTIN_CLASS_INFO.with(|names| names.to_vec()) {
            let class_name = &class_info.name;

            if let Ok(def) = self.src_metadata.instantiate_struct_def(class_name, StructKind::Class) {
                let src_ty = typ::Type::class(class_name.clone());
                self.insert_type_dtor(class_info.id, src_ty, def.as_ref());
            }
        }

        self.build_typeinfo();

        self.gen_static_closure_init();
        self.gen_static_type_init();

        gen_func_invokers(&mut self);

        let metadata = self.metadata.build();

        let mut lib = ir::Library::new(self.name, self.version, self.references, metadata);
        lib.functions = self.functions;
        lib.init = self.init_code;

        lib
    }
    
    pub fn opts(&self) -> &CodegenOpts {
        &self.opts
    }

    pub fn metadata(&self) -> &ir::MetadataBuilder {
        &self.metadata
    }
    
    pub fn metadata_mut(&mut self) -> &mut ir::MetadataBuilder {
        &mut self.metadata
    }

    pub fn translate_unit(&mut self, unit: &typ::ast::Unit) {
        for (_, const_binding) in unit.binding_items(BindingDeclKind::Const) {
            let literal_val = const_binding.init
                .as_ref()
                .and_then(|init| init.expr.const_eval(&self.src_metadata))
                .expect("const binding must always have a const value");

            let binding_ty = self.translate_type(&const_binding.ty);

            for ident in &const_binding.idents {
                let binding_name = unit.ident.clone().child(ident.clone());
                let binding_path = ir::NamePath::from_ident_path(&binding_name, []);

                let value = literal_to_val(&literal_val, &binding_ty, self);

                self.metadata.new_const(binding_path, value);
            }
        }

        for (_, var) in unit.binding_items(BindingDeclKind::Var) {
            let var_ty = self.translate_type(&var.ty);

            for ident in &var.idents {
                let var_name = unit.ident.clone().child(ident.clone());
                let var_path = ir::NamePath::from_ident_path(&var_name, None);

                let id = self.metadata.new_variable(Some(var_path), var_ty.clone());

                self.variables_by_name.insert(var_name, id);
                self.variables.insert(id, var_ty.clone());

                if let Some(var_init) = &var.init {
                    let mut var_init_builder = IRBuilder::new(self);

                    let expr = expr_to_val(&var_init.expr, &mut var_init_builder);
                    var_init_builder.mov(ir::GlobalRef::Variable(id), expr);

                    let init_body = &mut var_init_builder.finish();
                    self.init_code.append(init_body);
                }
            }
        }
        
        if let Some(init_block) = &unit.init {
            let mut init_builder = IRBuilder::new(self);
            
            for stmt in &init_block.body {
                translate_stmt(stmt, &mut init_builder);
            }
            
            let unit_init = init_builder.finish();
            
            let debug_name = if self.opts.debug {
                Some(format!("{}.<init>", unit.ident))
            } else {
                None
            };

            let init_sig = ir::FunctionSig {
                param_types: Vec::new(),
                result_type: ir::Type::Nothing
            };
            let init_func = ir::FunctionDef {
                body: unit_init,
                sig: init_sig.clone(),
                type_params: Vec::new(),
                debug_name,
            };

            let init_func_id = self.metadata.insert_func(None, init_sig, false, []);
            self.functions.insert(init_func_id, ir::Function::Local(init_func));

            self.init_code.push(ir::Instruction::Call {
                function: ir::Ref::Global(ir::GlobalRef::Function(init_func_id)).into(),
                args: Vec::new(),
                type_args: Vec::new(),
                out: None,
            }, None);
        }

        if self.opts.strip <= StripMode::UnusedImpl {
            for iface_decl in &unit.iface_section.decls {
                match iface_decl {
                    typ::ast::UnitDecl::FunctionDecl { decl } 
                    if decl.name.type_params.is_none() => {
                        self.translate_unit_func_decl(&unit.ident, decl);
                    }

                    typ::ast::UnitDecl::FunctionDef { def } 
                    if def.decl.name.type_params.is_none() => {
                        self.translate_unit_func_decl(&unit.ident, &def.decl);
                    }
                    
                    _ => {
                        continue;
                    }
                };
            }
        }
    }

    pub(crate) fn instantiate_func(&mut self, key: &FunctionDeclKey) -> FunctionInstance {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match key {
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
                self.instantiate_method(method_key)
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

        self.gen_type_info(&set_flags_type.struct_id.to_struct_type([]));

        set_flags_type
    }

    pub fn translate_tag_groups(&mut self, tags: &[typ::ast::Tag]) -> Vec<ir::TagInfo> {
        // most groups will probably contain 1 tag
        let mut result = Vec::with_capacity(tags.len());
        
        for group in tags {
            result.extend(self.translate_tags(&group.items));
        }
        
        result
    }

    pub fn translate_tags(&mut self, tags: &[typ::ast::TagItem]) -> Vec<ir::TagInfo> {
        let mut result = Vec::with_capacity(tags.len());
        
        for item in tags {
            let tag_type = self.translate_type(&item.tag_type);
            
            // typechecker must ensure all tags are class objects
            let ir::Type::Object(ir::ObjectID::Class(tag_class_id)) = tag_type else {
                panic!("translate_tags: illegal type for tag item: {}", item.tag_type);
            };
            
            let Some(tag_def) = self.metadata.get_struct_def(tag_class_id.def_id).cloned() else {
                panic!("translate_tags: missing definition for type used as tag: {}", item.tag_type);
            };
            
            let mut tag_info = ir::TagInfo::new(tag_class_id.def_id);

            for arg in &item.args.members {
                let Some((field_id, field_type)) = tag_def.fields
                    .iter()
                    .find_map(|(id, field_def)| {
                        let Some(field_name) = &field_def.name else {
                            return None;
                        };

                        (*field_name == *arg.ident.name).then_some((*id, &field_def.ty))
                    })
                else {
                    panic!("translate_tags: missing definition for field {}.{}", item.tag_type, arg.ident);
                };

                match arg.value.annotation() {
                    typ::Value::Const(const_val) => {
                        let field_val = literal_to_val(&const_val.value, field_type, self);
                        tag_info.fields.insert(field_id, field_val);
                    }
                    
                    _ => {
                        panic!("translate_tags: constructor of tag must only contain const values (field `{}`: {} was {})", arg.ident, arg.value, arg.value.annotation());
                    }
                }
            }
            
            result.push(tag_info);
        }
        
        result
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
            func_def.decl.name.type_params.as_ref(),
            &func_def.decl.result_ty,
            &func_def.locals,
            &func_def.body,
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
        let return_ty = self.translate_type(&sig.result_ty);

        let param_tys = sig.params
            .iter()
            .map(|sig_param| {
                let param_ty = self.translate_type(&sig_param.ty);
                match sig_param.modifier {
                    None => param_ty,
                    Some(FunctionParamMod::Var | FunctionParamMod::Out) => param_ty.ptr(),
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
                src: extern_src.value.as_ref().clone(),
                symbol: (*extern_decl.name.ident.name).clone(),

                sig: ir::FunctionSig { result_type: return_ty, param_types: param_tys },
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
                typ::Type::class(sym.as_ref().clone().with_ty_args(None))
            }

            typ::Type::Record(sym) if sym.type_args.is_some() => {
                typ::Type::record(sym.as_ref().clone().with_ty_args(None))
            }

            typ::Type::Variant(sym) if sym.type_args.is_some() => {
                typ::Type::variant(sym.as_ref().clone().with_ty_args(None))
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
            .get_method(method_key.method_index, &self.src_metadata)
            .unwrap_or_else(|e| {
                panic!("instantiate_method: failed to get method metadata for {} method {}: {}", decl_self_ty, method_key.method_index, e)
            });
        
        let method_sig = Arc::new(method_decl.func_decl.sig());

        let method_def = self
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

        let ns = method_key
            .self_ty
            .full_path()
            .expect("instantiate_method: methods should only be generated for named types")
            .into_owned();

        let id = self.declare_func(&method_def.decl, ns);

        // cache the function before translating the instantiation, because
        // it may recurse and instantiate itself in its own body
        let func_instance = FunctionInstance {
            id,
            src_sig: Arc::new(method_def.decl.sig()),
            published: method_decl.is_published(),
        };

        let key = FunctionDeclKey::Method(method_key.clone());

        let debug_name = method_def.decl.name.to_debug_string(None);
        
        self.translated_funcs.insert(key, func_instance.clone());

        let def = build_func_def(
            self,
            &method_def.decl.param_groups,
            method_def.decl.name.type_params.as_ref(),
            &method_def.decl.result_ty,
            &method_def.locals,
            &method_def.body,
            true,
            Some(debug_name),
        );
        self.functions.insert(id, ir::Function::Local(def));

        func_instance
    }

    fn instantiate_virtual_method(&mut self, virtual_key: &VirtualMethodKey) -> FunctionInstance {
        let impl_method = &virtual_key.impl_method;

        // virtual methods can't be generic
        let self_ty = self.translate_type(&impl_method.self_ty);

        // virtual calls can't have type args yet
        let impl_func = self.instantiate_method(&impl_method);

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

                let mut builder = IRBuilder::new(self);
                let iface_meta = builder.translate_iface(&src_iface_def);
                self.metadata.define_iface(iface_meta)
            });

        self.metadata.impl_method(iface_id, self_ty, method_name, impl_func.id);

        let key = FunctionDeclKey::VirtualMethod(virtual_key.clone());

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
    ) -> FunctionInstance {
        let key = FunctionDeclKey::Method(MethodDeclKey {
            method_index: self_ty_method_index,
            self_ty,
        });

        // methods must always be present so make sure they're immediately instantiated
        self.instantiate_func(&key)
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

        let tags = self.translate_tag_groups(&func_decl.tags);

        let sig = func_decl.sig();
        let ir_sig = translate_sig(&sig, self);

        self.metadata.insert_func(global_name, ir_sig, self.opts.rtti, tags)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        func_sig: Arc<typ::FunctionSig>,
    ) -> FunctionInstance {
        let key = FunctionDeclKey::Function { name: func_name, sig: func_sig };

        self.instantiate_func(&key)
    }

    fn translate_unit_func_decl(
        &mut self,
        unit_name: &IdentPath,
        decl: &typ::ast::FunctionDecl,
    ) -> Option<FunctionInstance> {
        if decl.type_params_len() > 0 {
            return None;
        }
        
        let func_name = unit_name.clone().child(decl.name.ident.clone());
        let func_sig = decl.sig();

        let instance = self.translate_func(func_name, Arc::new(func_sig));
        Some(instance)
    }

    pub fn insert_function(&mut self, id: ir::FunctionID, function: ir::Function) {
        assert!(
            self.metadata.get_function_info(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.functions.insert(id, function);
    }

    pub fn find_iface_decl(&mut self, iface_name: &typ::Symbol) -> Option<ir::InterfaceID> {
        let name = translate_name(iface_name, self);

        self.metadata
            .interfaces()
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
    fn gen_iface_impls(&mut self, src_self_ty: &typ::Type, self_ty: &ir::Type) {
        let ifaces = src_self_ty
            .implemented_ifaces(&self.src_metadata)
            .unwrap_or_else(|err| {
                panic!("failed to retrieve implementation list for type {}: {}", src_self_ty, err)
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

                let impl_sig = iface_method.func_decl.sig().with_self(&src_self_ty);
                let impl_index = self.find_method_index(&src_self_ty, &method_name, &impl_sig);

                let virtual_key = VirtualMethodKey {
                    iface_ty: iface_ty.clone(),
                    iface_method_index,

                    impl_method: MethodDeclKey {
                        self_ty: src_self_ty.clone(),
                        method_index: impl_index,
                    },
                };

                self.instantiate_func(&FunctionDeclKey::VirtualMethod(virtual_key));
            }

            let iface_name = iface_ty.full_name()
                .expect("interface types must have names");
            let iface_id = self.find_iface_decl(iface_name.as_ref())
                .expect("implemented interface type must already be declared");

            self.metadata_mut().declare_iface_impl(iface_id, self_ty.clone());
        }
    }

    pub fn apply_ty_args<Generic>(&self,
        target: Generic,
        params: &impl TypeParamContainer,
        args: &impl typ::TypeArgResolver
    ) -> Generic 
    where 
        Generic: typ::Specializable,
    {
        target.apply_type_args(params, args)
    }
    
    fn insert_type_dtor(&mut self,
        type_id: ir::TypeDefID,
        src_ty: typ::Type,
        src_def: &impl MethodOwner<typ::Value>
    ) {
        let Some(dtor_method_index) = src_def.find_dtor_index() else {
            return;
        };

        let dtor = self.instantiate_method(&MethodDeclKey {
            self_ty: src_ty,
            method_index: dtor_method_index,
        });
        
        self.dtors.insert(type_id, dtor.id);
    }
    
    fn add_cached_type(&mut self, src_ty: typ::Type, ty: ir::Type) {
        self.type_cache.insert(src_ty.clone(), ty.clone());
        self.cached_types.insert(ty.clone(), src_ty.clone());
    }

    pub fn translate_type(&mut self, src_ty: &typ::Type) -> ir::Type {
        if let Some(cached) = self.type_cache.get(&src_ty) {
            let ty = cached.clone();
            return ty;
        }

        // instantiate types which may contain generic params
        let ty = match &src_ty {
            typ::Type::Variant(name) => {
                self.translate_variant_type(src_ty, name)
            }

            typ::Type::Record(name) => {
                self.translate_struct_type(src_ty, name, StructKind::Record)
            },

            typ::Type::Class(name) => {
                self.translate_struct_type(src_ty, name, StructKind::Class)
            }
            
            typ::Type::Weak(weak_ty) => {
                let ty = match self.translate_type(weak_ty) {
                    ir::Type::Object(id) => ir::Type::WeakObject(id),
                    other => unreachable!("only RC class types can be weak, found: {}", other),
                };

                self.add_cached_type(src_ty.clone(), ty.clone());
                
                ty
            }

            typ::Type::Interface(iface_sym) => {
                let iface_def = self.src_metadata.instantiate_iface_def(iface_sym).unwrap();

                let iface_name = translate_name(&iface_def.name, self);
                let id = self.metadata.declare_iface(&iface_name);
                let ty = ir::Type::Object(ir::ObjectID::Interface(id));

                self.add_cached_type(src_ty.clone(), ty.clone());

                let iface_meta = translate_iface(&iface_def, self);
                let def_id = self.metadata.define_iface(iface_meta);
                assert_eq!(def_id, id);

                ty
            },

            typ::Type::Array(array_ty) => {
                let elem_ty = self.translate_type(&array_ty.element_ty);
                let ty = elem_ty.array(array_ty.dim);

                self.add_cached_type(src_ty.clone(), ty.clone());
                ty
            },

            typ::Type::DynArray(element) => {
                let ty = self.translate_type(element).dyn_array();

                self.add_cached_type(src_ty.clone(), ty.clone());

                ty
            },

            typ::Type::Function(func_sig) => {
                let func_ty_id = self.translate_func_ty(func_sig);
                let ty = ir::Type::Object(ir::ObjectID::AnyClosure(func_ty_id));

                self.add_cached_type(src_ty.clone(), ty.clone());

                ty
            },
            
            typ::Type::Set(set_ty) => {
                let flags_ty = self.get_set_flags_type_info(set_ty.flags_type_bits());

                let ty = ir::Type::Flags(flags_ty.struct_id);

                self.add_cached_type(src_ty.clone(), ty.clone());
                
                ty
            }

            typ::Type::Nothing => {
                self.add_cached_type(src_ty.clone(), ir::Type::Nothing);
                ir::Type::Nothing
            },

            typ::Type::Primitive(typ::Primitive::Boolean) => {
                self.add_cached_type(src_ty.clone(), ir::Type::Bool);
                ir::Type::Bool
            },

            typ::Type::Primitive(typ::Primitive::Int8) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I8);
                ir::Type::I8
            },
            typ::Type::Primitive(typ::Primitive::UInt8) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U8);
                ir::Type::U8
            },
            typ::Type::Primitive(typ::Primitive::Int16) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I16);
                ir::Type::I16
            },
            typ::Type::Primitive(typ::Primitive::UInt16) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U16);
                ir::Type::U16
            },
            typ::Type::Primitive(typ::Primitive::Int32) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I32);
                ir::Type::I32
            },
            typ::Type::Primitive(typ::Primitive::UInt32) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U32);
                ir::Type::U32
            },
            typ::Type::Primitive(typ::Primitive::Int64) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I64);
                ir::Type::I64
            },
            typ::Type::Primitive(typ::Primitive::UInt64) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U64);
                ir::Type::U64
            },
            typ::Type::Primitive(typ::Primitive::NativeInt) => {
                self.add_cached_type(src_ty.clone(), ir::Type::ISize);
                ir::Type::ISize
            },
            typ::Type::Primitive(typ::Primitive::NativeUInt) => {
                self.add_cached_type(src_ty.clone(), ir::Type::USize);
                ir::Type::USize
            },

            typ::Type::Primitive(typ::Primitive::Real32) => {
                self.add_cached_type(src_ty.clone(), ir::Type::F32);
                ir::Type::F32
            },
            typ::Type::Primitive(typ::Primitive::Real64) => {
                self.add_cached_type(src_ty.clone(), ir::Type::F64);
                ir::Type::F64
            },

            typ::Type::Primitive(typ::Primitive::Pointer) => {
                self.add_cached_type(src_ty.clone(), ir::Type::Nothing.ptr());
                ir::Type::Nothing.ptr()
            },

            typ::Type::Any => {
                self.add_cached_type(src_ty.clone(), ir::ANY_TYPE);
                ir::ANY_TYPE
            }

            typ::Type::Pointer(inner) => {
                let inner_type = self.translate_type(inner);
                let ptr_type = inner_type.ptr();
                self.add_cached_type(src_ty.clone(), ptr_type.clone());
                ptr_type
            }

            typ::Type::Box(inner) => {
                let inner_type = self.translate_type(inner);
                let boxed_type = inner_type.boxed();
                self.add_cached_type(src_ty.clone(), boxed_type.clone());
                boxed_type
            }

            typ::Type::Enum(..) => {
                let ord_type = self.translate_type(&typ::ast::ENUM_ORD_TYPE);
                self.add_cached_type(src_ty.clone(), ord_type.clone());
                ord_type
            }

            typ::Type::GenericParam(param) => {
                ir::Type::Generic(Rc::new(param.name.name.to_string()))
            }

            typ::Type::MethodSelf => {
                panic!("translate_type: unresolved Self type")
            }
        };

        // ensure the runtime type info exists for all referenced types
        if self.metadata().is_defined(&ty) {
            self.gen_type_info(&ty);
        }

        ty
    }

    fn translate_variant_type(&mut self, src_type: &typ::Type, name: &typ::Symbol) -> ir::Type {
        // the definition uses the generic (unspecialized) type, which is
        // shared between all specializations
        let def_name = name.clone().to_generic_name();
        let def_path = translate_name(&def_name, self);

        let def_id = match self.metadata.find_variant_def(&def_path) {
            Some((def_id, _)) => {
                def_id
            }

            None => {
                let src_def = self.src_metadata
                    .find_variant_def(&def_name.full_path)
                    .unwrap();

                let def_id = self.metadata.new_type();

                let def_type = typ::Type::variant(def_name);
                let variant_type = def_id.to_variant_type(def_path.type_args.clone());

                self.add_cached_type(def_type.clone(), variant_type.clone());

                self.metadata.declare_type(def_id, &def_path);

                let def = translate_variant_def(&src_def, self);
                self.metadata.define_variant(def_id, def);

                self.insert_type_dtor(def_id, def_type.clone(), src_def.as_ref());

                def_id
            }
        };

        let name_path = translate_name(name, self);
        if name_path == def_path {
            return def_id.to_variant_type(def_path.type_args.clone());
        }

        let instance_type = def_id.to_variant_type(name_path.type_args.clone());
        self.add_cached_type(src_type.clone(), instance_type.clone());
        instance_type
    }

    fn translate_struct_type(
        &mut self,
        src_type: &typ::Type,
        name: &typ::Symbol,
        kind: StructKind,
    ) -> ir::Type {
        let def_name = name.clone().to_generic_name();
        let def_path = translate_name(&def_name, self);

        let type_ctor = |id: ir::TypeDefID, args: &[ir::Type]| match kind {
            StructKind::Class => {
                id.to_class_ptr_type(args.to_vec())
            },
            StructKind::Record => {
                id.to_struct_type(args.to_vec())
            },
        };

        let def_id = match self.metadata.find_struct_def(&def_path) {
            Some((def_id, _)) => {
                def_id
            }

            None => {
                let src_def = self.src_metadata
                    .find_struct_def(&name.full_path, kind)
                    .unwrap();

                let def_src_type = typ::Type::from_struct_type(def_name.clone(), kind);

                let def_id = self.metadata.new_type();
                let def_type = type_ctor(def_id, &def_path.type_args);

                self.add_cached_type(def_src_type.clone(), def_type);
                self.metadata.declare_type(def_id, &def_path);

                let def = translate_struct_def(&src_def, self);
                self.metadata.define_struct(def_id, def);

                self.insert_type_dtor(def_id, def_src_type, src_def.as_ref());
                def_id
            }
        };

        let name_path = translate_name(name, self);
        if name_path == def_path {
            return type_ctor(def_id, &def_path.type_args);
        }

        let instance_type = type_ctor(def_id, &name_path.type_args);
        self.add_cached_type(src_type.clone(), instance_type.clone());
        instance_type
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
    
    pub fn get_rc_method_info(&mut self, ty: &ir::Type) -> RcMethodInfo {
        if let Some(method_info) = self.rc_methods.get(ty) {
            return *method_info;
        }

        let method_info = RcMethodInfo::gen_for_type(self, ty);
        self.rc_methods.insert(ty.clone(), method_info);
        
        method_info
    }

    // get or generate runtime type for a given type, which contains the function IDs etc
    // used for RC operations at runtime. the rest of the RTTI info will be filled in later 
    // in a separate pass
    pub fn gen_type_info(&mut self, ty: &ir::Type) -> Rc<ir::TypeInfo> {
        if let Some(existing) = self.metadata.get_type_info(&ty) {
            return existing;
        }

        assert!(self.metadata().is_defined(ty), "gen_runtime_type: type {} ({:?}) is not defined yet", self.metadata().pretty_type_name(ty), ty);
        
        let flags = ir::TypeInfo::type_runtime_flags(ty);

        // type names and methods will be added after codegen
        let mut rtti = ir::TypeInfo::new(None, flags);
        
        if self.opts.debug && self.opts.rtti {
            rtti.debug_name = Some(self.metadata().pretty_type_name(&ty).into_owned());
        }

        self.metadata.insert_type_info(ty.clone(), rtti)
    }

    fn build_typeinfo(&mut self) {
        // TODO: this might be overly defensive, maybe the type cache will never change here?
        // clone the type cache for iteration
        // the RTTI generation process can touch new parts of code and
        // cause the type cache to expand, so repeat until it stops growing
        let mut done_types = 0;
        let mut done_closures = 0;

        let mut populate_types = Vec::new();
        let mut populate_closures = Vec::new();

        loop {
            populate_types.extend(self.type_cache
                .iter()
                .skip(done_types)
                .map(|(src_ty, ty)| (src_ty.clone(), ty.clone())));
            
            let closure_types = self.metadata.closures();
            populate_closures.extend(closure_types.skip(done_closures));
            
            for closure_id in populate_closures.drain(0..) {
                gen_closure_runtime_type(self, closure_id);
                self.gen_type_info(&closure_id.to_class_ptr_type([]));
                self.gen_type_info(&closure_id.to_class_weak_type([]));
                done_closures += 1;
            }

            for (src_ty, ty) in populate_types.drain(0..) {
                done_types += 1;
                
                // for any non-object types, ensure boxed versions exist in the type cache
                // for use with RTTI
                if !src_ty.is_object() 
                    && self.opts().rtti 
                    && let Ok(true) = src_ty.is_sized(&self.src_metadata) 
                {
                    let box_src_type  = src_ty.clone().boxed();
                    let box_type = self.translate_type(&box_src_type);

                    self.gen_type_info(&box_type);
                }

                if !self.metadata.is_defined(&ty) {
                    continue;
                }

                if src_ty.as_class().is_ok() {
                    gen_class_runtime_type(self, &ty);
                }
                
                if matches!(src_ty, typ::Type::DynArray { .. }) {
                    gen_dynarray_runtime_type(self, &ty);
                }

                self.gen_iface_impls(&src_ty, &ty);

                self.populate_runtime_type_info(src_ty, ty.clone());
            }

            if done_types == self.type_cache.len() {
                break;
            }
        }
    }

    fn populate_runtime_type_info(&mut self, src_ty: typ::Type, ty: ir::Type) {
        if !self.opts.rtti {
            return;
        }
        
        // should fetch from the cache at this point
        let mut type_info = (*self.gen_type_info(&ty)).clone();
        type_info.name = Some(self.metadata.find_or_insert_string(&src_ty.to_string()));

        match &src_ty {
            typ::Type::Record(name) | typ::Type::Class(name) => {
                let def = self.src_metadata
                    .instantiate_struct_def(name.as_ref(), src_ty.struct_kind().unwrap())
                    .unwrap();

                for (method_index, method) in def.methods().enumerate() {
                    let method_decl = &method.func_decl;

                    if method.is_published() && method_decl.name.type_params.is_none() {
                        let method_info = self.create_method_info(ty.clone(), &src_ty, method_index, false, method_decl);
                        type_info.methods.push(method_info);
                    }
                }
            }
            
            typ::Type::Interface(name) => {
                let def = self.src_metadata
                    .instantiate_iface_def(name.as_ref())
                    .unwrap();

                for (method_index, method) in def.methods.iter().enumerate() {
                    let method_decl = &method.decl;

                    if method_decl.name.type_params.is_none() {
                        let method_info = self.create_method_info(ty.clone(), &src_ty, method_index, true, method_decl);
                        type_info.methods.push(method_info);
                    }
                }
            }

            _ => {
                // type has no publishable methods
            }
        };

        // replace the existing RTTI
        self.metadata.insert_type_info(ty, type_info);
    }

    fn create_method_info(&mut self,
        instance_ty: ir::Type,
        src_ty: &typ::Type,
        method_index: usize,
        is_abstract: bool,
        decl: &typ::ast::FunctionDecl,
    ) -> ir::MethodInfo {
        let tags = self.translate_tag_groups(&decl.tags);
        
        let method_name = decl.name.ident.as_str();
        let method_name_id = self.metadata.find_or_insert_string(method_name);

        let params = decl
            .params()
            .map(|(param, _)| {
                // in RTTI, "self" params for interfaces become untyped pointers
                let mut param_ty = match param.ty.ty() {
                    typ::Type::MethodSelf => ir::Type::Nothing.ptr(),
                    ty => self.translate_type(ty)
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
            let method_instance = self.instantiate_method(&method_key);
            Some(method_instance.id)
        };

        ir::MethodInfo {
            function,
            index: method_index,
            instance_ty,
            name: method_name_id,
            params,
            result_ty: self.translate_type(&decl.result_ty),
            tags,
        }
    }

    pub fn translate_func_ty(&mut self, func_sig: &typ::FunctionSig) -> ir::TypeDefID {
        let func_ty_id = match self.find_func_ty(&func_sig) {
            Some(id) => id,
            None => {
                let mut ir_sig = translate_sig(&func_sig, self);

                // any reference to a function value creates a closure, which has an extra param
                // for the reference to itself
                ir_sig.param_types.insert(0, ir::Type::any());

                self.define_func_ty(func_sig.clone(), ir_sig)
            },
        };

        func_ty_id
    }

    pub fn build_closure_instance(
        &mut self,
        func: &typ::ast::AnonymousFunctionDef,
    ) -> ClosureInstance {
        // this is the signature of the *function type* of the closure, not the signature of
        // the real method implementing the closure, which has an extra type-erased parameter
        // for the closure itself
        let func_ty_sig = typ::FunctionSig::of_anonymous_func(func);

        let func_ty_id = self.translate_func_ty(&func_ty_sig);

        // since we're translating a closure function, the sig needs to include the implicit
        // closure object parameter
        let mut sig = translate_sig(&func.sig(), self);
        sig.param_types.insert(0, ir::Type::any());

        let id = self.metadata.insert_func(None, sig, false, []);

        let closure_identity = ir::ClosureIdentity {
            virt_func_ty: func_ty_id,
            id,
        };

        let closure_id = translate_closure_struct(
            closure_identity,
            &func.captures,
            self
        );

        let debug_name = if self.opts.debug {
            Some("<anonymous function>".to_string())
        } else {
            None
        };

        let func_def = build_closure_function_def(self, &func, closure_id, debug_name);

        self.functions.insert(id, ir::Function::Local(func_def));

        ClosureInstance {
            func_instance: FunctionInstance {
                id,
                src_sig: func_ty_sig,
                published: false,
            },
            func_ty_id,
            closure_id,
        }
    }

    pub fn build_func_static_closure_instance(
        &mut self,
        func: &FunctionInstance
    ) -> &ir::StaticClosure {
        if let Some(existing_id) = self.static_closures
            .iter()
            .find_map(|(id, static_closure)| {
                (static_closure.func == func.id).then_some(*id)
            })
        {
            return &self.static_closures[&existing_id];
        }

        // function reference closures can never have a capture list or type args
        let captures = LinkedHashMap::default();

        let func_ty_id = self.translate_func_ty(func.src_sig.as_ref());

        let ir_func = self.functions
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
            self
        );

        // build the closure function, which is a thunk that just calls the global function
        let mut closure_func_sig = translate_sig(&func.src_sig, self);

        // closure parameter (unused for static closures)
        closure_func_sig.param_types.insert(0, ir::Type::any());

        let thunk_id = self.metadata.insert_func(None, closure_func_sig, false, []);
        let thunk_def = build_func_static_closure_def(self, func, &ir_func);

        self.functions.insert(thunk_id, ir::Function::Local(thunk_def));

        let closure = ClosureInstance {
            closure_id,
            func_ty_id,
            func_instance: FunctionInstance {
                id: thunk_id,
                src_sig: func.src_sig.clone(),
                published: false,
            },
        };

        let static_closure = self.build_static_closure_instance(closure);
        let static_closure_id = static_closure.id;

        &self.static_closures[&static_closure_id]
    }

    pub fn build_static_closure_instance(&mut self, closure: ClosureInstance) -> &ir::StaticClosure {
        let existing_id = self.static_closures.iter()
            .filter_map(|(var_id, static_closure)| {
                if static_closure.closure_id == closure.closure_id {
                    Some(var_id)
                } else {
                    None
                }
            })
            .next();

        if let Some(id) = existing_id {
            return self.static_closures.get(id).unwrap();
        }

        let id = self.metadata.new_variable(None, closure.func_ty_id.to_closure_ptr_type());
        let instance = build_static_closure_impl(closure, id, self);

        self.static_closures.insert(id, instance);
        self.static_closures.get(&id).unwrap()
    }

    /// Add static closure init function calls at top of init block
    fn gen_static_closure_init(&mut self) {
        let mut static_closures_init = ir::InstructionList::new();
        for (_id, static_closure) in &self.static_closures {
            static_closures_init.push(ir::Instruction::Call {
                function: ir::Ref::from(static_closure.init_func).value(),
                args: Vec::new(),
                type_args: Vec::new(),
                out: None,
            }, None);
        }
        static_closures_init.append(&mut self.init_code);
        self.init_code = static_closures_init;
    }

    fn gen_static_type_init(&mut self) {
        let mut instructions = ir::InstructionList::new();

        if self.opts.rtti && let Some(tag_init_func) = gen_tags_init(self) {
            instructions.push(ir::Instruction::Call {
                args: Vec::new(),
                type_args: Vec::new(),
                out: None,
                function: ir::Value::from(tag_init_func),
            }, None);
        };

        instructions.append(&mut self.init_code);
        self.init_code = instructions;
    }
}

fn gen_dynarray_runtime_type(lib: &mut LibraryBuilder, array_type: &ir::Type) {
    let ir::Type::Object(ir::ObjectID::Array(element_type)) = &array_type else {
        panic!("dyn array type did not translate to a dyn array reference");
    };
    
    // the destructor can be skipped entirely if the element isn't an RC object itself and
    // doesn't have a release function for its own elements
    let element_release = lib.get_rc_method_info(&element_type).release_elements;
    if element_release.is_none() && !element_type.is_object() {
        return;
    }
    let dtor_sig = ir::FunctionSig {
        param_types: vec![array_type.clone()],
        result_type: ir::Type::Nothing,
    };

    let dtor_id = lib.metadata.insert_func(None, dtor_sig.clone(), false, []);

    let mut dtor_builder = IRBuilder::new(lib);
    
    let self_arg = ir::ArgID(0);
    dtor_builder.bind_param(self_arg, array_type.clone(), "self");

    dtor_builder.gen_dyn_array_dtor_body(self_arg, element_type);
    
    let dtor_body = dtor_builder.finish();

    let dtor_debug_name = lib.opts.debug.then(|| {
        format!("generated dtor for {}", lib.metadata.pretty_type_name(array_type))
    });

    lib.insert_function(dtor_id, ir::Function::Local(ir::FunctionDef {
        debug_name: dtor_debug_name,
        type_params: Vec::new(),
        sig: dtor_sig,
        body: dtor_body,
    }));

    let mut runtime_type = lib.gen_type_info(array_type).as_ref().clone();
    runtime_type.dtor = Some(dtor_id);
    lib.metadata.insert_type_info(array_type.clone(), runtime_type);
}

fn gen_closure_runtime_type(lib: &mut LibraryBuilder, closure_id: ir::TypeDefID) {
    let type_id = ir::GenericTypeID::new(closure_id, []);
    let closure_class_ty = type_id.to_class_object_type();
    let closure_weak_ty = type_id.to_weak_class_object_type();

    let runtime_type = lib.gen_type_info(&closure_class_ty);
    let mut runtime_type = (*runtime_type).clone();

    let mut weak_runtime_type = lib.gen_type_info(&closure_weak_ty)
        .as_ref()
        .clone();

    gen_class_dtor(lib, &type_id, &mut runtime_type);

    // this type is the unnamed class implementing a closure, give it the function flag too
    runtime_type.flags |= ir::TYPE_FLAG_FUNCTION;
    weak_runtime_type.flags |= ir::TYPE_FLAG_FUNCTION;
    
    lib.metadata.insert_type_info(closure_class_ty, runtime_type);
    lib.metadata.insert_type_info(closure_weak_ty, weak_runtime_type);
}

// class types must generate cleanup code for their inner struct which isn't
// explicitly called in IR but must be called dynamically by the target to
// clean up the inner structs of class RC cells.
// for example, a class instance maybe be stored behind an `Any` reference,
// at which point rc instructions must discover the actual class type
// dynamically from the rc cell's class pointer/class ID
fn gen_class_runtime_type(lib: &mut LibraryBuilder, class_ty: &ir::Type) {
    let class_id = class_ty
        .as_object()
        .and_then(|class_id| class_id.as_class())
        .expect("resource class of translated class type was not a struct");

    lib.gen_type_info(&class_ty);
    lib.gen_type_info(&class_id.to_class_object_type());

    let mut runtime_type = lib.gen_type_info(&class_ty)
        .as_ref()
        .clone();

    gen_class_dtor(lib, class_id, &mut runtime_type);

    lib.metadata.insert_type_info(class_ty.clone(), runtime_type);
}

fn gen_class_dtor(
    lib: &mut LibraryBuilder,
    class_id: &Rc<ir::GenericTypeID>,
    runtime_type: &mut ir::TypeInfo,
) {
    let class_ty = class_id.to_class_object_type();

    let user_dtor = lib.dtors.get(&class_id.def_id).cloned();

    let mut dtor_builder = IRBuilder::new(lib);
    
    let self_arg = ir::ArgID(0);
    dtor_builder.bind_param(self_arg, class_ty.clone(), "self");

    let mut has_dtor = false;
    if let Some(user_dtor_id) = user_dtor {
        dtor_builder.call(user_dtor_id, [self_arg.value()], [], None);
        has_dtor = true;
    }
    
    if dtor_builder.gen_class_object_dtor_body(class_id, self_arg) {
        has_dtor = true;
    }

    if !has_dtor {
        return;
    }

    let dtor_body = dtor_builder.finish();

    let dtor_sig = ir::FunctionSig {
        param_types: vec![class_ty.clone()],
        result_type: ir::Type::Nothing,
    };

    let dtor_id = lib.metadata.insert_func(None, dtor_sig.clone(), false, []);
    runtime_type.dtor = Some(dtor_id);

    let dtor_debug_name = lib.opts.debug
        .then(|| format!("generated dtor for {}", lib.metadata.pretty_type_name(&class_ty)));

    lib.insert_function(dtor_id, ir::Function::Local(ir::FunctionDef {
        debug_name: dtor_debug_name,
        body: dtor_body,
        sig: dtor_sig,
        type_params: Vec::new(),
    }));
}

fn gen_func_invokers(lib: &mut LibraryBuilder) {
    if !lib.opts.rtti {
        return;
    }
    
    // temporarily swap because we can't build new functions with this borrowed
    let mut all_funcs = LinkedHashMap::new();
    mem::swap(&mut all_funcs, &mut lib.translated_funcs);

    let invoker_sig = ir::FunctionSig::new([
        ir::ANY_TYPE.temp_ref(), // self-arg
        ir::ANY_TYPE.dyn_array(), // rest args
        ir::Type::I32.temp_ref(), // error code out
    ], ir::ANY_TYPE);

    for (_, func) in &all_funcs {
        if !func.published {
            continue;
        }

        let debug_name = if lib.opts.debug {
            let debug_name = lib.functions[&func.id].debug_name();
            
            let func_name_display = match debug_name {
                Some(name) => format!("{name} ({})", func.id),
                None => format!("{}", func.id),
            };

            Some(format!("generated invoker for {}", func_name_display))
        } else {
            None
        };

        let invoker_id = lib.metadata_mut().insert_func(None, invoker_sig.clone(), false, []);
        
        // the source sig may be generic, so look up the translated sig rather than attempting
        // to translate it without a generic context
        let sig = lib.functions[&func.id].sig().clone();

        let mut builder = IRBuilder::new(lib);
        builder.bind_return(sig.result_type.clone());

        let self_arg = ir::ArgID(0);
        let args_arg = ir::ArgID(1);
        let error_out_arg = ir::ArgID(2);
        
        builder.bind_param(self_arg, ir::ANY_TYPE.temp_ref(), "self");
        builder.bind_param(args_arg, ir::ANY_TYPE.dyn_array(), "args");
        builder.bind_param(error_out_arg, ir::Type::I32.temp_ref(), "error_out");

        builder.gen_invoker_body(
            func.id,
            &sig,
            self_arg.to_ref(),
            args_arg.to_ref(),
            error_out_arg.to_deref(),
        );
        
        let body = builder.finish();
        
        let invoker_func = ir::FunctionDef {
            sig: invoker_sig.clone(),
            debug_name,
            body,
            type_params: Vec::new(),
        };

        lib.functions.insert(invoker_id, ir::Function::Local(invoker_func));

        lib.metadata.insert_func_invoker(func.id, invoker_id);
    }
    
    // nothing we do above should modify this!
    assert_eq!(0, lib.translated_funcs.len());
    mem::swap(&mut all_funcs, &mut lib.translated_funcs);
    
}