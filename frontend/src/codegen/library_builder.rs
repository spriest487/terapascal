mod init;
mod build_functions;

pub use self::build_functions::*;
use self::init::gen_tags_init;
use crate::ast::BindingDeclKind;
use crate::ast::FunctionParamMod;
use crate::ast::Ident;
use crate::ast::IdentPath;
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
use crate::typ::builtin_funcinfo_name;
use crate::typ::builtin_methodinfo_name;
use crate::typ::builtin_string_name;
use crate::typ::builtin_typeinfo_name;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::Primitive;
use crate::typ::Specializable;
use crate::typ::TypeArgsResult;
use ir::InstructionBuilder as _;
use linked_hash_map::LinkedHashMap;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::version::Version;

#[derive(Debug)]
pub struct LibraryBuilder<'a> {
    name: String,
    version: Version,

    references: &'a [LibraryRef],

    tags: Vec<ir::TagInfo>,

    root_ctx: &'a typ::Context,

    opts: CodegenOpts,

    type_cache: LinkedHashMap<typ::Type, ir::Type>,
    cached_types: LinkedHashMap<ir::Type, typ::Type>,

    // key is size (bits)
    flags_repr_types: BTreeMap<usize, FlagsReprType>,

    translated_funcs: LinkedHashMap<FunctionDeclKey, FunctionInstance>,

    functions: BTreeMap<ir::FunctionID, ir::Function>,
    function_types_by_sig: HashMap<typ::FunctionSig, ir::TypeDefID>,

    variables: BTreeMap<ir::VariableID, ir::Type>,
    variables_by_name: HashMap<IdentPath, ir::VariableID>,

    static_closures: BTreeMap<ir::VariableID, ir::StaticClosure>,

    // looked up on first use
    free_mem_func: Option<ir::FunctionID>,
    get_mem_func: Option<ir::FunctionID>,

    init_code: ir::InstructionList,

    metadata: ir::MetadataBuilder,
}

#[derive(Debug, Clone)]
struct BuiltinClassInfo {
    name: typ::Symbol,
    id: ir::TypeDefID,
    rtti: bool,
}

#[derive(Debug, Clone)]
pub struct LibraryRef {
    pub lib: Rc<ir::Library>,
    pub imported_funcs: HashMap<FunctionDeclKey, FunctionInstance>,
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
        refs: &'a [LibraryRef],
        opts: CodegenOpts
    ) -> Self {
        let builtin_classes = BUILTIN_CLASS_INFO
            .with(|class_infos| class_infos.to_vec());

        let metadata_refs = refs.iter().map(|r| r.lib.metadata.clone());

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
            references: refs,

            version,

            metadata,

            tags: Vec::new(),

            opts,
            root_ctx: src_metadata,

            type_cache,
            cached_types,

            flags_repr_types: BTreeMap::new(),
            
            functions: BTreeMap::new(),
            translated_funcs: LinkedHashMap::new(),

            function_types_by_sig: HashMap::new(),

            variables: BTreeMap::new(),
            variables_by_name: HashMap::new(),
            
            static_closures: BTreeMap::new(),
            
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
        for defined_type in self.root_ctx.defined_types() {
            // defined generic types need to be translated with their placeholders
            let translate_type = match &defined_type {
                typ::Type::Record(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    typ::Type::Record(Arc::new(sym.into_owned()))
                }

                typ::Type::Class(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    typ::Type::Class(Arc::new(sym.into_owned()))
                }

                typ::Type::Variant(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    typ::Type::Variant(Arc::new(sym.into_owned()))
                }

                typ::Type::Interface(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    typ::Type::Interface(Arc::new(sym.into_owned()))
                }

                _ => defined_type,
            };

            self.translate_type(&translate_type);
        }

        // ensure methods of all defined types are instantiated. they might already be instantiated
        // in another referenced library - we just need to ensure any un-referenced methods are
        // generated
        let all_defined_methods: Vec<_> = self.root_ctx
            .defined_types()
            .into_iter()
            .filter(|ty| {
                !ty.is_abstract() && !ty.contains_unresolved_params(self.root_ctx)
            })
            .flat_map(|ty| {
                let methods = ty.methods(self.root_ctx).unwrap();
                (0..methods.len())
                    .map(move |method_index| FunctionDeclKey::Method(MethodDeclKey {
                        self_ty: ty.clone(),
                        method_index,
                    }))
            })
            .chain(Primitive::ALL.iter()
                // ensure all primitive methods are instantiated too
                .flat_map(|primitive| {
                    self.root_ctx
                        .get_primitive_methods(*primitive)
                        .iter()
                        .enumerate()
                        .map(|(method_index, _)| FunctionDeclKey::Method(MethodDeclKey {
                            self_ty: typ::Type::from(*primitive),
                            method_index,
                        }))
                }))
            .collect();

        for method_key in all_defined_methods {
            self.instantiate_func(&method_key);
        }

        self.build_typeinfo();

        self.gen_static_closure_init();
        self.gen_static_type_init();

        gen_func_invokers(&mut self);

        let metadata = self.metadata.build();

        let ref_names: Vec<_> = self.references
            .iter()
            .map(|lib_ref| lib_ref.lib.name.clone())
            .collect();

        let mut lib = ir::Library::new(self.name, self.version, ref_names, metadata);
        lib.functions = self.functions;
        lib.init = self.init_code;
        lib.tags.extend(self.tags);

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
                .and_then(|init| init.expr.const_eval(&self.root_ctx))
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
            
            let internal_name = format!("{}.<init>", unit.ident);
            let debug_name = self.opts.debug.then(|| internal_name.clone());
            let identity = ir::FunctionIdentity::internal(internal_name);

            let init_sig = Rc::new(ir::FunctionSig {
                param_types: Vec::new(),
                result_type: ir::Type::Nothing
            });
            let init_func = ir::FunctionDef {
                body: unit_init,
                sig: init_sig.clone(),
                type_params: Vec::new(),
                debug_name,
            };

            let init_func_id = self.metadata.insert_func(identity, init_sig, false, []);
            self.functions.insert(init_func_id, ir::Function::Local(init_func));

            self.init_code.push(ir::Instruction::Call {
                function: ir::Ref::Global(ir::GlobalRef::func(init_func_id, [])).into(),
                args: Vec::new(),
                out: None,
            }, None);
        }

        for unit_decl in &unit.iface_section.decls {
            self.translate_unit_decl(&unit.ident, unit_decl);
        }

        for unit_decl in &unit.impl_section.decls {
            self.translate_unit_decl(&unit.ident, unit_decl);
        }
    }

    fn translate_unit_decl(&mut self, unit_ident: &IdentPath, unit_decl: &typ::ast::UnitDecl) {
        match unit_decl {
            typ::ast::UnitDecl::FunctionDecl { decl } => {
                self.translate_unit_func_decl(&unit_ident, decl);
            }

            typ::ast::UnitDecl::FunctionDef { def } => {
                self.translate_unit_func_decl(&unit_ident, &def.decl);
            }

            typ::ast::UnitDecl::Type { decl } => {
                for item in &decl.items {
                    self.translate_unit_type_decl(item);
                }
            }

            typ::ast::UnitDecl::Binding { .. } => {
                // bindings are handled separately
            }

            typ::ast::UnitDecl::Uses { .. } => {
                // uses items are only relevant for the frontend
            }
        };
    }

    fn translate_unit_type_decl(&mut self, decl: &typ::ast::TypeDeclItem) {
        match decl {
            typ::ast::TypeDeclItem::Struct(struct_decl) => {
                self.translate_type(&typ::Type::from_struct_type(struct_decl.name.clone(), struct_decl.kind));
            }

            typ::ast::TypeDeclItem::Interface(iface_decl) => {
                self.translate_type(&typ::Type::interface(iface_decl.name.clone()));
            }

            typ::ast::TypeDeclItem::Variant(variant_decl) => {
                self.translate_type(&typ::Type::variant(variant_decl.name.clone()));
            }

            typ::ast::TypeDeclItem::Alias(alias_type) => {
                let target_type = alias_type.target.ty();
                self.translate_type(target_type);

                create_alias_tag(self, &alias_type.name.full_path, target_type);
            }

            typ::ast::TypeDeclItem::Enum(enum_decl) => {
                self.translate_type(&typ::Type::Enum(Arc::new(enum_decl.name.full_path.clone())));
            }

            typ::ast::TypeDeclItem::Set(set_decl) => {
                let set_type = set_decl
                    .to_set_type(&self.root_ctx)
                    .unwrap_or_else(|err| panic!("translate_unit_type_decl: {err}"));

                self.translate_type(&typ::Type::Set(Arc::new(set_type)));
            }
        }
    }

    pub fn get_method(&self, ty: &typ::Type, index: usize) -> typ::ast::MethodDecl {
        let method =  ty.get_method(index, &self.root_ctx);
        
        method.unwrap_or_else(|e| {
            panic!("get_method: failed to get method {index} of {ty}: {e}")
        })
    }
    
    pub fn find_method_index(&self, ty: &typ::Type, name: &Ident, sig: &typ::FunctionSig) -> usize {
        let index = ty.find_method_index(name, sig, self.root_ctx)
            .ok()
            .unwrap_or_else(|| {
                let methods = ty
                    .methods(self.root_ctx)
                    .unwrap_or_else(|_| Vec::new());
                let method_count = methods.len();

                let method_list = ty
                    .methods(self.root_ctx)
                    .unwrap_or_else(|_| Vec::new())
                    .into_iter()
                    .map(|method_decl| format!("\n* {}", method_decl.func_decl))
                    .collect::<Vec<_>>()
                    .join("");

                panic!("find_method_index: no such method {} of type {} with sig {}\n{method_count} method(s) found{}", name, ty, sig, method_list)
            });

        index
    }

    pub fn find_struct_def(&self, name_path: &IdentPath, kind: StructKind) -> Option<&Arc<typ::ast::StructDecl>> {
        self.root_ctx.find_struct_def(name_path, kind).ok()
    }

    pub fn find_type_seq_support(&self, src_ty: &typ::Type) -> Option<TypeSequenceSupport> {
        TypeSequenceSupport::try_from_type(src_ty, &self.root_ctx).ok()
    }

    pub fn get_flags_repr_type(&mut self, bits: usize) -> FlagsReprType {
        if let Some(repr_type) = self.flags_repr_types.get(&bits) {
            return repr_type.clone();
        }

        let repr_type = FlagsReprType::build(self, bits);
        self.flags_repr_types.insert(bits, repr_type.clone());

        repr_type
    }
    
    pub fn translate_set_type(&mut self, set_type: &typ::SetType) -> SetFlagsType {
        let set_flags_type = SetFlagsType::translate(self, set_type);

        self.gen_type_info(&set_flags_type.struct_id.to_struct_type([]));
        self.gen_type_info(&set_flags_type.repr_type.repr_type());

        set_flags_type
    }

    pub fn add_tag(&mut self, tag: ir::TagInfo) {
        self.tags.push(tag);
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

    fn translate_unit_func_decl(
        &mut self,
        unit_name: &IdentPath,
        decl: &typ::ast::FunctionDecl,
    ) -> Option<FunctionInstance> {
        match &decl.name.context {
            typ::ast::FunctionDeclContext::FreeFunction => {
                let func_name = unit_name.clone().child(decl.name.ident.clone());
                let func_sig = decl.sig();

                let instance = self.translate_func(func_name, Arc::new(func_sig));
                Some(instance)
            }

            typ::ast::FunctionDeclContext::MethodDecl { enclosing_type } => {
                Some(self.translate_unit_method(enclosing_type, decl))
            }

            typ::ast::FunctionDeclContext::MethodDef { declaring_type } => {
                Some(self.translate_unit_method(declaring_type.ty(), decl))
            }
        }
    }

    fn translate_unit_method(
        &mut self,
        declaring_type: &typ::Type,
        decl: &typ::ast::FunctionDecl,
    ) -> FunctionInstance {
        let (method_index, _method_decl) = declaring_type
            .find_method(&decl.name.ident, &decl.sig(), &self.root_ctx)
            .ok()
            .unwrap_or_else(|| {
                panic!("translate_unit_method: failed to find method decl for {}", decl)
            });

        let self_ty = if let Some(type_args) = declaring_type.type_params() {
            let generic_args = type_args.clone().into_type_args();
            declaring_type.specialize(&generic_args, &self.root_ctx).unwrap().into_owned()
        } else {
            declaring_type.clone()
        };

        let func_key = FunctionDeclKey::Method(MethodDeclKey {
            self_ty,
            method_index,
        });

        let method_instance = self.instantiate_func(&func_key);

        // eprintln!("translate_unit_method: {decl} ({}.{method_index}) = {}", declaring_type, method_instance.id);
        method_instance
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
            .implemented_ifaces(&self.root_ctx)
            .unwrap_or_else(|err| {
                panic!("failed to retrieve implementation list for type {}: {}", src_self_ty, err)
            });

        for iface_ty in &ifaces {
            let iface_methods: Vec<_> = iface_ty
                .methods(&self.root_ctx)
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
        params: &impl typ::TypeParamContainer,
        args: &impl typ::TypeArgResolver
    ) -> Generic 
    where 
        Generic: typ::Specializable,
    {
        target.apply_type_args(params, args)
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
                self.translate_variant_type(name)
            }

            typ::Type::Record(name) => {
                self.translate_struct_type(name, StructKind::Record)
            },

            typ::Type::Class(name) => {
                self.translate_struct_type(name, StructKind::Class)
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
                self.translate_iface_type(iface_sym)
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
                let sig = translate_sig(func_sig, self);

                // values of function type will always be translated as closure references,
                // raw function pointers aren't used
                let closure_type = sig.into_closure_ptr_type();

                self.add_cached_type(src_ty.clone(), closure_type.clone());

                closure_type
            },
            
            typ::Type::Set(set_ty) => {
                let flags_ty = self.translate_set_type(set_ty);

                let ty = flags_ty.struct_id.to_struct_type([]);

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

    fn translate_variant_type(&mut self, name: &Arc<typ::Symbol>) -> ir::Type {
        let src_type = typ::Type::variant(name.clone());

        if let Some(cached) = self.type_cache.get(&src_type) {
            let ty = cached.clone();
            return ty;
        }

        // the definition uses the generic (unspecialized) type, which is
        // shared between all specializations
        let def_name = name.to_generic_name();
        let def_path = translate_name(&def_name, self);

        let def_id = match self.metadata.find_variant_def(&def_path) {
            Some((def_id, _)) => {
                def_id
            }

            None => {
                let src_def = self.root_ctx
                    .find_variant_def(&def_name.full_path)
                    .unwrap();

                let def_id = self.metadata.forward_declare_type(&def_path);

                let def_type = typ::Type::variant(def_name);
                let variant_type = def_id.to_variant_type(def_path.type_args.clone());

                self.add_cached_type(def_type.clone(), variant_type.clone());

                self.metadata.declare_type(def_id, &def_path);

                let def = translate_variant_def(&src_def, self);
                self.metadata.define_variant(def_id, def);

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
        name: &Arc<typ::Symbol>,
        kind: StructKind,
    ) -> ir::Type {
        let src_type = typ::Type::from_struct_type(name.clone(), kind);

        if let Some(cached) = self.type_cache.get(&src_type) {
            let ty = cached.clone();
            return ty;
        }

        let def_name = name.to_generic_name();
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
                let src_def = self.root_ctx
                    .find_struct_def(&name.full_path, kind)
                    .unwrap_or_else(|e| panic!("translate_struct_type: {}", e));

                let def_src_type = typ::Type::from_struct_type(def_name.clone(), kind);

                let def_id = self.metadata.forward_declare_type(&def_path);
                let def_type = type_ctor(def_id, &def_path.type_args);

                self.add_cached_type(def_src_type.clone(), def_type);
                self.metadata.declare_type(def_id, &def_path);

                let def = translate_struct_def(&src_def, self);
                self.metadata.define_struct(def_id, def);

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

    fn translate_iface_type(&mut self, src_name: &Arc<typ::Symbol>) -> ir::Type {
        let src_type = typ::Type::Interface(src_name.clone());

        if let Some(cached) = self.type_cache.get(&src_type) {
            let ty = cached.clone();
            return ty;
        }

        let src_def = if src_name.is_unspecialized_generic() {
            self.root_ctx
                .find_iface_def(&src_name.full_path)
                .unwrap_or_else(|err| panic!("translate_iface_type: {err}"))
                .clone()
        } else {
            self.root_ctx
                .instantiate_iface_def(src_name)
                .unwrap_or_else(|err| panic!("translate_iface_type: {err}"))
        };

        let iface_name = translate_name(&src_def.name, self);
        let decl_id = self.metadata.declare_iface(&iface_name);
        let iface_type = ir::Type::Object(ir::ObjectID::Interface(decl_id));

        self.add_cached_type(src_type.clone(), iface_type.clone());

        let def = translate_iface(&src_def, self);
        let iface_id = self.metadata.define_iface(def);
        assert_eq!(iface_id, decl_id);

        iface_type
    }

    pub fn find_func_ty(&self, sig: &typ::FunctionSig) -> Option<ir::TypeDefID> {
        self.function_types_by_sig.get(&sig).cloned()
    }

    pub fn find_global_var(&self, name_path: &IdentPath) -> Option<ir::VariableID> {
        self.variables_by_name.get(name_path).cloned()
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

            for (mut src_ty, ty) in populate_types.drain(0..) {
                done_types += 1;

                // automatically convert reference to unspecialized types into their generic forms
                if let TypeArgsResult::Unspecialized(param_list) = src_ty.type_args() {
                    let generic_args = param_list.clone().into_type_args();
                    src_ty = src_ty
                        .specialize(&generic_args, &self.root_ctx)
                        .unwrap_or_else(|err| {
                            panic!("build_typeinfo: {err}")
                        })
                        .into_owned();
                }
                
                // for any non-object types, ensure boxed versions exist in the type cache
                // for use with RTTI
                if !src_ty.is_object() 
                    && self.opts().rtti 
                    && let Ok(true) = src_ty.is_sized(&self.root_ctx)
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

        self.gen_type_info(&ty);

        // TODO: is there a better way to identify types defined in this library?
        // only update types where the original entry is not from a referenced metadata
        let Some(mut type_info) = self.metadata
            .metadata()
            .get_type_info(&ty)
            .map(|type_info_ref| type_info_ref.as_ref().clone())
        else {
            return;
        };

        type_info.name = Some(self.metadata.find_or_insert_string(&src_ty.to_string()));

        match &src_ty {
            typ::Type::Record(name) | typ::Type::Class(name) => {
                let name = name.as_ref().clone();
                let kind = src_ty.struct_kind().unwrap();

                let def = if name.is_unspecialized_generic() {
                    self.root_ctx
                        .find_struct_def(&name.full_path, kind)
                        .cloned()
                        .unwrap_or_else(|err| {
                            panic!("populate_runtime_type_info: missing struct def {}: {}", name.full_path, err)
                        })
                } else {
                    self.root_ctx
                        .instantiate_struct_def(&name, kind)
                        .unwrap_or_else(|err| {
                            panic!("populate_runtime_type_info: failed to instantiate struct {}: {}", name, err)
                        })
                };

                for (method_index, method) in def.methods().enumerate() {
                    let method_decl = &method.func_decl;

                    if method.is_published() && method_decl.name.type_params.is_none() {
                        let method_info = self.create_method_info(ty.clone(), &src_ty, method_index, false, method_decl);
                        type_info.methods.push(method_info);
                    }
                }
            }
            
            typ::Type::Interface(name) => {
                let def = self.root_ctx
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
            let method_key = FunctionDeclKey::Method(MethodDeclKey {
                self_ty: src_ty.clone(),
                method_index,
            });

            let method_instance = self.instantiate_func(&method_key);
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

    pub fn build_closure_instance(
        &mut self,
        func: &typ::ast::AnonymousFunctionDef,
    ) -> ClosureInstance {
        // this is the signature of the *function type* of the closure, not the signature of
        // the real method implementing the closure, which has an extra type-erased parameter
        // for the closure itself
        let src_virtual_sig = typ::FunctionSig::of_anonymous_func(func);
        let virtual_sig = Rc::new(translate_sig(&src_virtual_sig, self));

        // since we're translating a closure function, the sig needs to include the implicit
        // closure object parameter
        let sig = translate_sig(&func.sig(), self).into_closure_function_ptr_sig();

        let internal_name = "<anonymous function>".to_string();

        let debug_name = self.opts.debug.then(|| internal_name.clone());
        let identity = ir::FunctionIdentity::internal(internal_name);

        let id = self.metadata.insert_func(identity, sig, false, []);

        let closure_identity = ir::ClosureIdentity {
            sig: virtual_sig.clone(),
            id,
        };

        let closure_id = translate_closure_struct(
            closure_identity,
            &func.captures,
            self
        );

        let func_def = build_closure_function_def(self, &func, closure_id, &virtual_sig, debug_name);

        self.functions.insert(id, ir::Function::Local(func_def));

        ClosureInstance {
            func_instance: FunctionInstance {
                id,
                src_sig: src_virtual_sig,
                published: false,
            },
            sig: virtual_sig,
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
                (static_closure.identity.id == func.id).then_some(*id)
            })
        {
            return &self.static_closures[&existing_id];
        }

        // function reference closures can never have a capture list or type args
        let captures = LinkedHashMap::default();

        let virtual_sig = Rc::new(translate_sig(func.src_sig.as_ref(), self));

        let ir_func = self.functions
            .get(&func.id)
            .expect("function passed to build_function_closure_instance must have been previously translated")
            .clone();

        let closure_identity = ir::ClosureIdentity {
            sig: virtual_sig.clone(),
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
        closure_func_sig.param_types.insert(0, virtual_sig.to_closure_ptr_type());

        let internal_name = "<static closure function>".to_string();
        let identity = ir::FunctionIdentity::internal(internal_name);

        let thunk_id = self.metadata.insert_func(identity, closure_func_sig, false, []);
        let thunk_def = build_func_static_closure_def(self, func, &virtual_sig, &ir_func);

        self.functions.insert(thunk_id, ir::Function::Local(thunk_def));

        let closure = ClosureInstance {
            closure_id,
            sig: virtual_sig,
            func_instance: FunctionInstance {
                id: thunk_id,
                src_sig: func.src_sig.clone(),
                published: false,
            },
        };

        let static_closure = self.build_static_closure(closure);
        let static_closure_id = static_closure.id;

        &self.static_closures[&static_closure_id]
    }

    pub fn build_static_closure(&mut self, closure: ClosureInstance) -> &ir::StaticClosure {
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

        let id = self.metadata.new_variable(None, closure.sig.to_closure_ptr_type());
        let instance = build_static_closure_impl(closure, id, self);

        self.static_closures.insert(id, instance);
        self.static_closures.get(&id).unwrap()
    }

    /// Add static closure init function calls at top of init block
    fn gen_static_closure_init(&mut self) {
        let mut static_closures_init = ir::InstructionList::new();
        for (_id, static_closure) in &self.static_closures {
            static_closures_init.push(ir::Instruction::Call {
                function: ir::GlobalRef::func(static_closure.init_func, []).to_ref().value(),
                args: Vec::new(),
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
                out: None,
                function: ir::GlobalRef::func(tag_init_func, []).to_ref().value(),
            }, None);
        };

        instructions.append(&mut self.init_code);
        self.init_code = instructions;
    }
}

fn gen_closure_runtime_type(lib: &mut LibraryBuilder, closure_id: ir::TypeDefID) {
    let type_id = ir::TypeRef::new(closure_id, []);
    let closure_class_ty = type_id.to_class_object_type();
    let closure_weak_ty = type_id.to_weak_class_object_type();

    let runtime_type = lib.gen_type_info(&closure_class_ty);
    let mut runtime_type = (*runtime_type).clone();

    let mut weak_runtime_type = lib.gen_type_info(&closure_weak_ty)
        .as_ref()
        .clone();

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
        .expect("gen_class_runtime_type: resource class of translated class type was not a struct");

    lib.gen_type_info(&class_id.to_class_object_type());
    lib.gen_type_info(&class_id.to_weak_class_object_type());
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

        // TODO: there should be a better way to know which functions belong to this library
        // skip functions not defined in this lib e.g. imported functions
        let Some(lib_func) = lib.functions.get(&func.id) else {
            continue;
        };

        let target_debug_name = lib_func.debug_name();
        let target_name = match target_debug_name {
            Some(name) => format!("{name} ({})", func.id),
            None => format!("{}", func.id),
        };

        let internal_name = format!("generated invoker for {}", target_name);
        let debug_name = lib.opts.debug.then(|| internal_name.clone());
        let identity = ir::FunctionIdentity::internal(internal_name);

        // the source sig may be generic, so look up the translated sig rather than attempting
        // to translate it without a generic context
        let sig = lib_func.sig().clone();

        let invoker_id = lib.metadata_mut().insert_func(identity, invoker_sig.clone(), false, []);

        let mut builder = IRBuilder::new(lib);
        builder.bind_result(sig.result_type.clone());

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
            sig: Rc::new(invoker_sig.clone()),
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