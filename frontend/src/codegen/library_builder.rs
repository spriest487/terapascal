mod build_functions;

pub use self::build_functions::*;

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
use crate::codegen::CodegenOpts;
use crate::codegen::FunctionInstance;
use crate::codegen::SetFlagsType;
use crate::codegen::*;
use crate::ir;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::ENUM_ORD_TYPE;
use crate::typ::builtin_funcinfo_name;
use crate::typ::builtin_methodinfo_name;
use crate::typ::builtin_string_name;
use crate::typ::builtin_string_type;
use crate::typ::builtin_typeinfo_name;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::Def;
use ir::InstructionBuilder as _;
use linked_hash_map::LinkedHashMap;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::mem;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::version::Version;
use typ::*;

#[derive(Debug)]
pub struct LibraryBuilder<'a> {
    name: String,
    version: Version,

    references: &'a [LibraryRef],

    tags: Vec<ir::TagInfo>,

    root_ctx: &'a Context,

    opts: CodegenOpts,

    pub(super) defined_types: HashSet<Type>,
    type_cache: LinkedHashMap<Type, ir::Type>,
    cached_types: LinkedHashMap<ir::Type, Type>,

    // key is size (bits)
    flags_repr_types: BTreeMap<usize, FlagsReprType>,

    translated_funcs: LinkedHashMap<FunctionDeclKey, FunctionInstance>,

    functions: BTreeMap<ir::FunctionID, ir::Function>,
    function_types_by_sig: HashMap<FunctionSig, ir::TypeDefID>,

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
    name: Symbol,
    id: ir::TypeDefID,
    rtti: bool,
}

#[derive(Debug, Clone)]
pub struct LibraryRef {
    pub lib: Rc<ir::Library>,

    pub imported_funcs: HashMap<FunctionDeclKey, FunctionInstance>,
    pub imported_namespaces: HashSet<IdentPath>,
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
        src_metadata: &'a Context,
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
                let src_type = Type::class(class_info.name.clone());
                let result_type = class_info.id.to_class_ptr_type([]);

                (src_type, result_type)
            })
            .collect();
        
        let cached_types = type_cache
            .iter()
            .map(|(src_ty, ty)| (ty.clone(), src_ty.clone()))
            .collect();

        let mut defined_types = HashSet::new();

        if opts.no_system {
            for primitive in Primitive::ALL {
                defined_types.insert(Type::Primitive(primitive));
            }

            defined_types.insert(Type::Any);
            defined_types.insert(builtin_string_type());
        }

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

            defined_types,

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
                Type::Record(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    Type::Record(Arc::new(sym.into_owned()))
                }

                Type::Class(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    Type::Class(Arc::new(sym.into_owned()))
                }

                Type::Variant(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    Type::Variant(Arc::new(sym.into_owned()))
                }

                Type::Interface(sym) if let Some(params) = &sym.type_params => {
                    let type_args = params.clone().into_type_args();
                    let sym = sym.specialize(&type_args, &self.root_ctx).unwrap();
                    Type::Interface(Arc::new(sym.into_owned()))
                }

                _ => defined_type,
            };

            self.translate_type(&translate_type);
        }

        // ensure methods of all defined types are instantiated. they might already be instantiated
        // in another referenced library - we just need to ensure any un-referenced methods are
        // generated
        let all_defined_methods: Vec<_> = self.defined_types
            .iter()
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
                            self_ty: Type::from(*primitive),
                            method_index,
                        }))
                }))
            .collect();

        for method_key in all_defined_methods {
            self.instantiate_func(&method_key);
        }

        self.build_defined_type_info();

        self.gen_static_closure_init();

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

    pub fn translate_unit(&mut self, unit: &ast::Unit) {
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

                self.metadata.new_const(binding_path, value, binding_ty.clone(), []);
            }
        }

        for (_, var) in unit.binding_items(BindingDeclKind::Var) {
            let var_ty = self.translate_type(&var.ty);

            for ident in &var.idents {
                let var_name = unit.ident.clone().child(ident.clone());
                let var_path = ir::NamePath::from_ident_path(&var_name, None);

                let id = self.metadata.new_variable(Some(var_path), var_ty.clone(), []);

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
            let identity = ir::FunctionIdentity::internal(internal_name);

            let init_sig = Rc::new(ir::FunctionSig {
                param_types: Vec::new(),
                result_type: ir::Type::Nothing
            });
            let init_func = ir::FunctionDef {
                body: unit_init,
                sig: init_sig.clone(),
                type_params: Vec::new(),
            };

            let init_func_id = self.metadata.insert_func(identity, [], ir::Type::Nothing, false, []);
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

    fn translate_unit_decl(&mut self, unit_ident: &IdentPath, unit_decl: &ast::UnitDecl) {
        match unit_decl {
            ast::UnitDecl::FunctionDecl { decl } => {
                self.translate_unit_func_decl(&unit_ident, decl);
            }

            ast::UnitDecl::FunctionDef { def } => {
                self.translate_unit_func_decl(&unit_ident, &def.decl);
            }

            ast::UnitDecl::Type { decl } => {
                for item in &decl.items {
                    self.translate_unit_type_decl(item);
                }
            }

            ast::UnitDecl::Binding { .. } => {
                // bindings are handled separately
            }

            ast::UnitDecl::Uses { .. } => {
                // uses items are only relevant for the frontend
            }
        };
    }

    fn translate_unit_type_decl(&mut self, decl: &ast::TypeDeclItem) {
        match decl {
            ast::TypeDeclItem::Struct(struct_decl) => {
                let name = struct_decl.name.clone();
                let struct_type = Type::from_struct_type(name, struct_decl.kind);

                self.translate_type(&struct_type);
            }

            ast::TypeDeclItem::Interface(iface_decl) => {
                self.translate_type(&Type::interface(iface_decl.name.clone()));
            }

            ast::TypeDeclItem::Variant(variant_decl) => {
                self.translate_type(&Type::variant(variant_decl.name.clone()));
            }

            ast::TypeDeclItem::Alias(alias_type) => {
                let target_type = alias_type.target.ty();
                self.translate_type(target_type);

                create_alias_tag(self, &alias_type.name.full_path, target_type);
            }

            ast::TypeDeclItem::Enum(enum_decl) => {
                self.translate_type(&Type::Enum(Arc::new(enum_decl.name.full_path.clone())));
            }

            ast::TypeDeclItem::Set(set_decl) => {
                let set_info = set_decl
                    .to_set_type(&self.root_ctx)
                    .unwrap_or_else(|err| panic!("translate_unit_type_decl: {err}"));
                let set_type = Type::Set(Arc::new(set_info));

                self.translate_type(&set_type);

                self.defined_types.insert(set_type);
            }
        }
    }

    pub fn get_method(&self, ty: &Type, index: usize) -> ast::MethodDecl {
        let method =  ty.get_method(index, &self.root_ctx);
        
        method.unwrap_or_else(|e| {
            panic!("get_method: failed to get method {index} of {ty}: {e}")
        })
    }
    
    pub fn find_method_index(&self, ty: &Type, name: &Ident, sig: &FunctionSig) -> usize {
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

    pub fn find_struct_def(&self, name_path: &IdentPath, kind: StructKind) -> Option<&Arc<ast::StructDecl>> {
        self.root_ctx.find_struct_def(name_path, kind).ok()
    }

    pub fn find_type_seq_support(&self, src_ty: &Type) -> Option<TypeSequenceSupport> {
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
    
    pub fn translate_set_type(&mut self, set_type: &Arc<SetDef>) -> SetFlagsType {
        let set_flags_type = SetFlagsType::translate(self, set_type);

        set_flags_type
    }

    pub(crate) fn build_enum_def(&mut self, enum_name: &IdentPath) -> ir::TypeDefID {
        let find_def = self.root_ctx.find_def(enum_name, &DefKey::Unique);

        let Some(Def::Enum(enum_def)) = find_def else {
            panic!("build_enum_def: missing definition for {enum_name}");
        };

        let Some(namespace) = enum_name.parent() else {
            panic!("build_enum_def: invalid enum path {enum_name}");
        };

        let namespace_path = ir::NamePath::from_ident_path(&namespace, []);

        let ord_type = self.translate_type(&ENUM_ORD_TYPE);

        let enum_type_path = namespace_path.clone().child(enum_name.last().as_str());

        // enum types have no actual definition but also only get declared once (below) by the
        // library they're in, so assume if there's even a forward decl, the constants are
        // stored in another library
        if let Some(id) = self.metadata.find_type_decl(&enum_type_path) {
            return id;
        }

        let enum_member_tag_id = match EnumMemberTagInfo::find_in_metadata(&self.metadata) {
            Some(info) => {
                info.class_id
            },
            None => {
                self.metadata.forward_declare_type(&ir::NamePath::new(
                    [SYSTEM_UNIT_NAME.to_string()],
                    ENUM_MEMBER_TAG_NAME,
                ))
            }
        };

        // insert a forward type def for the enum type itself - this type will never actually
        // be defined beyond giving it this name, but it means we can refer to it as a type
        let enum_type_id = self.metadata.forward_declare_type(&enum_type_path);
        let enum_def_type = enum_type_id.to_struct_type([]);
        let enum_type_ref = ir::GlobalRef::StaticTypeInfo(Rc::new(enum_def_type.clone()));

        for item in &enum_def.items {
            let item_path = namespace_path.clone().child(item.ident.as_str());

            // typechecked enum items must have statically-known values
            let Some(const_val) = item.annotation.as_const() else {
                panic!("build_enum_def: enum item {item_path} does not have a const value");
            };

            let value = literal_to_val(&const_val.value, &ord_type, self);

            let mut member_tag = ir::TagInfo::new(enum_member_tag_id);
            member_tag.fields.insert(ENUM_MEMBER_TAG_TYPE_FIELD, enum_type_ref.clone().to_ref().value());

            self.metadata_mut().new_const(item_path, value, ord_type.clone(), [member_tag]);
        }

        // custom typeinfo because it's not a real type def
        let runtime_name = enum_type_path.to_pretty_string(self.metadata());
        let runtime_name_id = self.metadata.find_or_insert_string(&runtime_name);

        self.metadata.insert_type_info(enum_def_type, ir::TypeInfo {
            name: runtime_name_id,
            flags: ir::TYPE_FLAG_VALUE,
            debug_name: Some(runtime_name),
            methods: Vec::new(),
        });

        enum_type_id
    }

    pub fn add_tag(&mut self, tag: ir::TagInfo) {
        self.tags.push(tag);
    }

    pub fn translate_tag_groups(&mut self, tags: &[ast::Tag]) -> Vec<ir::TagInfo> {
        // most groups will probably contain 1 tag
        let mut result = Vec::with_capacity(tags.len());
        
        for group in tags {
            result.extend(self.translate_tags(&group.items));
        }
        
        result
    }

    pub fn translate_tags(&mut self, tags: &[ast::TagItem]) -> Vec<ir::TagInfo> {
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
                    Value::Const(const_val) => {
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
        decl: &ast::FunctionDecl,
    ) -> Option<FunctionInstance> {
        match &decl.name.context {
            ast::FunctionDeclContext::FreeFunction => {
                let func_name = unit_name.clone().child(decl.name.ident.clone());
                let func_sig = decl.sig();

                let instance = self.translate_func(func_name, Arc::new(func_sig));
                Some(instance)
            }

            ast::FunctionDeclContext::MethodDecl { enclosing_type } => {
                Some(self.translate_unit_method(enclosing_type, decl))
            }

            ast::FunctionDeclContext::MethodDef { declaring_type } => {
                Some(self.translate_unit_method(declaring_type.ty(), decl))
            }
        }
    }

    fn translate_unit_method(
        &mut self,
        declaring_type: &Type,
        decl: &ast::FunctionDecl,
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

    pub fn find_iface_decl(&mut self, iface_name: &Symbol) -> Option<ir::InterfaceID> {
        let name = translate_name(iface_name, self);

        self.metadata
            .interface_defs()
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
    fn gen_iface_impls(&mut self, src_self_ty: &Type, self_ty: &ir::Type) {
        let ifaces = src_self_ty
            .implemented_ifaces(&self.root_ctx)
            .unwrap_or_else(|err| {
                panic!("failed to retrieve implementation list for type {}: {}", src_self_ty, err)
            });

        for src_iface_type in &ifaces {
            let iface_methods: Vec<_> = src_iface_type
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
                    iface_ty: src_iface_type.clone(),
                    iface_method_index,

                    impl_method: MethodDeclKey {
                        self_ty: src_self_ty.clone(),
                        method_index: impl_index,
                    },
                };

                self.instantiate_func(&FunctionDeclKey::VirtualMethod(virtual_key));
            }

            let iface_type = self.translate_type(src_iface_type);
            let Some(iface_ref) = iface_type.as_iface().cloned() else {
                panic!("gen_iface_impls: interface type {} did not translate to an interface", src_iface_type);
            };

            self.metadata_mut().declare_iface_impl(iface_ref, self_ty.clone());
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
    
    fn add_cached_type(&mut self, src_ty: Type, ty: ir::Type) {
        self.type_cache.insert(src_ty.clone(), ty.clone());
        self.cached_types.insert(ty.clone(), src_ty.clone());
    }

    pub fn translate_type(&mut self, src_ty: &Type) -> ir::Type {
        if let Some(cached) = self.type_cache.get(&src_ty) {
            let ty = cached.clone();
            return ty;
        }

        // instantiate types which may contain generic params
        let ty = match &src_ty {
            Type::Variant(name) => {
                self.translate_variant_type(name)
            }

            Type::Record(name) => {
                self.translate_struct_type(name, StructKind::Record)
            },

            Type::Class(name) => {
                self.translate_struct_type(name, StructKind::Class)
            }
            
            Type::Weak(weak_ty) => {
                let ty = match self.translate_type(weak_ty) {
                    ir::Type::Object(id) => ir::Type::WeakObject(id),
                    other => unreachable!("only RC class types can be weak, found: {}", other),
                };

                self.add_cached_type(src_ty.clone(), ty.clone());
                
                ty
            }

            Type::Interface(iface_sym) => {
                self.translate_iface_type(iface_sym)
            },

            Type::Array(array_ty) => {
                let elem_ty = self.translate_type(&array_ty.element_ty);
                let ty = elem_ty.array(array_ty.dim);

                self.add_cached_type(src_ty.clone(), ty.clone());
                ty
            },

            Type::DynArray(element) => {
                let ty = self.translate_type(element).dyn_array();

                self.add_cached_type(src_ty.clone(), ty.clone());

                ty
            },

            Type::Function(func_sig) => {
                let sig = translate_sig(func_sig, self);

                // values of function type will always be translated as closure references,
                // raw function pointers aren't used
                let closure_type = sig.into_closure_ptr_type();

                self.add_cached_type(src_ty.clone(), closure_type.clone());

                closure_type
            },
            
            Type::Set(set_ty) => {
                let flags_ty = self.translate_set_type(set_ty);

                let ty = flags_ty.struct_id.to_struct_type([]);

                self.add_cached_type(src_ty.clone(), ty.clone());
                
                ty
            }

            Type::Nothing => {
                self.add_cached_type(src_ty.clone(), ir::Type::Nothing);
                ir::Type::Nothing
            },

            Type::Primitive(Primitive::Boolean) => {
                self.add_cached_type(src_ty.clone(), ir::Type::Bool);
                ir::Type::Bool
            },

            Type::Primitive(Primitive::Int8) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I8);
                ir::Type::I8
            },
            Type::Primitive(Primitive::UInt8) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U8);
                ir::Type::U8
            },
            Type::Primitive(Primitive::Int16) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I16);
                ir::Type::I16
            },
            Type::Primitive(Primitive::UInt16) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U16);
                ir::Type::U16
            },
            Type::Primitive(Primitive::Int32) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I32);
                ir::Type::I32
            },
            Type::Primitive(Primitive::UInt32) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U32);
                ir::Type::U32
            },
            Type::Primitive(Primitive::Int64) => {
                self.add_cached_type(src_ty.clone(), ir::Type::I64);
                ir::Type::I64
            },
            Type::Primitive(Primitive::UInt64) => {
                self.add_cached_type(src_ty.clone(), ir::Type::U64);
                ir::Type::U64
            },
            Type::Primitive(Primitive::NativeInt) => {
                self.add_cached_type(src_ty.clone(), ir::Type::ISize);
                ir::Type::ISize
            },
            Type::Primitive(Primitive::NativeUInt) => {
                self.add_cached_type(src_ty.clone(), ir::Type::USize);
                ir::Type::USize
            },

            Type::Primitive(Primitive::Real32) => {
                self.add_cached_type(src_ty.clone(), ir::Type::F32);
                ir::Type::F32
            },
            Type::Primitive(Primitive::Real64) => {
                self.add_cached_type(src_ty.clone(), ir::Type::F64);
                ir::Type::F64
            },

            Type::Primitive(Primitive::Pointer) => {
                self.add_cached_type(src_ty.clone(), ir::Type::Nothing.ptr());
                ir::Type::Nothing.ptr()
            },

            Type::Any => {
                self.add_cached_type(src_ty.clone(), ir::ANY_TYPE);
                ir::ANY_TYPE
            }

            Type::Pointer(inner) => {
                let inner_type = self.translate_type(inner);
                let ptr_type = inner_type.ptr();
                self.add_cached_type(src_ty.clone(), ptr_type.clone());
                ptr_type
            }

            Type::Box(inner) => {
                let inner_type = self.translate_type(inner);
                let boxed_type = inner_type.boxed();
                self.add_cached_type(src_ty.clone(), boxed_type.clone());
                boxed_type
            }

            Type::Enum(name) => {
                self.build_enum_def(name);

                let ord_type = self.translate_type(&ast::ENUM_ORD_TYPE);
                self.add_cached_type(src_ty.clone(), ord_type.clone());
                ord_type
            }

            Type::GenericParam(param) => {
                ir::Type::Generic(Rc::new(param.name.name.to_string()))
            }

            Type::MethodSelf => {
                panic!("translate_type: unresolved Self type")
            }
        };

        ty
    }

    fn translate_variant_type(&mut self, name: &Arc<Symbol>) -> ir::Type {
        let src_type = Type::variant(name.clone());

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

                let def_src_type = Type::variant(def_name);
                let variant_type = def_id.to_variant_type(def_path.type_args.clone());

                self.add_cached_type(def_src_type.clone(), variant_type.clone());

                self.metadata.declare_type(def_id, &def_path);

                let def = translate_variant_def(&src_def, self);
                self.metadata.define_variant(def_id, def);

                self.defined_types.insert(def_src_type);

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
        name: &Arc<Symbol>,
        kind: StructKind,
    ) -> ir::Type {
        let src_type = Type::from_struct_type(name.clone(), kind);

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

                let def_src_type = Type::from_struct_type(def_name.clone(), kind);

                let def_id = self.metadata.forward_declare_type(&def_path);
                let def_type = type_ctor(def_id, &def_path.type_args);

                self.add_cached_type(def_src_type.clone(), def_type.clone());
                self.metadata.declare_type(def_id, &def_path);

                let def = translate_struct_def(&src_def, self);
                self.metadata.define_struct(def_id, def);

                self.gen_type_info(&def_type, &def_src_type.to_string());

                self.defined_types.insert(def_src_type);

                def_id
            }
        };

        // if this instance of the type is the same as the definition type (non generic, or
        // referenced via its definition name),
        let instance_path = translate_name(name, self);
        if instance_path == def_path {
            return type_ctor(def_id, &def_path.type_args);
        }

        let instance_type = type_ctor(def_id, &instance_path.type_args);
        self.add_cached_type(src_type.clone(), instance_type.clone());

        instance_type
    }

    fn translate_iface_type(&mut self, src_name: &Arc<Symbol>) -> ir::Type {
        let src_type = Type::Interface(src_name.clone());

        if let Some(cached) = self.type_cache.get(&src_type) {
            let ty = cached.clone();
            return ty;
        }

        let def_name = Arc::new(src_name.to_generic_name());
        let def_path = translate_name(&def_name, self);
        let instance_path = translate_name(src_name, self);

        match self.metadata.find_iface_def(&def_path) {
            Some(id) => {
                let instance_type = id.to_interface_type(instance_path.type_args.clone());

                self.add_cached_type(src_type.clone(), instance_type.clone());
                instance_type
            }

            None => {
                let decl_id = self.metadata.declare_iface(&def_path);

                let src_def_type = Type::interface(src_name.clone());
                let instance_type = decl_id.to_interface_type(instance_path.type_args.clone());

                self.add_cached_type(src_def_type.clone(), instance_type.clone());
                self.defined_types.insert(src_def_type);

                let src_def = self.root_ctx
                    .instantiate_iface_def(&def_name)
                    .unwrap_or_else(|err| panic!("translate_iface_type: {err}"));

                let def = translate_iface(decl_id, &src_def, self);
                let def_id = self.metadata.define_iface(def);
                assert_eq!(def_id, decl_id);

                instance_type
            }
        }
    }

    pub fn find_func_ty(&self, sig: &FunctionSig) -> Option<ir::TypeDefID> {
        self.function_types_by_sig.get(&sig).cloned()
    }

    pub fn find_global_var(&self, name_path: &IdentPath) -> Option<ir::VariableID> {
        self.variables_by_name.get(name_path).cloned()
    }

    // get or generate runtime type for a given type, which contains the function IDs etc
    // used for RC operations at runtime. the rest of the RTTI info will be filled in later 
    // in a separate pass
    pub fn gen_type_info(&mut self, ty: &ir::Type, name: &str) -> Rc<ir::TypeInfo> {
        if let Some(existing) = self.metadata.get_type_info(&ty) {
            return existing;
        }

        assert!(self.metadata().is_defined(ty), "gen_runtime_type: type {} ({:?}) is not defined yet", self.metadata().pretty_type_name(ty), ty);
        
        let flags = ir::TypeInfo::type_runtime_flags(ty, self.metadata());

        // type names and methods will be added after codegen
        let name_string = self.metadata.find_or_insert_string(name);

        let mut rtti = ir::TypeInfo::new(name_string, flags);

        if self.opts.debug && self.opts.rtti {
            rtti.debug_name = Some(self.metadata().pretty_type_name(&ty).into_owned());
        }

        self.metadata.insert_type_info(ty.clone(), rtti)
    }

    fn build_defined_type_info(&mut self) {
        if !self.opts.rtti {
            return;
        }

        // generate related box and dyn array type info for all types defined in this library
        for defined_type in self.defined_types.clone() {
            let Some(ir_type) = self.type_cache.get(&defined_type).cloned() else {
                // should be unreachable
                continue;
            };

            self.gen_type_info(&ir_type, &defined_type.to_string());

            self.gen_iface_impls(&defined_type, &ir_type);
            self.populate_method_info(defined_type, ir_type);
        }
    }

    fn populate_method_info(&mut self, src_ty: Type, ty: ir::Type) {
        if !self.opts.rtti {
            return;
        }

        // only update types where the original entry is not from a referenced metadata
        let Some(mut type_info) = self.metadata
            .metadata()
            .get_type_info(&ty)
            .map(|type_info_ref| type_info_ref.as_ref().clone())
        else {
            return;
        };

        match &src_ty {
            Type::Record(name) | Type::Class(name) => {
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
            
            Type::Interface(name) => {
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
        src_ty: &Type,
        method_index: usize,
        is_abstract: bool,
        decl: &ast::FunctionDecl,
    ) -> ir::MethodInfo {
        let tags = self.translate_tag_groups(&decl.tags);
        
        let method_name = decl.name.ident.as_str();
        let method_name_id = self.metadata.find_or_insert_string(method_name);

        let params = decl
            .params()
            .map(|(param, _)| {
                // in RTTI, "self" params for interfaces become untyped pointers
                let mut param_ty = match param.ty.ty() {
                    Type::MethodSelf => ir::Type::Nothing.ptr(),
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
        func: &ast::AnonymousFunctionDef,
    ) -> ClosureInstance {
        // this is the signature of the *function type* of the closure, not the signature of
        // the real method implementing the closure, which has an extra type-erased parameter
        // for the closure itself
        let src_virtual_sig = FunctionSig::of_anonymous_func(func);
        let virtual_sig = Rc::new(translate_sig(&src_virtual_sig, self));

        // since we're translating a closure function, the sig needs to include the implicit
        // closure object parameter
        let closure_sig = translate_sig(&func.sig(), self).into_closure_function_ptr_sig();

        let closure_params: Vec<_> = closure_sig.param_types
            .iter()
            .map(|ty| ir::FunctionParamInfo::new(ty.clone()))
            .collect();

        let internal_name = format!("<anonymous {}>", closure_sig.to_pretty_string(self.metadata()));
        let identity = ir::FunctionIdentity::internal(internal_name.clone());

        let id = self.metadata.insert_func(identity, closure_params, closure_sig.result_type.clone(), false, []);

        let closure_identity = ir::ClosureIdentity {
            sig: virtual_sig.clone(),
            id,
        };

        let closure_id = translate_closure_struct(
            closure_identity,
            &func.captures,
            self
        );

        let func_def = build_closure_function_def(self, &func, closure_id, &virtual_sig, Some(internal_name));

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
        let closure_sig = virtual_sig.to_closure_function_ptr_sig();

        let internal_name = "<static closure function>".to_string();
        let identity = ir::FunctionIdentity::internal(internal_name);

        let closure_params: Vec<_> = closure_sig.param_types
            .iter()
            .map(|ty| ir::FunctionParamInfo::new(ty.clone()))
            .collect();

        let thunk_id = self.metadata.insert_func(identity, closure_params, closure_sig.result_type.clone(), false, []);
        let thunk_def = build_func_static_closure_def(self, func, &virtual_sig, func.id);

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

        let id = self.metadata.new_variable(None, closure.sig.to_closure_ptr_type(), []);
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

        let target_name = match lib.metadata.func_desc(func.id) {
            Some(name) => format!("{name} ({})", func.id),
            None => format!("{}", func.id),
        };

        let internal_name = format!("generated invoker for {}", target_name);
        let identity = ir::FunctionIdentity::internal(internal_name);

        // the source sig may be generic, so look up the translated sig rather than attempting
        // to translate it without a generic context
        let sig = lib_func.sig().clone();

        let invoker_params: Vec<_> = invoker_sig.param_types
            .iter()
            .map(|ty| ir::FunctionParamInfo::new(ty.clone()))
            .collect();

        let invoker_id = lib.metadata_mut().insert_func(identity, invoker_params, invoker_sig.result_type.clone(), false, []);

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