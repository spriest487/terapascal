mod function;
mod stmt;
mod expr;
mod string_lit;
mod ty_def;
mod array;
mod boxed;
mod builtin;
mod type_map;
mod rtti;

pub use self::array::*;
pub use self::boxed::*;
pub use self::expr::*;
pub use self::function::*;
pub use self::stmt::*;
pub use self::ty_def::*;
use crate::c::rtti::FuncInfo;
use crate::c::rtti::TypeInfo;
use crate::c::string_lit::StringLiteral;
use crate::c::string_lit::StringLiteralKey;
use crate::c::type_map::ArraySig;
use crate::c::type_map::TypeID;
use crate::ir;
use crate::Options;
use bimap::BiBTreeMap;
use bimap::BiHashMap;
use std::borrow::Cow;
use std::collections::hash_map::HashMap;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use terapascal_ir::MetadataSource;
use topological_sort::TopologicalSort;

pub struct Unit<'a> {
    pub metadata: &'a ir::Metadata,

    functions: Vec<FunctionDef>,

    function_refs: HashMap<ir::FunctionRef, FunctionInstance>,
    function_types: BTreeMap<ir::FunctionID, Rc<ir::FunctionSig>>,

    func_instances: BTreeMap<FuncInstanceID, FunctionName>,

    types: BiHashMap<TypeID, ir::Type>,
    c_types: BTreeMap<TypeID, Type>,

    dyn_array_types_by_element: BiBTreeMap<TypeID, DynArrayTypeID>,
    box_types_by_element: BiBTreeMap<TypeID, BoxTypeID>,

    type_info: BTreeMap<TypeID, TypeInfo>,

    static_array_types: HashMap<ArraySig, TypeID>,

    type_defs: BTreeMap<TypeID, TypeDef>,
    type_defs_by_name: HashMap<TypeDefName, TypeID>,
    type_defs_order: TopologicalSort<TypeDefName>,
    
    ffi_funcs: Vec<FfiFunction>,

    global_vars: Vec<GlobalVar>,

    classes: Vec<Class>,
    ifaces: Vec<Interface>,

    string_literals: HashMap<StringLiteralKey, StringLiteral>,

    tag_arrays: HashMap<ir::TagLocation, usize>,

    object_array_id: DynArrayTypeID,

    opts: Options,

    runtime_func_infos: Vec<FuncInfo>,
}

impl<'a> Unit<'a> {
    pub fn new(metadata: &'a ir::Metadata, opts: Options) -> Self {
        let string_literals = metadata
            .strings()
            .map(|(id, str)| (
                StringLiteralKey::StringID(id), 
                StringLiteral::from(str.to_string())))
            .collect();

        let tag_arrays = metadata
            .all_tags()
            .map(|(loc, tags)| (loc, tags.len()))
            .collect();

        let mut unit = Unit {
            metadata,

            types: BiHashMap::new(),
            c_types: BTreeMap::new(),

            type_info: BTreeMap::new(),

            dyn_array_types_by_element: BiBTreeMap::new(),
            box_types_by_element: BiBTreeMap::new(),
            static_array_types: HashMap::new(),

            type_defs: BTreeMap::new(),
            type_defs_by_name: HashMap::new(),
            type_defs_order: TopologicalSort::new(),

            functions: Vec::new(),
            function_types: BTreeMap::new(),
            function_refs: HashMap::new(),
            func_instances: BTreeMap::new(),

            ffi_funcs: Vec::new(),
            
            global_vars: Vec::new(),

            string_literals,

            classes: Vec::new(),
            ifaces: Vec::new(),

            opts,

            runtime_func_infos: Vec::new(),
            
            tag_arrays,
            
            // temp value we're about to replace
            object_array_id: DynArrayTypeID(usize::MAX),
        };
        
        unit.create_named_string_lit(GlobalName::InvokeArgsError, "function invoked with invalid argument count");

        unit.object_array_id = unit.translate_dyn_array_type(&ir::ANY_TYPE).1;

        let system_funcs = builtin::system_funcs();

        for (pas_name, c_name, _return_ty, _params) in &system_funcs {
            let func_name = ir::FunctionName::new([
                "System".to_string(),
            ], pas_name.to_string());

            // if a function isn't used then it won't be included in the metadata
            if let Some(func_id) = metadata.find_function(&func_name) {
                unit.add_builtin_func(func_id, *c_name);
            }
        }

        for (iface_id, iface_def) in metadata.interface_defs() {
            unit.translate_type(&iface_id.to_interface_type(iface_def.name.type_args.clone()));
        }

        for (func_id, func_info) in metadata.functions() {
            let invoker = match func_info.invoker {
                Some(invoker_id) => {
                    let invoker_key = ir::FunctionRef::new(invoker_id);
                    let invoker_id = unit.translate_func_ref(&invoker_key);
                    Some(invoker_id.name)
                }
                None => None,
            };

            let name = func_info.runtime_name;

            unit.runtime_func_infos.push(FuncInfo {
                name,
                id: func_id,
                invoker,
            });
        }

        unit
    }

    pub fn pretty_type(&self, ir_ty: &ir::Type) -> Cow<'_, str> {
        match self.metadata.get_type_info(ir_ty).map(|typeinfo| typeinfo.name) {
            Some(name_id) => {
                let key = StringLiteralKey::StringID(name_id);
                let name = &self.string_literals[&key];

                Cow::Borrowed(name.as_str())
            },

            None => {
                Cow::Owned(ir_ty.to_pretty_string(self.metadata))
            },
        }
    }

    pub fn pretty_name(&self, name_path: &ir::NamePath) -> String {
        name_path.to_pretty_string(self.metadata)
    }

    pub fn function_name(&self, id: FuncInstanceID) -> FunctionName {
        self.func_instances[&id]
    }

    pub fn add_lib(&mut self, library: &ir::Library) {
        for (id, type_def) in library.metadata().type_defs() {
            match type_def {
                ir::TypeDef::Struct(struct_def) => {
                    if struct_def.is_generic() {
                        continue; 
                    }

                    let def_ty = struct_def.identity.to_definition_type(id);
                    self.translate_struct_type(&def_ty);
                },

                ir::TypeDef::Variant(variant_def) => {
                    if variant_def.is_generic() {
                        continue;
                    }

                    let def_ty = id.to_variant_type([]);
                    self.translate_variant_type(&def_ty);
                },
            }
        }

        for (func_id, func) in library.functions() {
            match func {
                ir::Function::Local(func_def) => {
                    if func_def.type_params.is_empty() {
                        self.translate_func_ref(&ir::FunctionRef::new(*func_id));
                    }
                },

                ir::Function::External(func_ref) => {
                    // builtin functions should already be added
                    if func_ref.src == ir::BUILTIN_SRC {
                        continue;
                    }

                    self.translate_extern_func_ref(*func_id, func_ref);
                },
            }

            self.function_types.insert(*func_id, func.sig().clone());
        }

        for (ty, type_info) in library.metadata.type_info() {
            self.translate_type_info(ty, type_info, &library.metadata);
        }

        while self.func_instances.len() < self.function_refs.len() {
            let func_keys: Vec<_> = self.function_refs
                .iter()
                .map(|(key, instance)| (key.clone(), instance.clone()))
                .collect();

            for (key, instance) in func_keys {
                let instance_id = instance.id;
                if self.func_instances.contains_key(&instance_id) {
                    continue;
                }

                self.func_instances.insert(instance_id, instance.name);

                let Some(ir::Function::Local(def)) = library.functions.get(&key.def_id) else {
                    continue;
                };

                self.build_func_ref(instance_id, key, def);
            }
        }

        // now that real functions are defined, we can generate method vcall wrappers
        for class in self.classes.clone() {
            for wrapper_func_def in class.gen_vcall_wrappers(self) {
                self.functions.push(wrapper_func_def);
            }
        }

        let mut class_index = 0;
        while class_index < self.classes.len() {
            let class = &self.classes[class_index];
            if let ClassIdentity::Class(id) = class.identity().clone() {
                let class_type = class.class_type().clone();

                if let Some(dtor) = Class::gen_class_dtor(self, &class_type, id) {
                    self.classes[class_index].add_dtor(dtor);
                }
            }

            class_index += 1;
        }

        let init_index = self
            .functions
            .iter()
            .position(|f| f.decl.name == FunctionName::Init);
        let mut init_func = match init_index {
            Some(index) => {
                self.functions.remove(index)
            },
            None => FunctionDef {
                decl: FunctionDecl {
                    name: FunctionName::Init,
                    params: Vec::new(),
                    return_ty: Type::Void,
                    comment: None,
                },
                body: Vec::new(),
            },
        };

        let mut init_stmts = Vec::new();

        // look up FFI functions
        for ffi_func in &self.ffi_funcs {
            init_stmts.push(ffi_func.init_statement());
        }

        // generate tag initialization
        let mut tag_init_builder = ir::RawInstructionBuilder::new(library.metadata.as_ref(), self.opts.debug);
        ir::util::gen_tags_init(&mut tag_init_builder);
        let tag_init_instructions = tag_init_builder.finish();

        let mut init_builder = CBuilder::new(self, &[], ir::Type::Nothing);
        init_builder.translate_instructions(&tag_init_instructions);

        init_builder.stmts.extend(init_stmts);

        // translate initialization blocks from library
        init_builder.stmts.push(Statement::Comment(format!("library init: {} {}", library.name, library.version)));
        init_builder.translate_instructions(library.init());
        init_func.body.extend(init_builder.stmts);

        for (var_id, var_info) in library.metadata.variables() {
            let name = GlobalName::Variable(var_id);
            let ty = self.translate_type(&var_info.value_type);

            self.global_vars.push(GlobalVar {
                name,
                ty,
            });
        }

        if self.opts.enable_rtti {
            let mut rtti_init_stmts = Vec::new();
            self.gen_rtti_init(&mut rtti_init_stmts);

            init_func.body.splice(0..0, rtti_init_stmts);
        }

        self.functions.push(init_func);
    }
    
    fn create_named_string_lit(&mut self, name: GlobalName, text: &str) {
        self.string_literals.insert(StringLiteralKey::Named(name), StringLiteral(text.to_string()));
    }

    fn get_string_lit(&self, key: StringLiteralKey) -> Option<&str> {
        self.string_literals.get(&key).map(|s| s.0.as_str())
    }
}

impl<'a> fmt::Display for Unit<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.opts.trace_heap {
            writeln!(f, "#define TRACE_HEAP 1")?;
        }

        if self.opts.trace_rc {
            writeln!(f, "#define TRACE_RC 1")?;
        }
        
        if !self.opts.enable_rtti {
            writeln!(f, "#define DISABLE_RTTI")?;
        }

        let string_id = self.get_type_id(&ir::STRING_ID.to_class_ptr_type([]));
        writeln!(f, "#define STRING_STRUCT struct {}", TypeDefName::Struct(string_id))?;
        writeln!(f, "#define STRING_CLASS {}", GlobalName::ClassInstance(string_id))?;
        writeln!(f, "#define STRING_CHARS(str_ptr) (str_ptr->{})", FieldName::ID(ir::STRING_CHARS_FIELD))?;
        writeln!(f, "#define STRING_LEN(str_ptr) (str_ptr->{})", FieldName::ID(ir::STRING_LEN_FIELD))?;

        writeln!(f, "#define DYNARRAY_PTR(arr) (arr->{})", FieldName::DynArrayElements)?;
        writeln!(f, "#define DYNARRAY_LEN(arr) (arr->{})", FieldName::DynArrayLength)?;

        writeln!(f, "#define OBJECT_ARRAY_STRUCT struct {}", TypeDefName::DynArray(self.object_array_id))?;

        if self.opts.enable_rtti {
            let type_info_id = self.get_type_id(&ir::TYPEINFO_ID.to_class_ptr_type([]));
            let method_info_id = self.get_type_id(&ir::METHODINFO_ID.to_class_ptr_type([]));
            let func_info_id = self.get_type_id(&ir::FUNCINFO_ID.to_class_ptr_type([]));

            writeln!(f, "#define TYPEINFO_STRUCT struct {}", TypeDefName::Struct(type_info_id))?;
            writeln!(f, "#define TYPEINFO_NAME(typeinfo) (typeinfo->{})", FieldName::ID(ir::TYPEINFO_NAME_FIELD))?;
            writeln!(f, "#define TYPEINFO_NAME_CHARS(typeinfo) STRING_CHARS(TYPEINFO_NAME(typeinfo))")?;

            writeln!(f, "#define METHODINFO_STRUCT struct {}", TypeDefName::Struct(method_info_id))?;
            writeln!(f, "#define METHODINFO_INVOKER(method) ((Invoker) method->{})", FieldName::ID(ir::METHODINFO_IMPL_FIELD))?;

            writeln!(f, "#define FUNCINFO_STRUCT struct {}", TypeDefName::Struct(func_info_id))?;
            writeln!(f, "#define FUNCINFO_NAME(typeinfo) (typeinfo->{})", FieldName::ID(ir::FUNCINFO_NAME_FIELD))?;
            writeln!(f, "#define FUNCINFO_NAME_CHARS(typeinfo) STRING_CHARS(FUNCINFO_NAME(typeinfo))")?;
            writeln!(f, "#define FUNCINFO_INVOKER(func) ((Invoker) func->{})", FieldName::ID(ir::FUNCINFO_IMPL_FIELD))?;
        }

        writeln!(f, "{}", include_str!("prelude.h"))?;

        let ordered_type_defs = self.ordered_type_defs();

        for (_, def_name) in &ordered_type_defs {
            if let Some(forward_decl) = def_name.forward_decl() {
                writeln!(f, "{};", forward_decl)?;
            }
        }
        writeln!(f)?;

        for (def_id, _) in &ordered_type_defs {
            // special case for System.String: we expect it to already be defined in the prelude
            if *def_id == string_id {
                continue;
            }

            let def = self.get_type_def(*def_id);

            writeln!(f, "{};", def)?;
            writeln!(f)?;
        }

        for func in &self.functions {
            writeln!(f, "{};", func.decl)?;
            writeln!(f)?;
        }

        for ffi_func in &self.ffi_funcs {
            writeln!(f, "{};", ffi_func.func_ptr_decl())?;
            writeln!(f)?;
        }
        
        // declare class vars - we need these before they're defined for the tag arrays
        for class in &self.classes {
            writeln!(f, "{}", class.to_decl_string())?;
        }
        
        // declare string vars
        let string_name = TypeDefName::Struct(string_id);
        for str_key in self.string_literals.keys() {
            write!(f, "static struct {} {};", string_name, str_key.global_name())?;
        } 
        
        for (tag_loc, tag_array_len) in &self.tag_arrays {
            let static_array_name = GlobalName::StaticTagArray(*tag_loc);

            // write data array
            let tag_ptr_ty = Type::object_ptr();
            let data_name = format!("{}_data", static_array_name);

            if *tag_array_len > 0 {
                let data_ty = tag_ptr_ty.sized_array(*tag_array_len);
                writeln!(f, "static {};", data_ty.to_decl_string(&data_name))?;
            }

            // write object
            let tag_array_struct_ty = Type::DefinedType(TypeDefName::DynArray(self.object_array_id));

            let tag_array_decl_string = tag_array_struct_ty.to_decl_string(&static_array_name);
            writeln!(f, "static {} = {{", tag_array_decl_string)?;

            write_immortal_rc_member(f, ClassIdentity::DynArrayClass(self.object_array_id))?;
            writeln!(f, ",")?;

            writeln!(f, "  .{len} = {tag_array_len},", len = FieldName::DynArrayLength)?;

            write!(f, "  .{ptr} = ", ptr = FieldName::DynArrayElements)?;
            if *tag_array_len > 0 {
                writeln!(f, "{data_name}")?;
            } else {
                writeln!(f, "NULL")?;
            }

            writeln!(f, "}};")?;
        }

        if self.opts.enable_rtti {
            let type_info_id = self.get_type_id(&ir::TYPEINFO_ID.to_class_ptr_type([]));
            let func_info_id = self.get_type_id(&ir::FUNCINFO_ID.to_class_ptr_type([]));

            let typeinfo_struct_name = TypeDefName::Struct(type_info_id);
            let typeinfo_class = GlobalName::ClassInstance(type_info_id);

            let typeinfo_def = self.metadata
                .get_struct_def(ir::TYPEINFO_ID)
                .expect("missing definition for TypeInfo object");

            let flags_field = typeinfo_def.fields
                .get(&ir::TYPEINFO_FLAGS_FIELD)
                .expect("missing definition for TypeInfo flags field");

            // should be a set struct
            let flags_type_id = self.get_type_id(&flags_field.ty);
            let flags_type_name = TypeDefName::Struct(flags_type_id);

            for (type_id, type_info) in &self.type_info {
                let ty = self.get_type(*type_id);

                if self.opts.debug {
                    let debug_name = self
                        .get_string_lit(type_info.name_key)
                        .map(str::to_string)
                        .unwrap_or_else(|| ty.to_string());

                    writeln!(f, "/** static TypeInfo of {} ({}) */", debug_name, ty.to_pretty_string(self.metadata))?;
                }

                write!(f, "static struct {} ", typeinfo_struct_name)?;
                write_global_typeinfo_decl_name(f, *type_id)?;
                writeln!(f, " = {{")?;

                writeln!(f, "  .{} = MAKE_RC({}, -1, 0),", FieldName::Rc, typeinfo_class)?;

                write!(f, "  .{} = ", FieldName::ID(ir::TYPEINFO_NAME_FIELD))?;

                writeln!(f, "&{},", type_info.name_key.global_name())?;

                // initialized at runtime for now
                writeln!(f, "  .{} = NULL,", FieldName::ID(ir::TYPEINFO_METHODS_FIELD))?;

                // should be a single 1-word flag type
                writeln!(f, "  .{} = (struct {flags_type_name}) {{", FieldName::ID(ir::TYPEINFO_FLAGS_FIELD))?;
                writeln!(f, "       .{} = {}", FieldName::ID(ir::FieldID(0)), type_info.flags)?;
                writeln!(f, "}},")?;

                writeln!(f, "  .{} = ", FieldName::ID(ir::TYPEINFO_TAGS_FIELD))?;

                let tags_loc = ty
                    .tags_loc()
                    .and_then(|loc| {
                        if *self.tag_arrays.get(&loc)? > 0 {
                            Some(loc)
                        } else {
                            None
                        }
                    });

                match tags_loc {
                    Some(loc) => write!(f, "&{}", GlobalName::StaticTagArray(loc))?,
                    None => write!(f, "NULL,")?,
                }

                writeln!(f, "}};")?;
            }

            let funcinfo_class = GlobalName::ClassInstance(func_info_id);
            let funcinfo_struct_name = TypeDefName::Struct(func_info_id);

            for func in &self.runtime_func_infos {
                let funcinfo_name = GlobalName::StaticFuncInfo(func.id);

                if self.opts.debug {
                    let debug_name = func.name
                        .and_then(|id| self.get_string_lit(StringLiteralKey::StringID(id)))
                        .map(str::to_string)
                        .unwrap_or_else(|| func.id.to_string());
                    writeln!(f, "/** static FunctionInfo of {} */", debug_name)?;
                }

                writeln!(f, "static struct {} {} = {{", funcinfo_struct_name, funcinfo_name)?;
                writeln!(f, "  .{} = MAKE_RC({}, -1, 0),", FieldName::Rc, funcinfo_class)?;

                let name_field = FieldName::ID(ir::FUNCINFO_NAME_FIELD);
                match func.name {
                    Some(name_id) => {
                        let name_str = GlobalName::StringLiteral(name_id);
                        writeln!(f, "  .{name_field} = &{name_str},")?;
                    }

                    None => {
                        writeln!(f, "  .{name_field} = NULL,")?;
                    }
                }

                write!(f, "  .{} = ", FieldName::ID(ir::FUNCINFO_IMPL_FIELD))?;
                if let Some(invoker) = func.invoker {
                    write!(f, "&{}", invoker)?;
                } else {
                    write!(f, "NULL")?;
                }
                writeln!(f, ",")?;

                writeln!(f, "  .{} = ", FieldName::ID(ir::FUNCINFO_TAGS_FIELD))?;

                let tags_loc = ir::TagLocation::Function(func.id);
                if self.tag_arrays.get(&tags_loc).is_some() {
                    write!(f, "&{}", GlobalName::StaticTagArray(tags_loc))?;
                } else {
                    write!(f, "NULL")?;
                };

                writeln!(f, "}};")?;
            }
        }

        for iface in &self.ifaces {
            writeln!(f, "{}", iface.method_table_string())?;
            writeln!(f)?;
        }

        for class in &self.classes {
            writeln!(f, "{}", class.to_decl_string())?;
            writeln!(f, "{}", class.to_def_string(self.opts.enable_rtti))?;
            writeln!(f)?;
        }

        let string_name = TypeDefName::Struct(string_id);
        for (str_key, lit) in &self.string_literals {
            write!(f, "static struct {} {} = ", string_name, str_key.global_name())?;
            writeln!(f, "MAKE_STRING_LIT(\"{}\");", lit.as_str().escape_default())?;
        }

        for global_var in &self.global_vars {
            writeln!(f, "static {};", global_var.ty.to_decl_string(&global_var.name))?;
        }

        for func in &self.functions {
            writeln!(f, "{}", func)?;
            writeln!(f)?;
        }

        writeln!(f, "{}", include_str!("epilogue.h"))?;

        writeln!(f)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct GlobalVar {
    pub name: GlobalName,
    pub ty: Type,
}

fn write_immortal_rc_member(f: &mut fmt::Formatter, class_id: ClassIdentity) -> fmt::Result {
    let class_name = class_id.class_instance_name();
    write!(f, ".rc = MAKE_RC({}, -1, 0)", class_name)
}
