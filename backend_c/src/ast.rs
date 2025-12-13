mod function;
mod stmt;
mod expr;
mod string_lit;
mod ty_def;
mod array;
mod boxed;
mod builtin;

pub use self::array::*;
pub use self::expr::*;
pub use self::function::*;
pub use self::stmt::*;
pub use self::ty_def::*;
use crate::ast::boxed::BoxTypeID;
use crate::ast::string_lit::StringLiteral;
use crate::ast::string_lit::StringLiteralKey;
use crate::ir;
use crate::rtti::RuntimeFuncInfo;
use crate::Options;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::hash_map::HashMap;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use terapascal_ir::FieldID;
use terapascal_ir::MetadataSource;
use topological_sort::TopologicalSort;

pub struct Unit<'a> {
    metadata: &'a ir::Metadata,
    
    functions: Vec<FunctionDef>,
    function_types: BTreeMap<ir::FunctionID, ir::TypeDefID>,
    
    ffi_funcs: Vec<FfiFunction>,
    builtin_funcs: HashMap<ir::FunctionID, BuiltinName>,

    global_vars: Vec<GlobalVar>,

    static_array_types: HashMap<ArraySig, Type>,
    
    dyn_array_types_by_element: HashMap<ir::Type, DynArrayTypeID>,
    box_types_by_element: HashMap<ir::Type, BoxTypeID>,

    type_defs: HashMap<TypeDefName, TypeDef>,
    type_defs_order: TopologicalSort<TypeDefName>,

    classes: Vec<Class>,
    ifaces: Vec<Interface>,

    string_literals: HashMap<StringLiteralKey, StringLiteral>,
    static_closures: Vec<ir::StaticClosure>,

    tag_arrays: HashMap<ir::TagLocation, usize>,
    
    object_array_id: DynArrayTypeID,

    opts: Options,
    
    type_infos: HashMap<ir::Type, Rc<ir::TypeInfo>>,
    
    runtime_funcinfos: Vec<RuntimeFuncInfo>,
}

impl<'a> Unit<'a> {
    pub fn new(metadata: &'a ir::Metadata, opts: Options) -> Self {
        let string_literals = metadata
            .strings()
            .map(|(id, str)| (
                StringLiteralKey::StringID(id), 
                StringLiteral::from(str.to_string())))
            .collect();
        
        let type_infos = metadata
            .type_info()
            .map(|(ty, rtti)| (ty.clone(), rtti.clone()))
            .collect();

        let func_infos = metadata
            .functions()
            .filter_map(|(id, func_info)| {
                let name = func_info.runtime_name?;

                Some(RuntimeFuncInfo {
                    name,
                    id,
                    invoker: func_info.invoker,
                })
            })
            .collect();

        let tag_arrays = metadata
            .all_tags()
            .map(|(loc, tags)| (loc, tags.len()))
            .collect();

        let mut unit = Unit {
            metadata,

            functions: Vec::new(),
            function_types: BTreeMap::new(),

            ffi_funcs: Vec::new(),
            builtin_funcs: HashMap::new(),

            type_defs: HashMap::new(),
            type_defs_order: TopologicalSort::new(),

            static_array_types: HashMap::new(),
            
            dyn_array_types_by_element: HashMap::new(),
            box_types_by_element: HashMap::new(),
            
            global_vars: Vec::new(),

            string_literals,
            static_closures: Vec::new(),

            classes: Vec::new(),
            ifaces: Vec::new(),

            opts,

            type_infos,

            runtime_funcinfos: func_infos,
            
            tag_arrays,
            
            // temp value we're about to replace
            object_array_id: DynArrayTypeID(usize::MAX),
        };
        
        unit.create_named_string_lit(GlobalName::InvokeArgsError, "function invoked with invalid argument count");

        unit.object_array_id = unit.get_dyn_array_type(&ir::ANY_TYPE);

        let system_funcs = builtin::system_funcs();
        
        let mut builtin_func_ids = HashMap::with_capacity(system_funcs.len());

        for (pas_name, c_name, _return_ty, _params) in &system_funcs {
            let global_name = &ir::NamePath::new(vec!["System".to_string()], *pas_name);

            // if a function isn't used then it won't be included in the metadata
            if let Some(func_id) = metadata.find_function(global_name) {
                unit.builtin_funcs.insert(func_id, *c_name);
                builtin_func_ids.insert(*c_name, func_id);
            }
        }

        for (class_id, _class_def) in metadata.class_defs() {
            let class = Class::translate(class_id, metadata, &mut unit);
            unit.classes.push(class);
        }
        
        for closure_id in metadata.closures() {
            let class = Class::gen_closure_class(metadata, closure_id);
            unit.classes.push(class);
        } 

        for (iface_id, iface_def) in metadata.interfaces() {
            let iface = Interface::translate(iface_id, iface_def, &mut unit);
            unit.ifaces.push(iface);
        }

        unit
    }

    pub fn pretty_type(&self, ir_ty: &ir::Type) -> Cow<'_, str> {
        match self.type_infos.get(ir_ty).and_then(|typeinfo| typeinfo.name) {
            Some(name_id) => {
                let name = &self.string_literals[&StringLiteralKey::StringID(name_id)];
                Cow::Borrowed(name.as_str())
            },

            None => Cow::Owned(ir_ty.to_string()),
        }
    }

    pub fn pretty_name(&self, name_path: &ir::NamePath) -> String {
        name_path.to_pretty_string(|ty| self.pretty_type(ty))
    }

    fn make_array_type(&mut self, element: Type, dim: usize) -> Type {
        let sig = ArraySig {
            element: element.clone(),
            dim,
        };

        let next_id = self.static_array_types.len();

        match self.static_array_types.entry(sig) {
            Entry::Occupied(entry) => {
                entry.get().clone()
            },

            Entry::Vacant(entry) => {
                let array_name = TypeDefName::StaticArray(ArrayTypeID(next_id));
                
                let array_struct = StructDef::new(array_name, false)
                    .with_comment(format!("array[{}] of {}", dim, element.typename()))
                    .with_member(StructMember {
                        name: FieldName::StaticArrayElements,
                        ty: element.clone().sized_array(dim),
                        comment: None,
                    });

                self.type_defs.insert(array_name, TypeDef::Struct(array_struct));
                self.type_defs_order.insert(array_name);
                
                for element_dep in element.type_def_deps() {
                    self.type_defs_order.add_dependency(element_dep, array_name);
                }

                let array_struct_ty = Type::DefinedType(array_name);
                entry.insert(array_struct_ty.clone());

                array_struct_ty
            },
        }
    }

    pub fn function_name(&self, id: ir::FunctionID) -> FunctionName {
        match self.builtin_funcs.get(&id) {
            Some(builtin) => FunctionName::Builtin(*builtin),
            None => FunctionName::ID(id),
        }
    }

    pub fn add_lib(&mut self, library: &ir::Library) {
        for (id, type_def) in library.metadata().type_defs() {
            let mut member_deps = Vec::new();

            let c_type_def = match type_def {
                ir::TypeDef::Struct(struct_def) => {
                    let struct_def = StructDef::translate(id, struct_def, self);
                    for member in &struct_def.members {
                        member.ty.collect_type_def_deps(&mut member_deps);
                    }

                    TypeDef::Struct(struct_def)
                },

                ir::TypeDef::Variant(variant_def) => {
                    let variant_def = VariantDef::translate(id, variant_def, self);
                    for case in &variant_def.cases {
                        if let Some(case_ty) = &case.ty {
                            case_ty.collect_type_def_deps(&mut member_deps);
                        }
                    }

                    TypeDef::Variant(variant_def)
                },

                ir::TypeDef::Function(func_def) => {
                    let return_ty = Type::from_metadata(&func_def.return_ty, self);
                    return_ty.collect_type_def_deps(&mut member_deps);

                    let mut param_tys = Vec::new();

                    for param_ty in &func_def.param_tys {
                        let param_ty = Type::from_metadata(param_ty, self);
                        param_ty.collect_type_def_deps(&mut member_deps);
                        param_tys.push(param_ty);
                    }

                    let func_alias_def = FuncAliasDef {
                        decl: TypeDecl {
                            name: TypeDefName::Alias(id),
                        },
                        param_tys,
                        return_ty,
                        comment: Some(func_def.to_string()),
                    };

                    TypeDef::FuncAlias(func_alias_def)
                },
            };

            let c_def_name = c_type_def.decl().name;
            self.type_defs.insert(c_def_name, c_type_def);
            self.type_defs_order.insert(c_def_name);
            
            for member_dep in member_deps {
                self.type_defs_order.add_dependency(member_dep, c_def_name);
            }
        }

        for static_closure in library.static_closures() {
            self.static_closures.push(static_closure.clone());
        }

        for (func_id, func) in library.functions() {
            match func {
                ir::Function::Local(func_def) => {
                    let c_func = FunctionDef::translate(*func_id, func_def, self);
                    self.functions.push(c_func);
                },

                ir::Function::External(func_ref) if func_ref.src == ir::BUILTIN_SRC => {
                },

                ir::Function::External(func_ref) => {
                    let ffi_func = FfiFunction::translate(*func_id, func_ref, self);
                    self.ffi_funcs.push(ffi_func);
                },
            }
            
            let sig = func.sig();
            let func_type_id = self.metadata.type_defs()
                .find_map(|(def_id, type_def)| {
                    let ir::TypeDef::Function(def_sig) = type_def else {
                        return None;
                    };

                    (*def_sig == *sig).then_some(def_id)
                });
            
            if let Some(id) = func_type_id {
                self.function_types.insert(*func_id, id);
            }
        }

        // now that real functions are defined, we can generate method vcall wrappers
        for class in self.classes.clone() {
            for wrapper_func_def in class.gen_vcall_wrappers(self) {
                self.functions.push(wrapper_func_def);
            }
            
            if let Some(dtor_invoker) = class.gen_dtor_invoker() {
                self.functions.push(dtor_invoker);
            }
        }

        let init_index = self
            .functions
            .iter()
            .position(|f| f.decl.name == FunctionName::Init);
        let mut init_func = match init_index {
            Some(index) => self.functions.remove(index),
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

        self.gen_rtti_init(&mut init_stmts);

        // look up FFI functions
        for ffi_func in &self.ffi_funcs {
            init_stmts.push(ffi_func.init_statement());
        }

        let mut init_builder = Builder::new(self);
        init_builder.stmts.append(&mut init_stmts);

        // translate initialization blocks from library
        init_builder.translate_instructions(library.init());
        init_func.body.extend(init_builder.stmts);
        
        for (var_id, var_info) in library.metadata.variables() {
            let name = GlobalName::Variable(var_id);
            let ty = Type::from_metadata(&var_info.r#type, self);
            
            self.global_vars.push(GlobalVar {
                name,
                ty,
            });
        }

        self.functions.push(init_func);
    }
    
    fn create_named_string_lit(&mut self, name: GlobalName, text: &str) {
        self.string_literals.insert(StringLiteralKey::Named(name), StringLiteral(text.to_string()));
    }

    fn get_string_lit(&self, id: ir::StringID) -> Option<&str> {
        self.string_literals
            .get(&StringLiteralKey::StringID(id))
            .map(|s| s.0.as_str())
    }
    
    fn gen_rtti_init(&mut self, init_stmts: &mut Vec<Statement>) {
        if !self.opts.enable_rtti {
            return;
        }

        let typeinfo_ty = Type::DefinedType(TypeDefName::Struct(ir::TYPEINFO_ID)).ptr();
        let funcinfo_ty = Type::DefinedType(TypeDefName::Struct(ir::FUNCINFO_ID)).ptr();

        let typeinfo_count = i32::try_from(self.type_infos.len()).unwrap_or(i32::MAX);
        let funcinfo_count = i32::try_from(self.runtime_funcinfos.len()).unwrap_or(i32::MAX);

        // allocate the global typeinfo and funcinfo lists
        init_stmts.push(Statement::assign(
            Expr::Global(GlobalName::TypeInfoCount),
            Expr::LitInt(typeinfo_count as i128),
        ));
        init_stmts.push(Statement::assign(
            Expr::Global(GlobalName::FuncInfoCount),
            Expr::LitInt(funcinfo_count as i128),
        ));

        init_stmts.push(Statement::assign(
            Expr::Global(GlobalName::TypeInfoList),
            Expr::Function(FunctionName::Builtin(BuiltinName::GetMem))
                .call([Expr::infix_op(
                    Expr::LitInt(typeinfo_count as i128),
                    InfixOp::Mul,
                    Expr::SizeOf(typeinfo_ty.clone()),
                )])
                .cast(typeinfo_ty.ptr()),
        ));
        init_stmts.push(Statement::Expr(Expr::Function(FunctionName::Forget).call([
            Expr::Global(GlobalName::TypeInfoList),
            Expr::LitCString("forget".to_string()),
        ])));
        
        init_stmts.push(Statement::assign(
            Expr::Global(GlobalName::FuncInfoList),
            Expr::Function(FunctionName::Builtin(BuiltinName::GetMem))
                .call([Expr::infix_op(
                    Expr::LitInt(funcinfo_count as i128),
                    InfixOp::Mul,
                    Expr::SizeOf(funcinfo_ty.clone()),
                )])
                .cast(funcinfo_ty.ptr()),
        ));
        init_stmts.push(Statement::Expr(Expr::Function(FunctionName::Forget).call([
            Expr::Global(GlobalName::FuncInfoList),
            Expr::LitCString("forget".to_string()),
        ])));
        
        let method_info_class_type = ir::METHODINFO_ID.to_class_ptr_type();
        let method_info_array_id = self.get_dyn_array_type(&method_info_class_type);

        // initialize type info fields that can't be statically initialized
        const METHODS_ARRAY_NAME: &str = "methods_array";
        init_stmts.push(Statement::VariableDecl {
            ty: Type::dyn_array_ptr(method_info_array_id),
            id: VariableID::named(METHODS_ARRAY_NAME),
            null_init: false,
        });

        const METHODINFO_NAME: &str = "methodinfo";
        init_stmts.push(Statement::VariableDecl {
            ty: Type::from_ir_struct(ir::METHODINFO_ID).ptr().ptr(),
            id: VariableID::named(METHODINFO_NAME),
            null_init: false,
        });

        const METHODNULL_NAME: &str = "method_null";
        init_stmts.push(Statement::VariableDecl {
            ty: Type::from_ir_struct(ir::METHODINFO_ID).ptr(),
            id: VariableID::named(METHODNULL_NAME),
            null_init: true,
        });

        let mut typeinfo_index = 0i128;

        for (ty, typeinfo) in &self.type_infos {
            // allocate the method dynarray instance for this typeinfo 
            let method_array_class_ptr = Expr::dyn_array_class(method_info_array_id).addr_of();
            let methods_array_var = Expr::named_var(METHODS_ARRAY_NAME);

            init_stmts.push(Statement::Expr(Expr::assign(
                methods_array_var.clone(),
                Expr::call_newarray(
                    method_info_array_id, 
                    Expr::LitInt(typeinfo.methods.len() as i128), 
                    true
                )
            )));

            let type_info_expr = Expr::Global(GlobalName::StaticTypeInfo(Rc::new(ty.clone())));

            // typeinfo_list[typeinfo_index] = &typeinfo
            init_stmts.push(Statement::Expr(Expr::assign(
                Expr::Global(GlobalName::TypeInfoList).index(Expr::LitInt(typeinfo_index)),
                type_info_expr.clone().addr_of(),
            )));

            typeinfo_index += 1;

            for method_index in 0..typeinfo.methods.len() {
                init_stmts.push(Statement::Expr(Expr::assign(
                    Expr::named_var(METHODINFO_NAME), 
                    method_array_class_ptr
                        .clone()
                        .arrow(FieldName::DynArrayClassElement)
                        .call([
                            methods_array_var.clone().cast(Type::object_ptr()),
                            Expr::LitInt(method_index as i128)
                        ])
                        .cast(Type::class_instance_ptr(ir::METHODINFO_ID).ptr())
                )));
                
                // *methodinfo = RcNew(..method info class, immortal: true)
                let method_info = Expr::named_var(METHODINFO_NAME).deref();
                init_stmts.push(Statement::Expr(
                    method_info.clone().assign_from(Expr::call_new(ir::METHODINFO_ID, true)))
                );

                let method = &typeinfo.methods[method_index];
                init_stmts.push(Statement::Expr(Expr::assign(
                    method_info.clone().clone().arrow(FieldName::ID(ir::METHODINFO_NAME_FIELD)),
                    Expr::Global(GlobalName::StringLiteral(method.name)).addr_of(),
                )));

                init_stmts.push(Statement::Expr(Expr::assign(
                    method_info.clone().arrow(FieldName::ID(ir::METHODINFO_OWNER_FIELD)),
                    type_info_expr.clone().addr_of(),
                )));

                let impl_ptr_expr = if let Some(method_func_id) = method.function
                    && let Some(invoker_id) = self.metadata
                    .get_function_info(method_func_id)
                    .and_then(|f| f.invoker)
                {
                    Expr::Function(FunctionName::ID(invoker_id)).addr_of()
                } else {
                    Expr::Null
                };

                init_stmts.push(Statement::Expr(Expr::assign(
                    method_info.clone().arrow(FieldName::ID(ir::METHODINFO_IMPL_FIELD)),
                    impl_ptr_expr,
                )));

                let tag_loc = ty
                    .tags_loc()
                    .and_then(|loc| {
                        let method_loc = loc.method_loc(method_index)?;
                        let tag_count = *self.tag_arrays.get(&method_loc)?;
                        if tag_count > 0 {
                            Some(method_loc)
                        } else {
                            None
                        }
                    });

                init_stmts.push(Statement::assign(
                    method_info.clone().clone().arrow(FieldName::ID(ir::METHODINFO_TAGS_FIELD)),
                    match tag_loc {
                        Some(loc) => Expr::Global(GlobalName::StaticTagArray(loc)).addr_of(),
                        None => Expr::Null,
                    },
                ));
            }

            // typeinfo.methods = methods_array
            init_stmts.push(Statement::Expr(Expr::assign(
                type_info_expr.field(FieldName::ID(ir::TYPEINFO_METHODS_FIELD)),
                methods_array_var.clone(),
            )));
        }

        for funcinfo_index in 0..self.runtime_funcinfos.len() {
            let func_id = self.runtime_funcinfos[funcinfo_index].id;
            let func_info_expr = Expr::Global(GlobalName::StaticFuncInfo(func_id));

            init_stmts.push(Statement::Expr(Expr::assign(
                Expr::Global(GlobalName::FuncInfoList).index(Expr::LitInt(funcinfo_index as i128)),
                func_info_expr.clone().addr_of(),
            )));
        }
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

        writeln!(f, "#define STRING_STRUCT struct {}", TypeDefName::Struct(ir::STRING_ID))?;
        writeln!(f, "#define STRING_CLASS {}", GlobalName::ClassInstance(ir::STRING_ID))?;
        writeln!(f, "#define STRING_CHARS(str_ptr) (str_ptr->{})", FieldName::ID(ir::STRING_CHARS_FIELD))?;
        writeln!(f, "#define STRING_LEN(str_ptr) (str_ptr->{})", FieldName::ID(ir::STRING_LEN_FIELD))?;
        
        writeln!(f, "#define TYPEINFO_STRUCT struct {}", TypeDefName::Struct(ir::TYPEINFO_ID))?;
        writeln!(f, "#define TYPEINFO_NAME(typeinfo) (typeinfo->{})", FieldName::ID(ir::TYPEINFO_NAME_FIELD))?;
        writeln!(f, "#define TYPEINFO_NAME_CHARS(typeinfo) STRING_CHARS(TYPEINFO_NAME(typeinfo))")?;

        writeln!(f, "#define METHODINFO_STRUCT struct {}", TypeDefName::Struct(ir::METHODINFO_ID))?;
        writeln!(f, "#define METHODINFO_INVOKER(method) ((Invoker) method->{})", FieldName::ID(ir::METHODINFO_IMPL_FIELD))?;

        writeln!(f, "#define FUNCINFO_STRUCT struct {}", TypeDefName::Struct(ir::FUNCINFO_ID))?;
        writeln!(f, "#define FUNCINFO_NAME(typeinfo) (typeinfo->{})", FieldName::ID(ir::FUNCINFO_NAME_FIELD))?;
        writeln!(f, "#define FUNCINFO_NAME_CHARS(typeinfo) STRING_CHARS(FUNCINFO_NAME(typeinfo))")?;
        writeln!(f, "#define FUNCINFO_INVOKER(func) ((Invoker) func->{})", FieldName::ID(ir::FUNCINFO_IMPL_FIELD))?;
        
        writeln!(f, "#define DYNARRAY_PTR(arr) (arr->{})", FieldName::DynArrayElements)?;
        writeln!(f, "#define DYNARRAY_LEN(arr) (arr->{})", FieldName::DynArrayLength)?;
        
        writeln!(f, "#define OBJECT_ARRAY_STRUCT struct {}", TypeDefName::DynArray(self.object_array_id))?;

        writeln!(f, "{}", include_str!("prelude.h"))?;

        let ordered_type_defs: Vec<_> = self.type_defs_order.clone().into_iter().collect();
        if ordered_type_defs.len() != self.type_defs_order.len() {
            eprintln!("ordered defs ({}):", ordered_type_defs.len());
            for def in ordered_type_defs {
                eprintln!(" - {}", def);
            }

            eprintln!("type order sort {}:", self.type_defs_order.len());
            for def in self.type_defs_order.clone().into_iter() {
                eprintln!(" - {}", def);
            }

            panic!("type metadata contained illegal circular references");
        }

        for def_name in &ordered_type_defs {
            if let Some(forward_decl) = self.type_defs[def_name].forward_decl() {
                writeln!(f, "{};", forward_decl)?;
            }
        }
        writeln!(f)?;

        for def_name in &ordered_type_defs {
            // special case for System.String: we expect it to already be defined in the prelude
            if *def_name == TypeDefName::Struct(ir::STRING_ID) {
                continue;
            }

            let def = &self.type_defs[def_name];

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
        let string_name = TypeDefName::Struct(ir::STRING_ID);
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

        let typeinfo_struct_name = TypeDefName::Struct(ir::TYPEINFO_ID);
        let typeinfo_class = GlobalName::ClassInstance(ir::TYPEINFO_ID);

        if self.opts.enable_rtti {
            let typeinfo_def = self.metadata
                .get_struct_def(ir::TYPEINFO_ID)
                .expect("missing definition for TypeInfo object");
            
            let flags_field = typeinfo_def.fields
                .get(&ir::TYPEINFO_FLAGS_FIELD)
                .expect("missing definition for TypeInfo flags field");
            
            // should be a set struct
            let flags_type_name = flags_field.ty
                .def_id()
                .map(|id| TypeDefName::Struct(id))
                .unwrap_or_else(|| panic!("invalid type for TypeInfo flags field: {}", flags_field.ty));

            for (ty, typeinfo) in &self.type_infos {
                if self.opts.debug {
                    let debug_name = typeinfo.name
                        .and_then(|id| self.get_string_lit(id))
                        .map(str::to_string)
                        .unwrap_or_else(|| ty.to_string());

                    writeln!(f, "/** static TypeInfo of {} */", debug_name)?;
                }

                write!(f, "static struct {} ", typeinfo_struct_name)?;
                write_global_typeinfo_decl_name(f, ty)?;
                writeln!(f, " = {{")?;

                writeln!(f, "  .{} = MAKE_RC({}, -1, 0),", FieldName::Rc, typeinfo_class)?;

                write!(f, "  .{} = ", FieldName::ID(ir::TYPEINFO_NAME_FIELD))?;

                let type_name_id = typeinfo.name.unwrap_or(ir::EMPTY_STRING_ID);
                let type_name_str = GlobalName::StringLiteral(type_name_id);

                writeln!(f, "&{},", type_name_str)?;

                // initialized at runtime for now
                writeln!(f, "  .{} = NULL,", FieldName::ID(ir::TYPEINFO_METHODS_FIELD))?;

                // should be a single 1-word flag type
                writeln!(f, "  .{} = (struct {flags_type_name}) {{", FieldName::ID(ir::TYPEINFO_FLAGS_FIELD))?;
                writeln!(f, "       .{} = {}", FieldName::ID(FieldID(0)), typeinfo.flags)?;
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

            let funcinfo_class = GlobalName::ClassInstance(ir::FUNCINFO_ID);
            let funcinfo_struct_name = TypeDefName::Struct(ir::FUNCINFO_ID);

            for func in &self.runtime_funcinfos {
                let funcinfo_name = GlobalName::StaticFuncInfo(func.id);

                if self.opts.debug {
                    let debug_name = self.get_string_lit(func.name)
                        .map(str::to_string)
                        .unwrap_or_else(|| func.id.to_string());
                    writeln!(f, "/** static FunctionInfo of {} */", debug_name)?;
                }

                writeln!(f, "static struct {} {} = {{", funcinfo_struct_name, funcinfo_name)?;
                writeln!(f, "  .{} = MAKE_RC({}, -1, 0),", FieldName::Rc, funcinfo_class)?;

                let name_str = GlobalName::StringLiteral(func.name);

                writeln!(f, "  .{} = &{},", FieldName::ID(ir::FUNCINFO_NAME_FIELD), name_str)?;

                write!(f, "  .{} = ", FieldName::ID(ir::FUNCINFO_IMPL_FIELD))?;
                if let Some(invoker_id) = func.invoker {
                    write!(f, "&{}", FunctionName::ID(invoker_id))?;
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

        let string_name = TypeDefName::Struct(ir::STRING_ID);
        for (str_key, lit) in &self.string_literals {
            write!(f, "static struct {} {} = ", string_name, str_key.global_name())?;
            writeln!(f, "MAKE_STRING_LIT(\"{}\");", lit.as_str().escape_default())?;
        }

        for static_closure in &self.static_closures {
            let global_name = GlobalName::StaticClosure(static_closure.id);
            let decl_string = Type::DefinedType(TypeDefName::Struct(static_closure.closure_id))
                .ptr()
                .to_decl_string(&global_name);

            writeln!(f, "{};", decl_string)?;
        }
        writeln!(f)?;
        
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

#[derive(Eq, PartialEq, Hash)]
struct ArraySig {
    element: Type,
    dim: usize,
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
