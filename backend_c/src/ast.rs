mod function;
mod stmt;
mod expr;
mod string_lit;
mod ty_def;

pub use self::expr::*;
pub use self::function::*;
pub use self::stmt::*;
pub use self::ty_def::*;
use crate::ast::string_lit::StringLiteral;
use crate::ir;
use crate::Options;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::hash_map::HashMap;
use std::fmt;
use std::rc::Rc;
use topological_sort::TopologicalSort;
use ir_lang::EMPTY_STRING_ID;

pub struct Unit {
    functions: Vec<FunctionDef>,
    ffi_funcs: Vec<FfiFunction>,
    builtin_funcs: HashMap<ir::FunctionID, FunctionName>,

    global_vars: Vec<GlobalVar>,

    static_array_types: HashMap<ArraySig, Type>,

    type_defs: HashMap<TypeDefName, TypeDef>,
    type_defs_order: TopologicalSort<TypeDefName>,

    classes: Vec<Class>,
    ifaces: Vec<Interface>,

    string_literals: HashMap<ir::StringID, StringLiteral>,
    static_closures: Vec<ir::StaticClosure>,
    
    tag_arrays: Vec<(ir::TagLocation, usize)>,
    tag_array_class: ir::TypeDefID,

    opts: Options,

    typeinfos: HashMap<ir::Type, Rc<ir::RuntimeType>>,
    methodinfo_array_class: ir::TypeDefID,
    pointer_array_class: ir::TypeDefID,
}

impl Unit {
    pub fn new(metadata: &ir::Metadata, opts: Options) -> Self {
        let string_ty = Type::DefinedType(TypeDefName::Struct(ir::STRING_ID)).ptr();

        // array types referenced in the system unit required for reflection to work
        let methodinfo_array_class = metadata
            .find_dyn_array_struct(&ir::METHODINFO_TYPE)
            .expect("method info array type must exist");
        
        let pointer_array_class = metadata
            .find_dyn_array_struct(&ir::Type::Nothing.ptr())
            .expect("raw pointer array type must exist");

        let typeinfo_ty = Type::DefinedType(TypeDefName::Struct(ir::TYPEINFO_ID)).ptr();

        let system_funcs = &[
            ("Int8ToStr", FunctionName::Int8ToStr, string_ty.clone(), vec![
                Type::SChar
            ]),
            ("UInt8ToStr", FunctionName::ByteToStr, string_ty.clone(), vec![
                Type::UChar
            ]),
            ("Int16ToStr", FunctionName::Int16ToStr, string_ty.clone(), vec![
                Type::Int16
            ]),
            ("UInt16ToStr", FunctionName::UInt16ToStr, string_ty.clone(), vec![
                Type::UInt16
            ]),
            ("Int32ToStr", FunctionName::IntToStr, string_ty.clone(), vec![
                Type::Int32
            ]),
            ("UInt32ToStr", FunctionName::UInt32ToStr, string_ty.clone(), vec![
                Type::UInt32
            ]),
            ("Int64ToStr", FunctionName::Int64ToStr, string_ty.clone(), vec![
                Type::Int64
            ]),
            ("UInt64ToStr", FunctionName::UInt64ToStr, string_ty.clone(), vec![
                Type::UInt64
            ]),
            ("NativeIntToStr", FunctionName::NativeIntToStr, string_ty.clone(), vec![
                Type::PtrDiffType
            ]),
            ("NativeUIntToStr", FunctionName::NativeUIntToStr, string_ty.clone(), vec![
                Type::SizeType
            ]),
            ("PointerToStr", FunctionName::PointerToStr, string_ty.clone(), vec![
                Type::Void.ptr()
            ]),
            ("RealToStr", FunctionName::RealToStr, string_ty.clone(), vec![
                Type::Float.ptr()
            ]),
            ("StrToInt", FunctionName::StrToInt, Type::Int32, vec![
                string_ty.clone()
            ]),
            ("GetMem", FunctionName::GetMem, Type::UChar.ptr(), vec![
                Type::Int32
            ]),
            ("FreeMem", FunctionName::FreeMem, Type::Void, vec![
                Type::UChar.ptr()
            ]),
            ("WriteLn", FunctionName::WriteLn, Type::Void, vec![
                string_ty.clone()
            ]),
            ("Write", FunctionName::Write, Type::Void, vec![
                string_ty.clone()
            ]),
            ("ReadLn", FunctionName::ReadLn, string_ty.clone(), vec![]),
            ("ArrayLengthInternal", FunctionName::ArrayLengthInternal, Type::Int32, vec![
                Type::Void.ptr()
            ]),
            ("ArraySetLengthInternal", FunctionName::ArraySetLengthInternal, Type::Void.ptr(), vec![
                Type::Void.ptr(), 
                Type::Int32, 
                Type::Void.ptr()
            ]),
            ("FindTypeInfo", FunctionName::FindTypeInfo, typeinfo_ty.clone(), vec![string_ty.clone()]),
            ("GetTypeInfoCount", FunctionName::GetTypeInfoCount, Type::Int32, vec![]),
            ("GetTypeInfoByIndex", FunctionName::GetTypeInfoByIndex, typeinfo_ty.clone(), vec![Type::Int32]),
            ("GetObjectTypeInfo", FunctionName::GetObjectTypeInfo, typeinfo_ty.clone(), vec![Type::Rc.ptr()]),
            ("InvokeMethod", FunctionName::InvokeMethod, Type::Int32, vec![
                Type::from_ir_struct(ir::METHODINFO_ID).ptr(),
                Type::Void.ptr(),
                Type::from_ir_struct(pointer_array_class).ptr(),
                Type::Void.ptr(),
            ]),
            ("RandomInteger", FunctionName::RandomInteger, Type::Int32, vec![
                Type::Int32, 
                Type::Int32]),
            ("RandomSingle", FunctionName::RandomSingle, Type::Float, vec![
                Type::Float, 
                Type::Float
            ]),
            ("Pow", FunctionName::Pow, Type::Float, vec![
                Type::Float, Type::Float
            ]),
            ("Sqrt", FunctionName::Sqrt, Type::Float, vec![
                Type::Float
            ]),
            ("Sin", FunctionName::Sin, Type::Float, vec![
                Type::Float
            ]),
            ("ArcSin", FunctionName::ArcSin, Type::Float, vec![
                Type::Float
            ]),
            ("Cos", FunctionName::Cos, Type::Float, vec![
                Type::Float
            ]),
            ("ArcCos", FunctionName::ArcCos, Type::Float, vec![
                Type::Float
            ]),
            ("Tan", FunctionName::Tan, Type::Float, vec![
                Type::Float
            ]),
            ("ArcTan", FunctionName::ArcTan, Type::Float, vec![
                Type::Float
            ]),
            ("Infinity", FunctionName::Infinity, Type::Float, vec![]),
            ("NaN", FunctionName::NaN, Type::Float, vec![]),
            ("IsInfinite", FunctionName::IsInfinite, Type::Bool, vec![
                Type::Float
            ]),
            ("IsNaN", FunctionName::IsNaN, Type::Bool, vec![
                Type::Float
            ]),
        ];

        let mut builtin_funcs = HashMap::new();
        for (pas_name, c_name, _params, _return_ty) in system_funcs {
            let global_name = &ir::NamePath::new(vec!["System".to_string()], *pas_name);

            // if a function isn't used then it won't be included in the metadata
            if let Some(func_id) = metadata.find_function(global_name) {
                builtin_funcs.insert(func_id, *c_name);
            }
        }

        let string_literals = metadata
            .strings()
            .map(|(id, str)| (id, StringLiteral::from(str.to_string())))
            .collect();
        
        let type_infos = metadata
            .runtime_types()
            .map(|(ty, rtti)| (ty.clone(), rtti.clone()))
            .collect();
        
        let tag_arrays = metadata
            .tag_counts()
            .map(|(loc, count)| (*loc, *count))
            .collect();
        
        let tag_array_class = metadata
            .find_dyn_array_struct(&ir::ANY_TYPE)
            .expect("object array type must exist");

        let mut module = Unit {
            functions: Vec::new(),
            ffi_funcs: Vec::new(),
            builtin_funcs,

            type_defs: HashMap::new(),
            type_defs_order: TopologicalSort::new(),

            static_array_types: HashMap::new(),
            
            global_vars: Vec::new(),

            string_literals,
            static_closures: Vec::new(),

            classes: Vec::new(),
            ifaces: Vec::new(),

            opts,

            typeinfos: type_infos,
            methodinfo_array_class,
            pointer_array_class,
            
            tag_arrays,
            tag_array_class,
        };

        for (class_id, _class_def) in metadata.class_defs() {
            let class = Class::translate(class_id, metadata, &mut module);
            module.classes.push(class);
        }

        for (iface_id, iface_def) in metadata.ifaces() {
            let iface = Interface::translate(iface_id, iface_def, &mut module);
            module.ifaces.push(iface);
        }

        module
    }

    pub fn pretty_type(&self, ir_ty: &ir::Type) -> Cow<str> {
        match self.typeinfos.get(ir_ty).and_then(|typeinfo| typeinfo.name) {
            Some(name_id) => {
                let name = &self.string_literals[&name_id];
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
            Entry::Occupied(entry) => entry.get().clone(),

            Entry::Vacant(entry) => {
                let name = TypeDefName::StaticArray(next_id);
                let array_struct = StructDef {
                    decl: TypeDecl { name: name.clone() },
                    packed: false,
                    members: vec![StructMember {
                        name: FieldName::StaticArrayElements,
                        ty: element.clone().sized_array(dim),
                        comment: None,
                    }],
                    comment: Some(format!("array[{}] of {}", dim, element.typename())),
                };

                self.type_defs
                    .insert(name.clone(), TypeDef::Struct(array_struct));

                let array_struct_ty = Type::DefinedType(name.clone());
                entry.insert(array_struct_ty.clone());

                self.type_defs_order.insert(name.clone());
                for element_dep in element.type_def_deps() {
                    self.type_defs_order
                        .add_dependency(element_dep, name.clone());
                }

                array_struct_ty
            },
        }
    }

    pub fn function_name(&self, id: ir::FunctionID) -> FunctionName {
        match self.builtin_funcs.get(&id) {
            Some(builtin) => *builtin,
            None => FunctionName::ID(id),
        }
    }

    pub fn add_lib(&mut self, library: &ir::Library) {
        let mut module_type_defs = Vec::new();

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

                    // any function-type object needs a closure param
                    param_tys.push(Type::Void.ptr());

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

            let c_def_name = c_type_def.decl().name.clone();

            module_type_defs.push(c_type_def.clone());
            self.type_defs.insert(c_def_name.clone(), c_type_def);

            self.type_defs_order.insert(c_def_name.clone());
            for member_dep in member_deps {
                self.type_defs_order
                    .add_dependency(member_dep, c_def_name.clone());
            }
        }

        for static_closure in library.static_closures() {
            self.static_closures.push(static_closure.clone());
        }

        for (id, func) in library.functions() {
            match func {
                ir::Function::Local(func_def) => {
                    let c_func = FunctionDef::translate(*id, func_def, self);
                    let rtti_invoker = FunctionDef::invoker(*id, func_def, self);

                    self.functions.push(c_func);
                    self.functions.push(rtti_invoker);
                },

                ir::Function::External(func_ref) if func_ref.src == ir::BUILTIN_SRC => {},

                ir::Function::External(func_ref) => {
                    let ffi_func = FfiFunction::translate(*id, func_ref, self);

                    self.ffi_funcs.push(ffi_func);
                },
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
        
        for (var_id, var_ty) in library.variables() {
            let name = GlobalName::Variable(*var_id);
            let ty = Type::from_metadata(var_ty, self);
            
            self.global_vars.push(GlobalVar {
                name,
                ty,
            });
        }

        self.functions.push(init_func);
    }
    
    fn gen_rtti_init(&self, init_stmts: &mut Vec<Statement>) {
        let typeinfo_ty = Type::DefinedType(TypeDefName::Struct(ir::TYPEINFO_ID)).ptr();
        let typeinfo_count = i32::try_from(self.typeinfos.len()).unwrap_or(i32::MAX);

        // allocate the global typeinfo list
        init_stmts.push(Statement::Expr(Expr::assign(
            Expr::Global(GlobalName::TypeInfoCount),
            Expr::LitInt(typeinfo_count as i128),
        )));

        init_stmts.push(Statement::Expr(Expr::assign(
            Expr::Global(GlobalName::TypeInfoList),
            Expr::Function(FunctionName::GetMem)
                .call([Expr::infix_op(
                    Expr::LitInt(typeinfo_count as i128),
                    InfixOp::Mul,
                    Expr::SizeOf(typeinfo_ty.clone()),
                )])
                .cast(typeinfo_ty.ptr()),
        )));
        
        // initialize type info fields that can't be statically initialized
        const METHODS_ARRAY_NAME: &str = "methods_array";
        init_stmts.push(Statement::VariableDecl {
            ty: Type::from_ir_struct(self.methodinfo_array_class).ptr(),
            id: VariableID::Named(Box::new(METHODS_ARRAY_NAME.to_string())),
            null_init: false,
        });

        const METHODINFO_NAME: &str = "methodinfo";
        init_stmts.push(Statement::VariableDecl {
            ty: Type::from_ir_struct(ir::METHODINFO_ID).ptr().ptr(),
            id: VariableID::Named(Box::new(METHODINFO_NAME.to_string())),
            null_init: false,
        });

        const METHODNULL_NAME: &str = "method_null";
        init_stmts.push(Statement::VariableDecl {
            ty: Type::from_ir_struct(ir::METHODINFO_ID).ptr(),
            id: VariableID::Named(Box::new(METHODNULL_NAME.to_string())),
            null_init: true,
        });

        let method_array_class_ptr = Expr::Global(GlobalName::ClassInstance(self.methodinfo_array_class))
            .addr_of()
            .cast(Type::Class.ptr());
        let method_class_ptr = Expr::Global(GlobalName::ClassInstance(ir::METHODINFO_ID))
            .addr_of();

        let call_array_rcalloc = Expr::Function(FunctionName::RcAlloc).call([
            method_array_class_ptr,
            Expr::LitBool(true),
        ]);

        let mut typeinfo_index = 0i128;
        
        for (ty, typeinfo) in &self.typeinfos {
            // allocate the method dynarray instance for this typeinfo 
            init_stmts.push(Statement::Expr(Expr::assign(
                Expr::named_var(METHODS_ARRAY_NAME),
                call_array_rcalloc.clone()
            )));

            // allocate the array memory
            let array_realloc = Expr::Class(self.methodinfo_array_class)
                .field(FieldName::DynArrayAlloc);
            init_stmts.push(Statement::Expr(array_realloc.call([
                Expr::named_var(METHODS_ARRAY_NAME),
                Expr::LitInt(typeinfo.methods.len() as i128),
                Expr::Null,
                Expr::named_var(METHODNULL_NAME).addr_of(),
            ])));

            let type_info_expr = Expr::Global(GlobalName::StaticTypeInfo(Box::new(ty.clone())));

            // typeinfo_list[typeinfo_index] = &typeinfo
            init_stmts.push(Statement::Expr(Expr::assign(
                Expr::Global(GlobalName::TypeInfoList).index(Expr::LitInt(typeinfo_index)),
                type_info_expr.clone().addr_of(),
            )));

            typeinfo_index += 1;

            let type_name_string_id = typeinfo.name.unwrap_or(EMPTY_STRING_ID);
            init_stmts.push(Statement::Expr(Expr::assign(
                type_info_expr.clone().field(FieldName::ID(ir::TYPEINFO_NAME_FIELD)),
                Expr::Global(GlobalName::StringLiteral(type_name_string_id)).addr_of(),
            )));

            for method_index in 0..typeinfo.methods.len() {
                // methodinfo = methods_array->ptr + method_index
                init_stmts.push(Statement::Expr(Expr::assign(
                    Expr::named_var(METHODINFO_NAME),
                    Expr::infix_op(
                        Expr::named_var(METHODS_ARRAY_NAME)
                            .arrow(FieldName::ID(ir::DYNARRAY_PTR_FIELD)),
                        InfixOp::Add,
                        Expr::LitInt(method_index as i128),
                    )
                )));

                let method_ptr_expr = Expr::named_var(METHODINFO_NAME).deref();

                // *methodinfo = RcAlloc(..method info class, immortal: true)
                init_stmts.push(Statement::Expr(Expr::assign(
                    method_ptr_expr.clone(),
                    Expr::Function(FunctionName::RcAlloc).call([
                        method_class_ptr.clone(),
                        Expr::LitBool(true),
                    ]),
                )));

                let method = &typeinfo.methods[method_index];
                init_stmts.push(Statement::Expr(Expr::assign(
                    method_ptr_expr.clone().arrow(FieldName::ID(ir::METHODINFO_NAME_FIELD)),
                    Expr::Global(GlobalName::StringLiteral(method.name)).addr_of(),
                )));

                init_stmts.push(Statement::Expr(Expr::assign(
                    method_ptr_expr.clone().arrow(FieldName::ID(ir::METHODINFO_OWNER_FIELD)),
                    type_info_expr.clone().addr_of(),
                )));

                init_stmts.push(Statement::Expr(Expr::assign(
                    method_ptr_expr.clone().arrow(FieldName::ID(ir::METHODINFO_IMPL_FIELD)),
                    Expr::Function(FunctionName::Invoker(method.function)).addr_of(),
                )));
            }

            // typeinfo.methods = methods_array
            init_stmts.push(Statement::Expr(Expr::assign(
                type_info_expr.field(FieldName::ID(ir::TYPEINFO_METHODS_FIELD)),
                Expr::named_var(METHODS_ARRAY_NAME),
            )));
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.opts.trace_heap {
            writeln!(f, "#define TRACE_HEAP 1")?;
        }

        if self.opts.trace_rc {
            writeln!(f, "#define TRACE_RC 1")?;
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
        
        writeln!(f, "#define POINTERARRAY_STRUCT struct {}", TypeDefName::Struct(self.pointer_array_class))?;
        
        writeln!(f, "#define DYNARRAY_PTR(arr) (arr->{})", FieldName::ID(ir::DYNARRAY_PTR_FIELD))?;
        writeln!(f, "#define DYNARRAY_LEN(arr) (arr->{})", FieldName::ID(ir::DYNARRAY_LEN_FIELD))?;

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

        let typeinfo_struct_name = TypeDefName::Struct(ir::TYPEINFO_ID);
        for (ty, _typeinfo) in &self.typeinfos {
            write!(f, "static struct {} ", typeinfo_struct_name)?;
            write_global_typeinfo_decl_name(f, ty)?;
            writeln!(f, " = {{")?;

            writeln!(f, "  .{} = {{", FieldName::Rc)?;
            writeln!(f, "    .{} = -1,", FieldName::RcStrongCount)?;
            writeln!(f, "    .{} = 0,", FieldName::RcWeakCount)?;
            writeln!(f, "  }},")?;

            // class typeinfo is initialized before strings (the string class must exist first),
            // so we'll initialize the rest before initialization in main()
            writeln!(f, "  .{} = NULL,", FieldName::ID(ir::TYPEINFO_NAME_FIELD))?;
            writeln!(f, "  .{} = NULL,", FieldName::ID(ir::TYPEINFO_METHODS_FIELD))?;
            writeln!(f, "  .{} = NULL,", FieldName::ID(ir::TYPEINFO_TAGS_FIELD))?;

            writeln!(f, "}};")?;
        }

        for iface in &self.ifaces {
            writeln!(f, "{}", iface.method_table_string())?;
            writeln!(f)?;
        }

        for class in &self.classes {
            writeln!(f, "{}", class.to_decl_string())?;
            writeln!(f, "{}", class.to_def_string())?;
            writeln!(f)?;
        }

        let string_name = TypeDefName::Struct(ir::STRING_ID);
        for (str_id, lit) in &self.string_literals {
            let lit_name = GlobalName::StringLiteral(*str_id);
            write!(f, "static struct {} {} = ", string_name, lit_name)?;
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
        
        for (tag_loc, tag_array_len) in &self.tag_arrays {
            let static_array_name = GlobalName::StaticTagArray(*tag_loc);
            
            // write data array
            let tag_ptr_ty = Type::Void.ptr();
            let data_name = format!("{}_data", static_array_name);

            if *tag_array_len > 0 {
                let data_ty = tag_ptr_ty.sized_array(*tag_array_len);
                writeln!(f, "static {};", data_ty.to_decl_string(&data_name))?;
            }

            // write object
            let tag_array_struct_ty = Type::DefinedType(TypeDefName::Struct(self.tag_array_class));
            let tag_array_object_ty = tag_array_struct_ty.clone().ptr();

            let tag_array_decl_string = tag_array_object_ty.to_decl_string(&static_array_name);
            writeln!(f, "static {} = &({}) {{", tag_array_decl_string, tag_array_struct_ty.typename())?;

            write_immortal_rc_member(f, self.tag_array_class)?;
            writeln!(f, ",")?;

            writeln!(f, "  .{len} = {tag_array_len},", len = FieldName::ID(ir::DYNARRAY_LEN_FIELD))?;

            write!(f, "  .{ptr} = ", ptr = FieldName::ID(ir::DYNARRAY_PTR_FIELD))?;
            if *tag_array_len > 0 {
                writeln!(f, "{data_name}")?;
            } else {
                writeln!(f, "NULL")?;
            }
            
            writeln!(f, "}};")?;
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

fn write_immortal_rc_member(f: &mut fmt::Formatter, class_id: ir::TypeDefID) -> fmt::Result {
    let class_name = GlobalName::ClassInstance(class_id);
    write!(f, ".rc = MAKE_RC({}, -1, 0)", class_name)
}
