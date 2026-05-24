use crate::c::boxed::BoxTypeID;
use crate::c::global_typeinfo_decl_name;
use crate::c::type_map::TypeID;
use crate::c::CBuilder;
use crate::c::DynArrayTypeID;
use crate::c::Expr;
use crate::c::FieldName;
use crate::c::FunctionDecl;
use crate::c::FunctionDef;
use crate::c::FunctionName;
use crate::c::GlobalName;
use crate::c::Statement;
use crate::c::Type;
use crate::c::TypeDefName;
use crate::c::Unit;
use crate::c::VariableID;
use crate::ir;
use ir::MetadataSource as _;
use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Write;
use terapascal_ir::InstructionBuilder;

#[derive(Clone, Debug)]
struct MethodImplFunc {
    name: FunctionName,

    vcall_wrapper_decl: FunctionDecl,
}

impl MethodImplFunc {
    fn new(
        iface_id: ir::InterfaceID,
        self_class: ClassIdentity,
        method_id: ir::MethodID,
        iface_method: &ir::Method,
        impl_func_id: ir::FunctionID,
        metadata: &ir::Metadata,
        module: &mut Unit,
    ) -> Self {
        let iface_ty = ir::Type::Object(ir::ObjectID::Interface(iface_id));

        // TODO: doesn't support methods with type params yet
        let method_instance_key = ir::FunctionRef::new(impl_func_id);
        let method_instance = module.translate_func_ref(&method_instance_key);

        let vcall_wrapper_name = FunctionName::MethodWrapper(iface_id, method_id, self_class.to_def_name());

        // generate virtual call wrapper with the param types of the virtually called iface method
        let wrapper_param_tys: Vec<_> = iface_method
            .params
            .iter()
            .map(|ty| module.translate_type(ty))
            .collect();
        let wrapper_return_ty = module.translate_type(&iface_method.return_ty);

        Self {
            name: method_instance.name,
            vcall_wrapper_decl: FunctionDecl {
                comment: Some(format!(
                    "virtual call wrapper impl of {}.{} for {}",
                    metadata.pretty_type_name(&iface_ty),
                    iface_method.name,
                    metadata.pretty_type_name(&self_class.to_ir_type(module)),
                )),
                name: vcall_wrapper_name,
                params: wrapper_param_tys,
                return_ty: wrapper_return_ty,
            },
        }
    }

    fn gen_vcall_wrapper(&self, module: &Unit) -> FunctionDef {
        let impl_func_def = module
            .functions
            .iter()
            .find(|f| f.decl.name == self.name)
            .unwrap();

        let mut call_impl_func_args = Vec::with_capacity(self.vcall_wrapper_decl.params.len());

        // forward the rest of the params as-is
        for i in 0..self.vcall_wrapper_decl.params.len() {
            let concrete_ty = &impl_func_def.decl.params[i];
            
            let param_arg = ir::ArgID(i);

            call_impl_func_args.push(Expr::arg_var(param_arg).cast(concrete_ty.clone()));
        }

        let call_impl_func = Expr::Function(self.name).call(call_impl_func_args);

        let body_stmt = match impl_func_def.decl.return_ty {
            Type::Void => Statement::Expr(call_impl_func),

            _ => {
                let casted_result = call_impl_func.cast(self.vcall_wrapper_decl.return_ty.clone());
                Statement::Expr(Expr::assign(Expr::result_var(), casted_result))
            },
        };

        FunctionDef {
            decl: self.vcall_wrapper_decl.clone(),
            body: vec![body_stmt],
        }
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceImpl {
    method_impls: BTreeMap<ir::MethodID, MethodImplFunc>,
}

#[derive(Copy, Clone, Debug)]
pub enum ClassIdentity {
    Class(TypeID),
    DynArrayClass(DynArrayTypeID),
    BoxClass(BoxTypeID),
}

impl fmt::Display for ClassIdentity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassIdentity::Class(id) => write!(f, "{}", GlobalName::ClassInstance(*id)),
            ClassIdentity::DynArrayClass(id) => write!(f, "{}", GlobalName::DynArrayClassInstance(*id)),
            ClassIdentity::BoxClass(id) => write!(f, "{}", GlobalName::BoxClassInstance(*id)),
        }
    }
}

impl ClassIdentity {
    pub fn to_def_name(&self) -> TypeDefName {
        match self {
            ClassIdentity::Class(id) => TypeDefName::Struct(*id),
            ClassIdentity::DynArrayClass(id) => TypeDefName::DynArray(*id),
            ClassIdentity::BoxClass(id) => TypeDefName::Box(*id),
        }
    }

    pub fn to_ir_type(&self, unit: &Unit) -> ir::Type {
        match self {
            ClassIdentity::Class(id) => {
                unit.get_type(*id).clone()
            },

            ClassIdentity::DynArrayClass(id) => {
                let element = unit.dyn_array_types_by_element.iter()
                    .find_map(|(element, element_array_id)| {
                        (*element_array_id == *id).then(|| {
                            element.clone()
                        })
                    })
                    .map(|id| unit.get_type(id))
                    .unwrap_or_else(|| panic!("missing array type for ID {}", id.0));
                
                element.dyn_array()
            }
            
            ClassIdentity::BoxClass(id) => {
                let element = unit.box_types_by_element
                    .iter()
                    .find_map(|(element_id, element_box_id)| {
                        (*element_box_id == *id).then(|| {
                            unit.get_type(*element_id)
                        })
                    })
                    .unwrap_or_else(|| panic!("missing box type for ID {}", id.0));
                
                element.boxed()
            }
        }
    }

    fn class_type_name(&self) -> GlobalName {
        match self {
            ClassIdentity::Class(_) => GlobalName::ClassType,
            ClassIdentity::DynArrayClass(_) => GlobalName::DynArrayClassType,
            ClassIdentity::BoxClass(_) => GlobalName::ClassType,
        }
    }
    
    pub fn class_instance_name(&self) -> GlobalName {
        match self {
            ClassIdentity::Class(id) => GlobalName::ClassInstance(*id),
            ClassIdentity::DynArrayClass(id) => GlobalName::DynArrayClassInstance(*id),
            ClassIdentity::BoxClass(id) => GlobalName::BoxClassInstance(*id),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    identity: ClassIdentity,
    impls: BTreeMap<ir::InterfaceID, InterfaceImpl>,

    dtor: Option<FunctionName>,

    comment: Option<String>,

    typeinfo_global_name: Option<String>,
}

impl Class {
    pub fn translate(class_ty: &ir::Type, unit: &mut Unit) -> Self {
        unit.translate_type(class_ty);
        let class_index = unit.get_type_id(class_ty);

        let mut impls = BTreeMap::new();

        for (iface_id, iface_impl) in unit.metadata.find_impls(class_ty) {
            let mut method_impls = BTreeMap::new();

            let iface = unit.metadata.get_iface_def(iface_id).unwrap();

            for (method_id, impl_func_id) in iface_impl.methods.iter() {
                let method_def = iface.get_method(*method_id).unwrap();

                let impl_func = MethodImplFunc::new(
                    iface_id,
                    ClassIdentity::Class(class_index),
                    *method_id,
                    method_def,
                    *impl_func_id,
                    unit.metadata,
                    unit,
                );

                method_impls.insert(*method_id, impl_func);
            }

            if method_impls.is_empty() {
                continue;
            }

            impls.insert(iface_id, InterfaceImpl { method_impls });
        }

        let type_def_name = TypeDefName::Struct(class_index);

        let runtime_type = unit.metadata
            .get_type_info(class_ty)
            .unwrap_or_else(|| {
                panic!("missing runtime type for class {}", unit.metadata.pretty_type_name(class_ty))
            });

        let comment = runtime_type
            .get_name_string(unit.metadata)
            .map(|name| name.clone())
            .unwrap_or_else(|| unit.metadata.pretty_type_name(class_ty).to_string());

        let typeinfo_name = global_typeinfo_decl_name(unit, class_ty);

        let ir::Type::Object(ir::ObjectID::Class(class_id)) = class_ty else {
            panic!("invalid type for class: {}", class_ty.to_pretty_string(unit.metadata));
        };

        let dtor = Self::gen_runtime_dtor(unit, class_ty, type_def_name, |builder, self_arg| {
            builder.gen_class_object_dtor_body(&class_id, self_arg)
        });

        Class {
            identity: ClassIdentity::Class(class_index),
            impls,
            dtor,
            comment: Some(comment),
            typeinfo_global_name: typeinfo_name,
        }
    }
    
    pub fn gen_dyn_array_class(
        unit: &mut Unit,
        id: DynArrayTypeID,
        element_type: ir::Type,
    ) -> Self {
        let comment = format!("generated dynarray class (array of {})", element_type.to_pretty_string(unit.metadata));

        let array_type = element_type.dyn_array();
        let array_def_name = TypeDefName::DynArray(id);

        let mut dtor = None;
        if element_type.contains_any_object_refs(unit.metadata) {
            dtor = Self::gen_runtime_dtor(unit, &array_type, array_def_name, |builder, self_ref| {
                builder.gen_dyn_array_dtor_body(self_ref, &element_type);
                true
            });
        }

        Class {
            identity: ClassIdentity::DynArrayClass(id),
            impls: BTreeMap::new(),
            dtor,
            comment: Some(comment),
            typeinfo_global_name: global_typeinfo_decl_name(unit, &array_type),
        }
    }

    pub fn gen_box_class(
        unit: &mut Unit,
        id: BoxTypeID,
        value_type: ir::Type,
    ) -> Self {
        let comment = format!("generated box class (box of {})", value_type.to_pretty_string(unit.metadata));

        let box_type = value_type.boxed();
        let box_def_name = TypeDefName::Box(id);

        let mut dtor = None;
        if value_type.contains_any_object_refs(unit.metadata) {
            dtor = Self::gen_runtime_dtor(unit, &box_type, box_def_name, |builder, self_ref| {
                let value_ref = self_ref.element_ref(box_type.clone(), ir::Value::I32_0);

                builder.release(value_ref.to_deref(), value_type.clone(), ir::Ref::Discard);
                true
            });
        }

        Class {
            identity: ClassIdentity::BoxClass(id),
            impls: BTreeMap::new(),
            dtor,
            comment: Some(comment),
            typeinfo_global_name: global_typeinfo_decl_name(unit, &box_type),
        }
    }
    
    pub fn gen_closure_class(unit: &mut Unit, closure_struct_id: ir::TypeDefID) -> Self {
        let comment = format!("closure class {}", closure_struct_id);

        let ty = closure_struct_id.to_class_ptr_type([]);

        let closure_type_index = unit.create_type_id(&ty);

        Class {
            identity: ClassIdentity::Class(closure_type_index),
            comment: Some(comment),
            dtor: None,
            impls: BTreeMap::new(),
            typeinfo_global_name: global_typeinfo_decl_name(unit, &ty),
        }
    }

    pub fn gen_vcall_wrappers(&self, unit: &Unit) -> Vec<FunctionDef> {
        let mut wrappers = Vec::new();
        for (_iface_id, iface_impl) in &self.impls {
            for (_method_id, method_impl) in &iface_impl.method_impls {
                wrappers.push(method_impl.gen_vcall_wrapper(unit));
            }
        }

        wrappers
    }

    pub fn gen_runtime_dtor<F>(
        unit: &mut Unit,
        object_type: &ir::Type,
        def_name: TypeDefName,
        build_fn: F,
    ) -> Option<FunctionName>
    where
        F: FnOnce(&mut ir::RawInstructionBuilder<ir::Metadata>, ir::Ref) -> bool
    {
        let self_arg_id = ir::ArgID(0);

        let mut builder = ir::RawInstructionBuilder::new(unit.metadata, true);
        builder.local_stack_mut().bind_unnamed_param(self_arg_id, ir::ANY_TYPE, false);

        // the self arg is provided as an Any pointer, so we have to cast it first
        let object_ref = builder.local_temp(object_type.clone());
        builder.cast(object_ref, self_arg_id, object_type.clone());

        if !build_fn(&mut builder, object_ref.to_ref()) {
            return None;
        }

        let comment = format!("runtime dtor for {}", object_type.to_pretty_string(unit.metadata));

        let dtor_body = builder.finish();

        let mut func_builder = CBuilder::new(unit, &[ir::ANY_TYPE], ir::Type::Nothing);

        func_builder.translate_instructions(&dtor_body);
        let dtor_stmts = func_builder.stmts;

        let name = FunctionName::Destructor(def_name);

        unit.functions.push(FunctionDef {
            body: dtor_stmts,
            decl: FunctionDecl {
                name,
                return_ty: Type::Void,
                params: vec![Type::Rc.ptr()],
                comment: Some(comment),
            },
        });

        Some(name)
    }

    pub fn to_decl_string(&self) -> String {
        let mut decls = String::new();

        for (iface_id, _) in &self.impls {
            decls.write_fmt(format_args!(
                "struct MethodTable_{} ImplTable_{}_{};\n",
                iface_id.0, self.identity, iface_id.0
            )).unwrap();
        }

        let class_type_name = self.identity.class_type_name();
        let class_instance_name = self.identity.class_instance_name();

        writeln!(decls, "struct {class_type_name} {class_instance_name};").unwrap();

        decls
    }

    pub fn to_def_string(&self, enable_rtti: bool) -> String {
        let mut def = String::new();
        
        if let Some(comment) = &self.comment {
            def.push_str("/** ");
            def.push_str(comment);
            def.push_str(" **/\n");
        }

        // impl method tables
        let impls: Vec<_> = self.impls.iter().enumerate().collect();

        for (i, (iface_id, iface_impl)) in &impls {
            def.push_str(&format!(
                "struct MethodTable_{} ImplTable_{}_{} = {{\n",
                iface_id.0, self.identity, iface_id.0
            ));

            def.push_str("  .base = {\n");

            def.push_str("    .iface = ");
            def.push_str(&iface_id.to_string());
            def.push_str(",\n");

            def.push_str("    .next = ");
            match impls.get(i + 1) {
                Some((_, (next_impl_id, _))) => {
                    def.push_str(&format!(
                        "&ImplTable_{}_{}.base",
                        self.identity, next_impl_id
                    ));
                },
                None => def.push_str("NULL"),
            }

            def.push_str(",\n");
            def.push_str("  }");
            
            if iface_impl.method_impls.len() > 0 {
                def.push_str(",");
            }
            def.push_str("\n");
            
            for (method_id, _method_name) in &iface_impl.method_impls {
                let wrapper_name = FunctionName::MethodWrapper(
                    **iface_id,
                    *method_id,
                    self.identity.to_def_name(),
                );

                def.push_str("  .method_");
                def.push_str(&method_id.0.to_string());
                def.push_str(" = &");
                def.push_str(&wrapper_name.to_string());
                def.push_str(",\n");
            }
            def.push_str("};\n\n");
        }

        // class struct

        let mut class_init = String::new();
        writeln!(class_init, "{{").unwrap();
        
        match self.identity {
            ClassIdentity::DynArrayClass(array_id) => {
                class_init.push_str("  .base = {\n");
                self.write_class_field_init(&mut class_init, enable_rtti, &impls);
                class_init.push_str("  },");

                let get_element_name = FieldName::DynArrayClassElement;
                let get_element_func = FunctionName::DynArrayGetElement(array_id);
                writeln!(class_init, "  .{} = {},", get_element_name, get_element_func).unwrap();

                let alloc_name = FieldName::DynArrayClassAlloc;
                let alloc_func = FunctionName::DynArrayAlloc(array_id);
                writeln!(class_init, "  .{} = {},", alloc_name, alloc_func).unwrap();

                let length_name = FieldName::DynArrayClassLength;
                let length_func = FunctionName::DynArrayLength(array_id);
                writeln!(class_init, "  .{} = {},", length_name, length_func).unwrap();
            }

            _ => {
                self.write_class_field_init(&mut class_init, enable_rtti, &impls);
            }
        }

        class_init.truncate(class_init.trim_end_matches(',').len());

        class_init.push_str("}");

        writeln!(
            def,
            "struct {} {} = {};",
            self.identity.class_type_name(),
            self.identity, 
            class_init,
        ).unwrap();

        def
    }
    
    fn write_class_field_init(&self,
        out: &mut String,
        enable_rtti: bool,
        impls: &[(usize, (&ir::InterfaceID, &InterfaceImpl))]
    ) {
        writeln!(
            out,
            "  .size = {},",
            Expr::SizeOf(Type::DefinedType(self.identity.to_def_name()))
        ).unwrap();

        if let Some(dtor_func) = &self.dtor {
            writeln!(out, "  .dtor = &{},", dtor_func).unwrap();
        } else {
            writeln!(out, "  .dtor = NULL,").unwrap();
        };

        write!(out, "  .typeinfo =").unwrap();
        if enable_rtti && let Some(typeinfo_name) = &self.typeinfo_global_name {
            writeln!(out, "&{},", typeinfo_name).unwrap();
        } else {
            writeln!(out, "NULL,").unwrap()
        }

        if let Some((_, (first_iface_id, _))) = impls.get(0) {
            writeln!(
                out,
                "  .iface_methods = (struct MethodTable*) &ImplTable_{}_{},",
                self.identity, first_iface_id
            )
                .unwrap();
        } else {
            writeln!(out, "  .iface_methods = NULL,").unwrap();
        }
    }
}

pub struct Interface {
    id: ir::InterfaceID,
    methods: Vec<FunctionDecl>,
}

impl Interface {
    pub fn translate(
        iface_id: ir::InterfaceID,
        iface: &ir::InterfaceDef,
        module: &mut Unit,
    ) -> Self {
        let methods = iface
            .methods
            .iter()
            .enumerate()
            .map(|(method_index, method)| {
                let return_ty = module.translate_type(&method.return_ty);
                let method_id = ir::MethodID(method_index);
                let params = method
                    .params
                    .iter()
                    .map(|param| module.translate_type(param))
                    .collect();

                let name = FunctionName::Method(iface_id, method_id);

                let comment = Some(format!(
                    "Method {} of interface {}",
                    method.name, iface.name
                ));

                FunctionDecl {
                    return_ty,
                    params,
                    name,
                    comment,
                }
            })
            .collect();

        Self {
            id: iface_id,
            methods,
        }
    }

    pub fn method_table_string(&self) -> String {
        let mut table = format!("struct MethodTable_{} {{\n", self.id.0);
        table.push_str("  struct MethodTable base;\n");

        for (method_index, method) in self.methods.iter().enumerate() {
            table.push_str("  ");
            let method_ptr_name = format!("method_{}", method_index);
            table.push_str(&method.ptr_type().to_decl_string(&method_ptr_name));
            table.push_str(";\n");
        }

        table.push_str("};\n\n");

        // vcall thunks for each method
        for (method_index, method) in self.methods.iter().enumerate() {
            table.push_str(&method.to_string());
            table.push_str(" {\n");

            let self_arg = Expr::arg_var(ir::ArgID(0));
            let self_arg_rc = self_arg.cast(Type::Rc.ptr());

            // find the matching table
            table.push_str(&format!(
                "  struct MethodTable* table = {}->class->iface_methods;\n",
                self_arg_rc
            ));
            table.push_str("  while (table) {\n");
            table.push_str(&format!("    if (table->iface == {}) {{\n", self.id.0));
            table.push_str(&format!(
                "      struct MethodTable_{}* my_table = (struct MethodTable_{}*) table;\n",
                self.id.0, self.id.0
            ));

            // get the pointer from the table
            table.push_str("      ");
            table.push_str(&method.ptr_type().to_decl_string("method_ptr"));
            table.push_str(&format!(" = my_table->method_{};\n", method_index));

            if method.return_ty == Type::Void {
                table.push_str("      ");
            } else {
                table.push_str("      return ");
            }

            // call the function
            table.push_str("method_ptr(");

            for (i, _param) in method.params.iter().enumerate() {
                if i > 0 {
                    table.push_str(", ");
                }

                let arg_var = VariableID::Arg(ir::ArgID(i));
                table.push_str(&arg_var.to_string());
            }
            table.push_str(");\n");

            if method.return_ty == Type::Void {
                table.push_str("      return;\n");
            }

            table.push_str("    } else {\n");
            table.push_str("      table = table->next;\n");
            table.push_str("    }\n");

            table.push_str("  }\n");

            // missing vcall
            table.push_str("  abort();\n");

            table.push_str("}\n\n");
        }
        table
    }
}
