use crate::ast::Expr;
use crate::ast::FunctionDecl;
use crate::ast::FunctionDef;
use crate::ast::FunctionName;
use crate::ast::GlobalName;
use crate::ast::Statement;
use crate::ast::Type;
use crate::ast::TypeDefName;
use crate::ast::Unit;
use crate::ir;
use std::collections::BTreeMap;
use std::fmt::Write;

#[derive(Clone, Debug)]
struct MethodImplFunc {
    name: FunctionName,

    vcall_wrapper_decl: FunctionDecl,
}

impl MethodImplFunc {
    fn new(
        iface_id: ir::InterfaceID,
        self_ty_id: ir::TypeDefID,
        method_id: ir::MethodID,
        iface_method: &ir::Method,
        impl_func_id: ir::FunctionID,
        metadata: &ir::Metadata,
        module: &mut Unit,
    ) -> Self {
        let class_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(self_ty_id));
        let iface_ty = ir::Type::RcPointer(ir::VirtualTypeID::Interface(iface_id));

        let impl_func_name = FunctionName::ID(impl_func_id);
        let vcall_wrapper_name = FunctionName::MethodWrapper(iface_id, method_id, self_ty_id);

        // generate virtual call wrapper with the param types of the virtually called iface method
        let wrapper_param_tys: Vec<_> = iface_method
            .params
            .iter()
            .map(|ty| Type::from_metadata(ty, module))
            .collect();
        let wrapper_return_ty = Type::from_metadata(&iface_method.return_ty, module);

        Self {
            name: impl_func_name,
            vcall_wrapper_decl: FunctionDecl {
                comment: Some(format!(
                    "virtual call wrapper impl of {}.{} for {}",
                    metadata.pretty_ty_name(&iface_ty),
                    iface_method.name,
                    metadata.pretty_ty_name(&class_ty),
                )),
                name: vcall_wrapper_name,
                params: wrapper_param_tys,
                return_ty: wrapper_return_ty,
            },
        }
    }

    fn gen_vcall_wrapper(&self, module: &Unit) -> FunctionDef {
        let mut next_param_local = ir::LocalID(match &self.vcall_wrapper_decl.return_ty {
            Type::Void => 0,
            _ => 1,
        });

        let impl_func_def = module
            .functions
            .iter()
            .find(|f| f.decl.name == self.name)
            .unwrap();

        let mut call_impl_func_args = Vec::with_capacity(self.vcall_wrapper_decl.params.len());

        // forward the rest of the params as-is
        for i in 0..self.vcall_wrapper_decl.params.len() {
            let concrete_ty = &impl_func_def.decl.params[i];

            call_impl_func_args.push(Expr::local_var(next_param_local).cast(concrete_ty.clone()));
            next_param_local.0 += 1;
        }

        let call_impl_func = Expr::Function(self.name).call(call_impl_func_args);

        let body_stmt = match impl_func_def.decl.return_ty {
            Type::Void => Statement::Expr(call_impl_func),

            _ => {
                let result_expr = Expr::local_var(ir::LocalID(0));
                let casted_result = call_impl_func.cast(self.vcall_wrapper_decl.return_ty.clone());
                Statement::Expr(Expr::assign(result_expr, casted_result))
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

#[derive(Clone, Debug)]
pub struct Class {
    struct_id: ir::TypeDefID,
    impls: BTreeMap<ir::InterfaceID, InterfaceImpl>,

    dtor: Option<ir::FunctionID>,
    release_func: Option<FunctionName>,

    // if this class is a dyn array, the RTTI for it
    dyn_array_type_info: Option<ir::DynArrayRuntimeType>,
    
    comment: Option<String>,
}

impl Class {
    pub fn translate(
        struct_id: ir::TypeDefID,
        metadata: &ir::Metadata,
        module: &mut Unit,
    ) -> Self {        
        let class_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(struct_id));

        let mut impls = BTreeMap::new();

        for (iface_id, iface_impl) in metadata.find_impls(&class_ty) {
            let mut method_impls = BTreeMap::new();

            let iface = metadata.get_iface_def(iface_id).unwrap();

            for (method_id, impl_func_id) in iface_impl.methods.iter() {
                let method_def = iface.get_method(*method_id).unwrap();

                let impl_func = MethodImplFunc::new(
                    iface_id,
                    struct_id,
                    *method_id,
                    method_def,
                    *impl_func_id,
                    metadata,
                    module,
                );

                method_impls.insert(*method_id, impl_func);
            }

            if method_impls.is_empty() {
                continue;
            }

            impls.insert(iface_id, InterfaceImpl { method_impls });
        }

        let dtor = metadata.find_dtor(struct_id);
        
        let runtime_type = metadata.get_runtime_type(&class_ty)
            .unwrap_or_else(|| {
                panic!("missing runtime type for class {}", metadata.pretty_ty_name(&class_ty))
            });

        let resource_ty = ir::Type::Struct(struct_id);
        let res_runtime_type = metadata
            .get_runtime_type(&resource_ty)
            .unwrap_or_else(|| {
                panic!(
                    "missing runtime type for resource struct of class {}",
                    metadata.pretty_ty_name(&resource_ty),
                )
            });

        let release_func = res_runtime_type.release.map(FunctionName::ID);

        let dyn_array_type_info = metadata
            .dyn_array_structs()
            .iter()
            .filter_map(|(el_ty, arr_struct_id)| {
                if *arr_struct_id == struct_id {
                    metadata.get_dynarray_runtime_type(el_ty)
                } else {
                    None
                }
            })
            .next();

        let comment = runtime_type
            .get_name_string(metadata)
            .map(|name| name.clone())
            .unwrap_or_else(|| metadata.pretty_ty_name(&class_ty).to_string());

        Class {
            struct_id,
            impls,
            dtor,
            release_func,
            dyn_array_type_info,
            comment: Some(comment),
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
    
    pub fn gen_dtor_invoker(&self) -> Option<FunctionDef> {
        let Some(dtor_func) = self.dtor else {
            return None;
        };
        
        let self_ty = Type::DefinedType(TypeDefName::Struct(self.struct_id));
        
        let def = FunctionDef {
            decl: FunctionDecl {
                name: FunctionName::DestructorInvoker(self.struct_id),
                params: vec![Type::Void.ptr()],
                comment: None,
                return_ty: Type::Void,
            },
            body: vec![
                // dtor((Self*) arg0);
                Statement::Expr(Expr::Function(FunctionName::ID(dtor_func)).call([
                    Expr::local_var(ir::LocalID(0)).cast(self_ty.ptr())
                ])),
            ]
        };
        
        Some(def)
    }

    pub fn to_decl_string(&self) -> String {
        let mut decls = String::new();
        for (iface_id, _) in &self.impls {
            decls
                .write_fmt(format_args!(
                    "struct MethodTable_{} ImplTable_{}_{};\n",
                    iface_id.0, self.struct_id.0, iface_id.0
                ))
                .unwrap();
        }

        match self.dyn_array_type_info {
            Some(..) => {
                writeln!(
                    decls,
                    "struct DynArrayClass {};",
                    GlobalName::ClassInstance(self.struct_id)
                )
                .unwrap();
            },
            None => {
                writeln!(
                    decls,
                    "struct Class {};",
                    GlobalName::ClassInstance(self.struct_id)
                )
                .unwrap();
            },
        }

        decls
    }

    pub fn to_def_string(&self) -> String {
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
                iface_id.0, self.struct_id.0, iface_id.0
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
                        self.struct_id, next_impl_id
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
                    self.struct_id
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
        writeln!(
            class_init,
            "  .size = {},",
            Expr::SizeOf(Type::DefinedType(TypeDefName::Struct(self.struct_id)))
        ).unwrap();

        if self.dtor.is_some() {
            let dtor_name = FunctionName::DestructorInvoker(self.struct_id);
            writeln!(class_init, "  .dtor = &{},", dtor_name).unwrap();
        } else {
            writeln!(class_init, "  .dtor = NULL,").unwrap();
        };

        writeln!(class_init, "  .typeinfo = &TypeInfo_VType_Class_{},", self.struct_id).unwrap();        
        write!(class_init, "  .cleanup = (RcCleanupFunc) ").unwrap();

        match self.release_func {
            Some(name) => writeln!(class_init, "&{},", name).unwrap(),
            None => writeln!(class_init, "NULL,").unwrap(),
        }

        if let Some((_, (first_iface_id, _))) = impls.get(0) {
            writeln!(
                class_init,
                "  .iface_methods = (struct MethodTable*) &ImplTable_{}_{},",
                self.struct_id, first_iface_id
            )
            .unwrap();
        } else {
            writeln!(class_init, "  .iface_methods = NULL,").unwrap();
        }

        class_init.push_str("}");

        match &self.dyn_array_type_info {
            Some(dyn_array_ty_info) => {
                writeln!(
                    def,
                    r#"
                struct DynArrayClass {class_name} = {{
                    .base = {class_init},
                    .alloc = {alloc_func},
                    .length = {len_func},
                }};"#,
                    class_name = GlobalName::ClassInstance(self.struct_id),
                    class_init = class_init,
                    alloc_func = FunctionName::ID(dyn_array_ty_info.alloc),
                    len_func = FunctionName::ID(dyn_array_ty_info.length),
                ).unwrap();
            },

            None => {
                writeln!(
                    def,
                    "struct Class Class_{} = {};",
                    self.struct_id, class_init,
                )
                .unwrap();
            },
        }

        def
    }
}

pub struct Interface {
    id: ir::InterfaceID,
    methods: Vec<FunctionDecl>,
}

impl Interface {
    pub fn translate(
        iface_id: ir::InterfaceID,
        iface: &ir::Interface,
        module: &mut Unit,
    ) -> Self {
        let methods = iface
            .methods
            .iter()
            .enumerate()
            .map(|(method_index, method)| {
                let return_ty = Type::from_metadata(&method.return_ty, module);
                let method_id = ir::MethodID(method_index);
                let params = method
                    .params
                    .iter()
                    .map(|param| Type::from_metadata(param, module))
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

            let (self_arg_local, arg_offset) = match method.return_ty {
                Type::Void => (Expr::local_var(ir::LocalID(0)), 0),
                _ => (Expr::local_var(ir::LocalID(1)), 1),
            };

            let self_arg_rc = self_arg_local.cast(Type::Rc.ptr());

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
                table.push_str("L");
                table.push_str(&(i + arg_offset).to_string());
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
