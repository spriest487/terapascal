use crate::c::string_lit::StringLiteral;
use crate::c::string_lit::StringLiteralKey;
use crate::c::BuiltinName;
use crate::c::Expr;
use crate::c::FieldName;
use crate::c::FunctionName;
use crate::c::GlobalName;
use crate::c::InfixOp;
use crate::c::Statement;
use crate::c::Type;
use crate::c::Unit;
use crate::c::VariableID;
use crate::ir;
use terapascal_ir::MetadataSource as _;

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name_key: StringLiteralKey,

    pub flags: u64,

    pub methods: Vec<ir::MethodInfo>,
}

#[derive(Debug, Clone)]
pub struct FuncInfo {
    pub id: ir::FunctionID,
    pub name: Option<ir::StringID>,

    pub invoker: Option<FunctionName>,
}

impl<'a> Unit<'a> {
    pub fn gen_rtti_init(&mut self, init_stmts: &mut Vec<Statement>) {
        if !self.opts.enable_rtti {
            return;
        }

        let typeinfo_ty = self.translate_type(&ir::TYPEINFO_ID.to_class_ptr_type([]));
        let funcinfo_ty = self.translate_type(&ir::FUNCINFO_ID.to_class_ptr_type([]));

        let method_info_class_type = ir::Type::method_info();
        let method_info_class_id = self.get_type_id(&method_info_class_type);

        let (_, method_info_array_id) = self.translate_dyn_array_type(&method_info_class_type);

        let new_method_info_expr = Expr::call_new(&method_info_class_type, true, self);

        let mut types: Vec<_> = self.types
            .iter()
            .map(|(id, ty)| (id.clone(), ty.clone()))
            .collect();
        types.sort_by_key(|(id, _)| *id);

        let mut type_info_refs = Vec::with_capacity(types.len());

        // this variable (the method array ptr) is reused for all type info
        let methods_array_var = Expr::named_var(METHODS_ARRAY_NAME);

        const METHODS_ARRAY_NAME: &str = "methods_array";
        init_stmts.push(Statement::VariableDecl {
            ty: Type::dyn_array_ptr(method_info_array_id),
            id: VariableID::named(METHODS_ARRAY_NAME),
            null_init: false,
        });

        const METHODINFO_NAME: &str = "methodinfo";
        init_stmts.push(Statement::VariableDecl {
            ty: self.translate_type(&ir::METHODINFO_ID.to_class_ptr_type([])).ptr(),
            id: VariableID::named(METHODINFO_NAME),
            null_init: false,
        });

        const METHODNULL_NAME: &str = "method_null";
        init_stmts.push(Statement::VariableDecl {
            ty: self.translate_type(&ir::METHODINFO_ID.to_class_ptr_type([])),
            id: VariableID::named(METHODNULL_NAME),
            null_init: true,
        });

        for (type_id, ty) in types {
            init_stmts.push(Statement::Comment(format!("type info init: {}", ty.to_pretty_string(self.metadata))));

            let type_info = match self.type_info.get(&type_id) {
                Some(type_info) => {
                    type_info.clone()
                },
                None => {
                    // synthesize default type info for types that don't have it naturally
                    let ty = self.get_type(type_id).clone();
                    self.new_type_info(&ty).clone()
                }
            };

            // allocate the method dynarray instance for this typeinfo
            let method_array_class_ptr = Expr::dyn_array_class(method_info_array_id).addr_of();

            let type_info_expr = Expr::Global(GlobalName::StaticTypeInfo(type_id));
            type_info_refs.push(type_info_expr.clone());

            init_stmts.push(Statement::Expr(Expr::assign(
                methods_array_var.clone(),
                Expr::call_newarray(
                    method_info_array_id,
                    Expr::LitInt(type_info.methods.len() as i128),
                    true,
                )
            )));

            for method_index in 0..type_info.methods.len() {
                init_stmts.push(Statement::Expr(Expr::assign(
                    Expr::named_var(METHODINFO_NAME),
                    method_array_class_ptr
                        .clone()
                        .arrow(FieldName::DynArrayClassElement)
                        .call([
                            methods_array_var.clone().cast(Type::object_ptr()),
                            Expr::LitInt(method_index as i128)
                        ])
                        .cast(Type::class_instance_ptr(method_info_class_id).ptr())
                )));

                // *methodinfo = RcNew(..method info class, immortal: true)
                let method_info_var = Expr::named_var(METHODINFO_NAME).deref();

                init_stmts.push(Statement::Expr(
                    method_info_var.clone().assign_from(new_method_info_expr.clone())
                ));

                let method = &type_info.methods[method_index];
                init_stmts.push(Statement::Expr(Expr::assign(
                    method_info_var.clone().clone().arrow(FieldName::ID(ir::METHODINFO_NAME_FIELD)),
                    Expr::Global(GlobalName::StringLiteral(method.name)).addr_of(),
                )));

                init_stmts.push(Statement::Expr(Expr::assign(
                    method_info_var.clone().arrow(FieldName::ID(ir::METHODINFO_OWNER_FIELD)),
                    type_info_expr.clone().addr_of(),
                )));

                let impl_ptr_expr = if let Some(method_func_id) = method.function
                    && let Some(invoker_id) = self.metadata
                    .get_function_info(method_func_id)
                    .and_then(|f| f.invoker)
                {
                    let invoker_key = ir::FunctionRef::new(invoker_id);
                    let invoker_instance = self.translate_func_ref(&invoker_key);

                    Expr::Function(invoker_instance.name).addr_of()
                } else {
                    Expr::Null
                };

                init_stmts.push(Statement::Expr(Expr::assign(
                    method_info_var.clone().arrow(FieldName::ID(ir::METHODINFO_IMPL_FIELD)),
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
                    method_info_var.clone().clone().arrow(FieldName::ID(ir::METHODINFO_TAGS_FIELD)),
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

        let typeinfo_count = i32::try_from(type_info_refs.len()).unwrap_or(i32::MAX);
        let funcinfo_count = i32::try_from(self.runtime_func_infos.len()).unwrap_or(i32::MAX);

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

        // initialize type info fields that can't be statically initialized
        for typeinfo_index in 0..type_info_refs.len() {
            let type_info_ref = &type_info_refs[typeinfo_index];

            // typeinfo_list[typeinfo_index] = &typeinfo
            init_stmts.push(Statement::Expr(Expr::assign(
                Expr::Global(GlobalName::TypeInfoList).index(Expr::LitInt(typeinfo_index as i128)),
                type_info_ref.clone().addr_of(),
            )));
        }

        for funcinfo_index in 0..self.runtime_func_infos.len() {
            let func_id = self.runtime_func_infos[funcinfo_index].id;
            let func_info_expr = Expr::Global(GlobalName::StaticFuncInfo(func_id));

            init_stmts.push(Statement::Expr(Expr::assign(
                Expr::Global(GlobalName::FuncInfoList).index(Expr::LitInt(funcinfo_index as i128)),
                func_info_expr.clone().addr_of(),
            )));
        }
    }

    pub fn translate_type_info(
        &mut self,
        ty: &ir::Type,
        type_info: &ir::TypeInfo,
        metadata: &ir::Metadata,
    ) {
        // it's valid for a type registered in type info to be undefined - the frontend may
        // register a type used in the context of the source language that has no IR
        // representation (e.g. Pascal enums), but which code may still request type info for
        let type_id = if metadata.is_defined(ty) {
            self.create_type_id(ty)
        } else {
            self.create_empty_type_id(ty)
        };

        self.type_info.insert(type_id, TypeInfo {
            name_key: StringLiteralKey::StringID(type_info.name),
            methods: type_info.methods.clone(),
            flags: type_info.flags,
        });
    }

    fn new_type_info(&mut self, ty: &ir::Type) -> &TypeInfo {
        let type_id = if self.metadata.is_defined(ty) {
            self.create_type_id(ty)
        } else {
            self.create_empty_type_id(ty)
        };

        let name = self.type_info_name(ty);

        self.type_info.insert(type_id, TypeInfo {
            flags: ir::TypeInfo::type_runtime_flags(ty, self.metadata),
            methods: Vec::new(),
            name_key: name,
        });

        &self.type_info[&type_id]
    }

    fn type_info_name(&mut self, ty: &ir::Type) -> StringLiteralKey {
        if let Some(type_info) = self.metadata.get_type_info(ty) {
            return StringLiteralKey::StringID(type_info.name);
        }

        let name_string = match ty {
            ir::Type::Array { element, dim } => {
                let element_name_key = self.type_info_name(element);
                let element_name = match self.get_string_lit(element_name_key) {
                    Some(name_str) => name_str.to_string(),
                    None => element.to_pretty_string(self.metadata),
                };

                format!("array[{dim}] of {element_name}")
            }

            ir::Type::Object(ir::ObjectID::Array(element_type)) => {
                let element_name_key = self.type_info_name(element_type);
                let element_name = match self.get_string_lit(element_name_key) {
                    Some(name_str) => name_str.to_string(),
                    None => element_type.to_pretty_string(self.metadata),
                };

                format!("array of {element_name}")
            }

            ir::Type::Object(ir::ObjectID::Box(value_type)) => {
                let value_name_key = self.type_info_name(value_type);
                let value_name = match self.get_string_lit(value_name_key) {
                    Some(name_str) => name_str.to_string(),
                    None => value_type.to_pretty_string(self.metadata),
                };

                format!("box of of {value_name}")
            }

            _ => {
                return StringLiteralKey::StringID(ir::EMPTY_STRING_ID);
            }
        };

        if let Some(id) = self.metadata.find_string_id(&name_string) {
            return StringLiteralKey::StringID(id);
        };

        let type_id = self.get_type_id(ty);
        let key = StringLiteralKey::Named(GlobalName::TypeNameString(type_id));

        self.string_literals.insert(key, StringLiteral(name_string));
        key
    }
}