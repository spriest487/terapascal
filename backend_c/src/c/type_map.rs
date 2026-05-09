use crate::c::{ArrayTypeID, FieldName, FuncAliasDef, StructDef, StructMember, Type, TypeDecl, TypeDef, TypeDefName, Unit, VariantDef};
use crate::ir;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Formatter;
use std::num::NonZeroUsize;
use std::rc::Rc;
use terapascal_ir::generic::instantiate_struct_def;
use terapascal_ir::generic::instantiate_variant_def;
use terapascal_ir::MetadataSource as _;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct TypeID(NonZeroUsize);

impl fmt::Display for TypeID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ArraySig {
    element: TypeID,
    dim: usize,
}

impl<'a> Unit<'a> {
    pub fn try_get_type(&self, id: TypeID) -> Option<&ir::Type> {
        self.types.get_by_left(&id)
    }

    pub fn get_type(&self, id: TypeID) -> &ir::Type {
        self.try_get_type(id)
            .unwrap_or_else(|| {
                panic!("invalid type index: {id}")
            })
    }

    pub fn try_get_type_id(&self, ty: &ir::Type) -> Option<TypeID> {
        self.types.get_by_right(ty).cloned()
    }

    pub fn get_type_id(&self, ty: &ir::Type) -> TypeID {
        self.types
            .get_by_right(ty)
            .cloned()
            .unwrap_or_else(|| {
                panic!("type not registered in type map: {ty}")
            })
    }

    pub fn try_get_c_type(&self, id: TypeID) -> Option<&Type> {
        self.c_types.get(&id)
    }

    fn get_c_type(&self, id: TypeID) -> Type {
        self.try_get_c_type(id)
            .cloned()
            .unwrap_or_else(|| {
                panic!("invalid type ID: {id}")
            })
    }

    pub fn translate_type(&mut self, ty: &ir::Type) -> Type {
        if let Some(id) = self.try_get_type_id(ty) {
            return self.get_c_type(id);
        }

        let id = self.build_type(ty);
        self.get_c_type(id)
    }

    pub fn create_type_id(&mut self, ir_ty: &ir::Type) -> TypeID {
        self.translate_type(ir_ty);
        self.get_type_id(ir_ty)
    }

    fn build_type(&mut self, ty: &ir::Type) -> TypeID {
        match ty {
            ir::Type::Nothing => {
                self.register_type(ty.clone(), Type::Void)
            },

            ir::Type::Pointer(deref_type) | ir::Type::TempRef(deref_type) => {
                let deref_c_type = self.translate_type(deref_type);
                self.register_type(ty.clone(), deref_c_type.ptr())
            }

            ir::Type::Struct(..) => {
                self.translate_struct_type(ty)
            }

            ir::Type::Variant(_) => {
                self.translate_variant_type(ty)
            }

            ir::Type::Flags(_) => {
                self.translate_struct_type(ty)
            }

            ir::Type::Array { element, dim } => {
                self.translate_array_type(element, *dim)
            }

            ir::Type::Object(object_id)| ir::Type::WeakObject(object_id) => {
                match object_id {
                    ir::ObjectID::Any
                    | ir::ObjectID::AnyClosure(..)
                    | ir::ObjectID::Interface(_) => {
                        self.register_type(ty.clone(), Type::Rc.ptr())
                    }

                    ir::ObjectID::Class(..) => {
                        self.translate_struct_type(ty)
                    }

                    ir::ObjectID::Array(element) => {
                        self.get_dyn_array_type(element).0
                    }
                    ir::ObjectID::Box(element) => {
                        self.get_box_type(element).0
                    }
                }
            }

            ir::Type::Function(..) => {
                self.register_type_with(ty.clone(), |id| {
                    Type::DefinedType(TypeDefName::Alias(id))
                })
            }

            ir::Type::Bool => self.register_type(ty.clone(), Type::Bool),
            ir::Type::U8 => self.register_type(ty.clone(), Type::UChar),
            ir::Type::I8 => self.register_type(ty.clone(), Type::SChar),
            ir::Type::I16 => self.register_type(ty.clone(), Type::UInt16),
            ir::Type::U16 => self.register_type(ty.clone(), Type::UInt16),
            ir::Type::I32 => self.register_type(ty.clone(), Type::Int32),
            ir::Type::U32 => self.register_type(ty.clone(), Type::UInt32),
            ir::Type::I64 => self.register_type(ty.clone(), Type::Int64),
            ir::Type::U64 => self.register_type(ty.clone(), Type::UInt64),
            ir::Type::USize => self.register_type(ty.clone(), Type::SizeType),
            ir::Type::ISize => self.register_type(ty.clone(), Type::PtrDiffType),
            ir::Type::F32 => self.register_type(ty.clone(), Type::Float),
            ir::Type::F64 => self.register_type(ty.clone(), Type::Double),

            ir::Type::Generic(name) => {
                panic!("translate_type: encountered generic placeholder type {name}")
            }
        }
    }

    pub fn translate_struct_type(&mut self, ty: &ir::Type) -> TypeID {
        if let Some(existing_id) = self.try_get_type_id(ty) {
            return existing_id;
        }

        let (def_id, args): (_, &[ir::Type]) = match ty {
            ir::Type::Struct(id) => (id.def_id, &id.args),
            ir::Type::Flags(id) => (*id, &[]),

            ir::Type::Object(ir::ObjectID::Class(..))
            | ir::Type::WeakObject(ir::ObjectID::Class(..)) => {
                // class types: register the pointer
                let type_id = self.register_type_with(ty.clone(), |type_id| {
                    // TODO: special names for builtin types
                    let def_name = TypeDefName::Struct(type_id);

                    Type::DefinedType(def_name).ptr()
                });

                return type_id;
            }

            _ => {
                panic!("unsupported struct type: {}", ty.to_pretty_string(self.metadata));
            }
        };

        let generic_def = self.metadata
            .get_struct_def(def_id)
            .cloned()
            .unwrap_or_else(|| {
                panic!("missing struct def: {def_id}")
            });

        match instantiate_struct_def(&generic_def, args) {
            Cow::Borrowed(..) => {
                // not a specialized generic
                self.define_struct(ty.clone(), &generic_def)
            },

            Cow::Owned(new_def) => {
                if self.opts.trace_generics {
                    let generic_name = generic_def.identity.to_pretty_string(self.metadata);
                    let new_name = new_def.identity.to_pretty_string(self.metadata);

                    eprintln!("new instantiation of struct {}: {}", generic_name, new_name);
                }

                self.define_struct(ty.clone(), &new_def)
            },
        }
    }

    fn define_struct(
        &mut self,
        ty: ir::Type,
        def: &ir::StructDef,
    ) -> TypeID {
        let comment = Some(ty.to_pretty_string(self.metadata));

        let id = self.register_type_with(ty, |index| {
            Type::DefinedType(TypeDefName::Struct(index))
        });

        let c_def = Rc::new(StructDef::translate(id, def, comment, self));

        let mut member_deps = Vec::with_capacity(c_def.members.len());
        for member in &c_def.members {
            member.ty.collect_type_def_deps(&mut member_deps);
        }

        self.register_type_def(id, c_def.decl.name, c_def, member_deps);

        id
    }

    pub fn get_struct_def(&self, id: TypeID) -> Rc<StructDef> {
        self.type_defs
            .get(&id)
            .and_then(|def| match def {
                TypeDef::Struct(struct_def) => Some(struct_def),
                _ => None,
            })
            .cloned()
            .unwrap_or_else(|| panic!("missing struct def: {id}"))
    }

    pub fn translate_variant_type(&mut self, ty: &ir::Type) -> TypeID {
        if let Some(existing_id) = self.try_get_type_id(ty) {
            return existing_id;
        }

        let ir::Type::Variant(variant_id) = ty else {
            panic!("{} is not a variant type", ty.to_pretty_string(self.metadata));
        };

        let generic_def = self.metadata
            .get_variant_def(variant_id.def_id)
            .cloned()
            .unwrap_or_else(|| {
                panic!("missing struct def: {}", variant_id.def_id)
            });

        match instantiate_variant_def(&generic_def, &variant_id.args) {
            Cow::Borrowed(..) => {
                // not a specialized generic
                self.define_variant(ty.clone(), &generic_def)
            },

            Cow::Owned(new_def) => {
                if self.opts.trace_generics {
                    let generic_name = generic_def.name.to_pretty_string(self.metadata);
                    let new_name = new_def.name.to_pretty_string(self.metadata);

                    eprintln!("new instantiation of variant {}: {}", generic_name, new_name);
                }

                self.define_variant(ty.clone(), &new_def)
            },
        }
    }

    fn define_variant(
        &mut self,
        ty: ir::Type,
        def: &ir::VariantDef,
    ) -> TypeID {
        let comment = Some(ty.to_pretty_string(self.metadata));

        let id = self.register_type_with(ty, |index| {
            Type::DefinedType(TypeDefName::Variant(index))
        });

        let c_def = Rc::new(VariantDef::translate(id, def, comment, self));

        let mut member_deps = Vec::with_capacity(def.cases.len());

        for case in &c_def.cases {
            if let Some(case_ty) = &case.ty {
                case_ty.collect_type_def_deps(&mut member_deps);
            }
        }

        self.register_type_def(id, c_def.decl.name, c_def, member_deps);

        id
    }

    pub fn get_variant_def(&self, id: TypeID) -> Rc<VariantDef> {
        self.type_defs
            .get(&id)
            .and_then(|def| match def {
                TypeDef::Variant(variant_def) => Some(variant_def),
                _ => None,
            })
            .cloned()
            .unwrap_or_else(|| panic!("missing variant def: {id}"))
    }

    pub fn translate_array_type(
        &mut self,
        element: &ir::Type,
        dim: usize,
    ) -> TypeID {
        let element_c_type = self.translate_type(element);
        let element_type_id = self.get_type_id(element);

        let sig = ArraySig {
            element: element_type_id,
            dim,
        };

        let next_id = self.static_array_types.len();

        if let Some(existing_id) = self.static_array_types.get(&sig) {
            return *existing_id;
        }

        let array_name = TypeDefName::StaticArray(ArrayTypeID(next_id));
        let array_type = Type::DefinedType(array_name);

        let array_type_id = self.register_type(element.array(dim), array_type);

        let array_struct = StructDef::new(array_name, false)
            .with_comment(format!("array[{}] of {}", dim, element_c_type.typename()))
            .with_member(StructMember {
                name: FieldName::StaticArrayElements,
                ty: element_c_type.clone().sized_array(dim),
                comment: None,
            });


        self.register_type_def(array_type_id, array_name, array_struct, element_c_type.type_def_deps());

        self.static_array_types.insert(sig, array_type_id);

        array_type_id
    }

    fn register_type_with<F>(&mut self, ty: ir::Type, f: F) -> TypeID
    where
        F: FnOnce(TypeID) -> Type
    {
        assert_eq!(None, self.types.get_by_right(&ty));

        let id = self.types
            .len()
            .checked_add(1)
            .and_then(|index| NonZeroUsize::new(index))
            .map(TypeID)
            .expect("type index overflow");

        let c_type = f(id);
        self.types.insert(id, ty);
        self.c_types.insert(id, c_type);

        id
    }

    pub fn register_type(&mut self, ty: ir::Type, c_type: Type) -> TypeID {
        self.register_type_with(ty, |_| c_type)
    }

    pub fn register_type_def(
        &mut self,
        id: TypeID,
        name: TypeDefName,
        def: impl Into<TypeDef>,
        deps: impl IntoIterator<Item=TypeDefName>,
    ) {
        self.type_defs.insert(id, def.into());
        self.type_defs_by_name.insert(name, id);
        self.type_defs_order.insert(name);

        for dep_id in deps {
            self.type_defs_order.add_dependency(dep_id, name);
        }
    }

    pub fn translate_function_type(&mut self, ty: ir::Type) -> TypeID {
        let ir::Type::Function(func_id) = &ty else {
            panic!("{} is not a function type", ty.to_pretty_string(self.metadata));
        };

        let sig = self.metadata
            .get_function_ptr_sig(*func_id)
            .unwrap_or_else(|| {
                panic!("missing definition for function type {}", ty.to_pretty_string(self.metadata));
            });

        let mut deps = Vec::new();

        let return_ty = self.translate_type(&sig.result_type);
        return_ty.collect_type_def_deps(&mut deps);

        let mut param_tys = Vec::new();

        for param_ty in &sig.param_types {
            let param_ty = self.translate_type(param_ty);
            param_ty.collect_type_def_deps(&mut deps);

            param_tys.push(param_ty);
        }

        let id = self.register_type_with(ty, |id| {
            Type::DefinedType(TypeDefName::Alias(id))
        });

        let name = TypeDefName::Alias(id);

        let func_alias_def = FuncAliasDef {
            decl: TypeDecl {
                name,
            },
            param_tys,
            return_ty,
            comment: Some(sig.to_string()),
        };

        self.register_type_def(id, name, func_alias_def, deps);

        id
    }

    pub fn get_type_def(&self, id: TypeID) -> &TypeDef {
        &self.type_defs[&id]
    }

    pub fn ordered_type_defs(&self) -> Vec<(TypeID, &TypeDef)> {
        let ordered_names: Vec<_> = self.type_defs_order.clone().into_iter().collect();

        if ordered_names.len() != self.type_defs_order.len() {
            eprintln!("ordered defs ({}):", ordered_names.len());
            for name in ordered_names {
                eprintln!(" - {}", name);
            }

            eprintln!("type order sort {}:", self.type_defs_order.len());
            for name in self.type_defs_order.clone().into_iter() {
                eprintln!(" - {}", name);
            }

            panic!("type metadata contained illegal circular references");
        }

        let mut defs = Vec::new();

        for name in ordered_names {
            let id = self.type_defs_by_name[&name];
            defs.push((id, &self.type_defs[&id]));
        }

        defs
    }
}