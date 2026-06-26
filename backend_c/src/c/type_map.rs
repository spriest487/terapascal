use crate::c::{ArrayTypeID, Expr};
use crate::c::Class;
use crate::c::FieldName;
use crate::c::FuncAliasDef;
use crate::c::StructDef;
use crate::c::StructMember;
use crate::c::Type;
use crate::c::TypeDecl;
use crate::c::TypeDef;
use crate::c::TypeDefName;
use crate::c::Unit;
use crate::c::VariantDef;
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

impl TypeID {
    pub fn as_lit_int(&self) -> Expr {
        Expr::LitInt(self.0.get() as i128)
    }
}

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
                panic!("type not registered in type map: {}", ty.to_pretty_string(self.metadata))
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
        // types containing any generic placeholders e.g. T, Struct<T>, array of T, etc, aren't real
        // types but need an index too, so map them to `void`
        if ty.contains_generic_params() {
            return self.register_type(ty.clone(), Type::Void);
        }

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

            ir::Type::Array { element, dim } => {
                self.translate_array_type(element, *dim)
            }

            ir::Type::Object(object_id)| ir::Type::WeakObject(object_id) => {
                match object_id {
                    ir::ObjectID::Any
                    | ir::ObjectID::Interface(..) => {
                        self.register_type(ty.clone(), Type::Rc.ptr())
                    }

                    ir::ObjectID::AnyClosure(..) => {
                        self.register_type(ty.clone(), Type::AnonymousClosure.ptr())
                    }

                    ir::ObjectID::Class(..) => {
                        self.translate_struct_type(ty)
                    }

                    ir::ObjectID::Array(element) => {
                        let (array_type_id, array_id) = self.translate_dyn_array_type(element);
                        if ty.is_weak() {
                            self.register_type(ty.clone(), Type::DefinedType(TypeDefName::DynArray(array_id)))
                        } else {
                            array_type_id
                        }
                    }

                    ir::ObjectID::Box(element) => {
                        let (box_type_id, box_id) = self.translate_box_type(element);
                        if ty.is_weak() {
                            self.register_type(ty.clone(), Type::DefinedType(TypeDefName::Box(box_id)))
                        } else {
                            box_type_id
                        }
                    }
                }
            }

            ir::Type::Function(sig) => {
                self.translate_func_type(ty, sig)
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

    pub fn translate_flags_type(&mut self, bits: u8) -> TypeID {
        unimplemented!("C flags type ({bits} bits)")
    }

    pub fn translate_struct_type(&mut self, ty: &ir::Type) -> TypeID {
        if let Some(existing_id) = self.try_get_type_id(ty) {
            return existing_id;
        }

        // eprintln!("translate_struct_type: {}", ty.to_pretty_string(self.metadata));

        let (def_id, args): (_, &[ir::Type]) = match ty {
            ir::Type::Struct(id) => (id.def_id, &id.args),

            ir::Type::Object(ir::ObjectID::Class(id))
            | ir::Type::WeakObject(ir::ObjectID::Class(id)) => {
                (id.def_id, &id.args)
            }

            _ => {
                panic!("unsupported struct type: {}", ty.to_pretty_string(self.metadata));
            }
        };

        let generic_def = self.metadata
            .get_struct_def(def_id)
            .map(|def| Rc::new(def.clone()))
            .unwrap_or_else(|| {
                panic!("missing struct def: {def_id}")
            });

        match instantiate_struct_def(&generic_def, args) {
            Cow::Borrowed(..) => {
                // not a specialized generic
                self.define_struct(ty.clone(), generic_def);
            },

            Cow::Owned(new_def) => {
                if self.opts.trace_generics {
                    let generic_name = generic_def.identity.to_pretty_string(self.metadata);
                    let new_name = new_def.identity.to_pretty_string(self.metadata);

                    eprintln!("new instantiation of struct {}: {}", generic_name, new_name);
                }

                self.define_struct(ty.clone(), Rc::new(new_def));
            },
        };

        // the ID returned from define_struct may not match the one registered for "ty" if "ty"
        // is a weak pointer, so look it up again, assuming it must have been added by define_struct
        self.types.get_by_right(ty).cloned().unwrap()
    }

    fn define_struct(
        &mut self,
        ty: ir::Type,
        def: Rc<ir::StructDef>,
    ) -> TypeID {
        // if this is an object type, use the strong version as its main type even if we
        // first encounter a weak pointer to it
        let ty = match ty {
            ir::Type::WeakObject(object_id) => object_id.to_object_type(),
            _ => ty,
        };

        let comment = Some(ty.to_pretty_string(self.metadata));

        let is_object = ty.is_object();

        let id = self.register_type_with(ty.clone(), |index| {
            let struct_type = Type::DefinedType(TypeDefName::Struct(index));

            if is_object {
                struct_type.ptr()
            } else {
                struct_type
            }
        });

        // if this is an object type, register the weak pointer type as the same C type
        if let ir::Type::Object(object_id) = &ty {
            let object_ptr_type = self.c_types[&id].clone();
            self.register_type(object_id.to_weak_object_type(), object_ptr_type);
        }

        let c_def = Rc::new(StructDef::translate(id, def, comment, self));

        let mut member_deps = Vec::with_capacity(c_def.members.len());
        for member in &c_def.members {
            member.ty.collect_type_def_deps(&mut member_deps);
        }

        self.register_type_def(id, c_def.decl.name, c_def, member_deps);

        if is_object {
            let class = Class::translate(&ty, self);
            self.classes.push(class);
        }

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
            // eprintln!("{} = {} ({})", ty.to_pretty_string(self.metadata), existing_id, self.c_types[&existing_id].typename());
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
                self.define_variant(ty.clone(), Rc::new(generic_def))
            },

            Cow::Owned(new_def) => {
                if self.opts.trace_generics {
                    let generic_name = generic_def.name.to_pretty_string(self.metadata);
                    let new_name = new_def.name.to_pretty_string(self.metadata);

                    eprintln!("new instantiation of variant {}: {}", generic_name, new_name);
                }

                self.define_variant(ty.clone(), Rc::new(new_def))
            },
        }
    }

    fn define_variant(
        &mut self,
        ty: ir::Type,
        def: Rc<ir::VariantDef>,
    ) -> TypeID {
        let comment = Some(ty.to_pretty_string(self.metadata));

        let id = self.register_type_with(ty.clone(), |id| {
            Type::DefinedType(TypeDefName::Variant(id))
        });

        let cases_len = def.cases.len();

        let c_def = Rc::new(VariantDef::translate(id, def, comment, self));

        let mut member_deps = Vec::with_capacity(cases_len);

        for case in &c_def.cases {
            if let Some(case_ty) = &case.ty {
                case_ty.collect_type_def_deps(&mut member_deps);
            }
        }

        self.register_type_def(id, c_def.decl.name, c_def, member_deps);

        // eprintln!("defined {}: ID {}", ty.to_pretty_string(self.metadata), id);

        id
    }

    pub fn get_variant_def(&self, id: TypeID) -> Rc<VariantDef> {
        let def = self.type_defs
            .get(&id)
            .unwrap_or_else(|| panic!("missing variant def: {id}"));

        match def {
            TypeDef::Variant(variant_def) => variant_def.clone(),
            _ => panic!("def {id} is not a variant"),
        }
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
        if let Some(id) = self.types.get_by_right(&ty) {
            panic!("type {} is already registered with ID {}", ty.to_pretty_string(self.metadata), id);
        }

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

    pub fn translate_func_type(&mut self, ty: &ir::Type, sig: &Rc<ir::FunctionSig>) -> TypeID {
        if let Some(existing_id) = self.try_get_type_id(ty) {
            return existing_id;
        }

        // not a real type, won't ever be instantiated
        if sig.contains_generic_params() {
            return self.register_type(ty.clone(), Type::Void);
        }

        let mut deps = Vec::new();

        let return_ty = self.translate_type(&sig.result_type);
        return_ty.collect_type_def_deps(&mut deps);

        let mut param_tys = Vec::new();

        for param_ty in &sig.param_types {
            let param_ty = self.translate_type(param_ty);
            param_ty.collect_type_def_deps(&mut deps);

            param_tys.push(param_ty);
        }

        let id = self.register_type_with(ty.clone(), |id| {
            Type::DefinedType(TypeDefName::FuncPointer(id))
        });

        let name = TypeDefName::FuncPointer(id);

        let func_alias_def = FuncAliasDef {
            decl: TypeDecl { name },
            param_tys,
            return_ty,
            comment: Some(sig.to_pretty_string(self.metadata)),
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