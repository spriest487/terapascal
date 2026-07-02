use crate::error::MetadataResult;
use crate::generic::instantiate_struct_def;
use crate::generic::instantiate_variant_def;
use crate::metadata::vars::ConstInfo;
use crate::FunctionID;
use crate::FunctionInfo;
use crate::FunctionSig;
use crate::IRFormatter;
use crate::InterfaceDef;
use crate::InterfaceID;
use crate::InterfaceImpl;
use crate::InterfaceRef;
use crate::MetadataError;
use crate::MethodID;
use crate::MethodInfo;
use crate::NamePath;
use crate::ObjectID;
use crate::StringID;
use crate::StructDef;
use crate::TagInfo;
use crate::TagLocation;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::TypeDefID;
use crate::TypeInfo;
use crate::TypeRef;
use crate::VariableID;
use crate::VariableInfo;
use crate::VariantDef;
use std::borrow::Cow;
use std::rc::Rc;

pub trait MetadataSource : Sized {
    fn as_formatter(&self) -> &impl IRFormatter;

    fn get_string(&self, id: StringID) -> Option<&String>;

    fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&StructDef>;
    fn find_struct_def(&self, name_path: &NamePath) -> Option<(TypeDefID, &StructDef)>;
    
    fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef>;
    fn find_variant_def(&self, name_path: &NamePath) -> Option<(TypeDefID, &VariantDef)>;
    
    fn instantiate_struct_def(&'_ self, id: TypeDefID, args: &[Type]) -> Option<Cow<'_, StructDef>> {
        let def = self.get_struct_def(id)?;
        Some(instantiate_struct_def(def, args))
    }

    fn instantiate_variant_def(&'_ self, id: TypeDefID, args: &[Type]) -> Option<Cow<'_, VariantDef>> {
        let def = self.get_variant_def(id)?;
        Some(instantiate_variant_def(def, args))
    }

    fn type_decls(&self) -> impl Iterator<Item = (TypeDefID, &TypeDecl)>;
    fn get_type_decl(&self, id: TypeDefID) -> Option<&TypeDecl>;
    fn get_type_def(&self, id: TypeDefID) -> Option<&TypeDef>;
    fn get_type_name(&self, id: TypeDefID) -> Option<&NamePath>;
    fn find_type_decl(&self, name: &NamePath) -> Option<TypeDefID>;
    fn get_type_info(&self, of_type: &Type) -> Option<Rc<TypeInfo>>;

    fn functions(&self) -> impl Iterator<Item = (FunctionID, &FunctionInfo)>;
    fn get_function_info(&self, id: FunctionID) -> Option<&FunctionInfo>;

    fn interface_defs(&self) -> impl Iterator<Item = (InterfaceID, &InterfaceDef)>;
    fn get_interface_def(&self, iface_id: InterfaceID) -> Option<&InterfaceDef>;

    fn is_impl(&self, ty: &Type, iface_ref: &InterfaceRef) -> bool;
    fn type_impls(&self, ty: &Type) -> Vec<(&InterfaceRef, &InterfaceImpl)>;
    fn find_impl(&'_ self, func_id: FunctionID) -> Option<InterfaceMethodImplRef<'_>>;
    fn get_interface_method(&self, impl_type: &Type, iface_ref: &InterfaceRef, method_id: MethodID) -> Option<FunctionID>;

    fn find_iface(&self, name: &NamePath) -> Option<InterfaceID> {
        self.interface_defs()
            .filter_map(|(id, def)| {
                (def.name == *name).then_some(id)
            })
            .next()
    }

    fn iface_name(&self, iface_ref: &InterfaceRef) -> String {
        self.get_interface_def(iface_ref.def_id)
            .map(|def| {
                if iface_ref.args != *def.name.type_args {
                    let mut instance_name = def.name.clone();
                    instance_name.type_args = iface_ref.args.clone();

                    instance_name.to_pretty_string(self.as_formatter())
                } else {
                    def.name.to_pretty_string(self.as_formatter())
                }
            })
            .unwrap_or_else(|| format!("interface({})", iface_ref))
    }

    fn methods(&self) -> impl Iterator<Item = &MethodInfo>;
    fn get_dtor_method(&self, for_type: &Type) -> Option<FunctionID>;

    fn find_variable(&self, name: &NamePath) -> Option<(VariableID, &VariableInfo)>;
    fn get_variable(&self, id: VariableID) -> Option<&VariableInfo>;
    
    fn find_constant(&self, name: &NamePath) -> Option<&ConstInfo>;
    fn constants(&self) -> impl Iterator<Item = &ConstInfo>;

    fn all_tags(&self) -> impl Iterator<Item = (TagLocation, &[TagInfo])> {
        let type_tags = self.type_defs().map(|(id, def)| {
            let tags = match def {
                TypeDef::Struct(struct_def) => struct_def.tags.as_slice(),
                TypeDef::Variant(variant_def) => variant_def.tags.as_slice(),
            };

            (TagLocation::TypeDef(id), tags)
        });

        let iface_tags = self
            .interface_defs()
            .map(|(id, iface_def)| (TagLocation::Interface(id), iface_def.tags.as_slice()));

        let func_tags = self
            .functions()
            .map(|(id, func_info)| (TagLocation::Function(id), func_info.tags.as_slice()));

        let method_tags = self.methods().map(|method_info| {
            let loc = match &method_info.instance_ty {
                Type::Object(ObjectID::Interface(iface_id)) => {
                    TagLocation::InterfaceMethod {
                        iface_id: iface_id.def_id,
                        method_index: method_info.index,
                    }
                },

                Type::Object(ObjectID::Class(type_id))
                | Type::Variant(type_id)
                | Type::Struct(type_id) => {
                    TagLocation::Method {
                        type_id: type_id.def_id,
                        method_index: method_info.index,
                    }
                }

                _ => {
                    let instance_ty_name = method_info
                        .instance_ty
                        .to_pretty_string(self.as_formatter());
                    panic!("unexpected base type for method: {}", instance_ty_name)
                },
            };

            (loc, method_info.tags.as_slice())
        });

        type_tags
            .chain(iface_tags)
            .chain(func_tags)
            .chain(method_tags)
    }

    fn type_defs(&self) -> impl Iterator<Item = (TypeDefID, &TypeDef)> {
        self.type_decls().filter_map(|(id, decl)| {
            match decl {
                TypeDecl::Def(def) => Some((id, def)),
                TypeDecl::Reserved | TypeDecl::Forward(..) => None,
            }
        })
    }

    fn pretty_type_name(&self, ty: &Type) -> Cow<'_, str> {
        match ty {
            Type::Struct(id) | Type::Variant(id) => {
                // if the type has a fully-qualified path, display that
                if let Some(name_path) = self.get_type_name(id.def_id) {
                    if id.args == name_path.type_args {
                        return Cow::Owned(name_path.to_pretty_string(self))
                    }

                    let name = NamePath {
                        path: name_path.path.clone(),
                        type_args: id.args.clone()
                    };

                    return Cow::Owned(name.to_pretty_string(self));
                }

                // if the type has a struct identity, display that
                match self.get_type_decl(id.def_id) {
                    Some(TypeDecl::Def(TypeDef::Struct(struct_def))) => {
                        Cow::Owned(struct_def.identity.to_pretty_string(self))
                    }

                    _ => {
                        Cow::Owned(id.to_string())
                    }
                }
            }

            Type::Array { element, dim } => {
                let elem_name = self.pretty_type_name(element);
                Cow::Owned(format!("array[{}] of {}", dim, elem_name))
            },

            Type::WeakObject(class_id) => {
                let resource_name = self.pretty_object_type_name(class_id);
                Cow::Owned(format!("*weak {}", resource_name))
            },

            Type::Object(class_id) => {
                let resource_name = self.pretty_object_type_name(class_id);
                Cow::Owned(format!("*{}", resource_name))
            },

            Type::Function(sig) => {
                let text = self.pretty_func_sig(sig);
                Cow::Owned(text)
            },

            Type::Pointer(ty) => {
                Cow::Owned(format!("^{}", self.pretty_type_name(ty)))
            },

            Type::TempRef(ty) => {
                Cow::Owned(format!("&{}", self.pretty_type_name(ty)))
            },

            ty => Cow::Owned(ty.to_string()),
        }
    }

    fn pretty_object_type_name(&self, id: &ObjectID) -> Cow<'_, str> {
        match id {
            ObjectID::Any => Cow::Borrowed("any"),

            ObjectID::Interface(iface_ref) => {
                Cow::Owned(self.iface_name(iface_ref))
            },

            ObjectID::AnyClosure(sig) => {
                let closure_name = format!("closure of {}", self.pretty_func_sig(sig));

                Cow::Owned(closure_name)
            },

            ObjectID::Class(struct_id) => {
                self.pretty_type_name(&struct_id.to_struct_type())
            },

            ObjectID::Array(element_type) => {
                Cow::Owned(format!("array of {}", self.pretty_type_name(element_type)))
            },

            ObjectID::Box(element_type) => {
                Cow::Owned(format!("box of {}", self.pretty_type_name(element_type)))
            },
        }
    }

    fn pretty_func_sig(&self, sig: &FunctionSig) -> String {
        let mut pretty = String::new();

        pretty.push_str("function(");

        for (i, param_ty) in sig.param_types.iter().enumerate() {
            if i > 0 {
                pretty.push_str("; ");
            }

            pretty.push_str(self.pretty_type_name(param_ty).as_ref());
        }

        pretty.push_str("): ");
        pretty.push_str(self.pretty_type_name(&sig.result_type).as_ref());

        pretty
    }

    // Find the type that represents the definition type of any given type. For non-generic types
    // this is the type itself, and for generic types this is the type with its generic type params
    // e.g. the definition type of Option[String] is Option[T]
    fn find_definition_type<'a>(&self, ty: &'a Type) -> MetadataResult<Cow<'a, Type>> {
        match &ty {
            Type::Object(ObjectID::Class(class_ref))
            | Type::WeakObject(ObjectID::Class(class_ref)) => {
                if class_ref.args.is_empty() {
                    return Ok(Cow::Borrowed(ty));
                }

                let def = self
                    .get_struct_def(class_ref.def_id)
                    .and_then(|def| def.is_class().then_some(def))
                    .ok_or_else(|| {
                        MetadataError::MissingTypeDef(ty.clone())
                    })?;

                let def_type = if let Some(def_name) = def.name() {
                    let def_ref = TypeRef::new(class_ref.def_id, def_name.type_args.clone());
                    if ty.is_weak() {
                        Cow::Owned(def_ref.to_weak_class_object_type())
                    } else {
                        Cow::Owned(def_ref.to_class_object_type())
                    }
                } else {
                    Cow::Borrowed(ty)
                };

                Ok(def_type)
            }

            Type::Variant(variant_ref) => {
                if variant_ref.args.is_empty() {
                    return Ok(Cow::Borrowed(ty));
                }

                let def = self
                    .get_variant_def(variant_ref.def_id)
                    .ok_or_else(|| {
                        MetadataError::MissingTypeDef(ty.clone())
                    })?;

                let def_type = TypeRef::new(variant_ref.def_id, def.name.type_args.clone())
                    .to_variant_type();

                Ok(Cow::Owned(def_type))
            }

            Type::Struct(struct_ref) => {
                if struct_ref.args.is_empty() {
                    return Ok(Cow::Borrowed(ty));
                }

                let def = self
                    .get_struct_def(struct_ref.def_id)
                    .ok_or_else(|| {
                        MetadataError::MissingTypeDef(ty.clone())
                    })?;

                let def_type = if let Some(def_name) = def.name() {
                    Cow::Owned(TypeRef::new(struct_ref.def_id, def_name.type_args.clone())
                        .to_struct_type())
                } else {
                    Cow::Borrowed(ty)
                };

                Ok(def_type)
            }

            Type::Object(ObjectID::Interface(iface_ref))
            | Type::WeakObject(ObjectID::Interface(iface_ref)) => {
                if iface_ref.args.is_empty() {
                    return Ok(Cow::Borrowed(ty));
                }

                let def = self
                    .get_interface_def(iface_ref.def_id)
                    .ok_or_else(|| {
                        MetadataError::MissingTypeDef(ty.clone())
                    })?;

                let def_type = Cow::Owned(InterfaceRef::new(iface_ref.def_id, def.name.type_args.clone())
                    .to_interface_type());

                Ok(def_type)
            }

            _ => {
                Ok(Cow::Borrowed(ty))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InterfaceMethodImplRef<'a> {
    pub interface: &'a NamePath,
    pub impl_type: &'a Type,
    pub method_name: &'a str,
}