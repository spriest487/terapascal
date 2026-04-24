use crate::{FunctionID, MethodID};
use crate::FunctionInfo;
use crate::FunctionSig;
use crate::IRFormatter;
use crate::InterfaceDef;
use crate::InterfaceID;
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
use crate::VariableID;
use crate::VariableInfo;
use crate::VariantDef;
use std::borrow::Cow;
use std::rc::Rc;
use crate::metadata::vars::ConstInfo;

pub trait MetadataSource : Sized {
    fn as_formatter(&self) -> &impl IRFormatter;

    fn get_string(&self, id: StringID) -> Option<&String>;

    fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&StructDef>;
    fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef>;

    fn type_decls(&self) -> impl Iterator<Item = (TypeDefID, &TypeDecl)>;
    fn get_type_decl(&self, id: TypeDefID) -> Option<&TypeDecl>;
    fn get_type_name(&self, id: TypeDefID) -> Option<&NamePath>;
    fn find_type_decl(&self, name: &NamePath) -> Option<TypeDefID>;
    fn get_type_info(&self, of_type: &Type) -> Option<Rc<TypeInfo>>;

    fn functions(&self) -> impl Iterator<Item = (FunctionID, &FunctionInfo)>;
    fn get_function_info(&self, id: FunctionID) -> Option<&FunctionInfo>;

    fn get_function_ptr_type(&self, sig: &FunctionSig) -> Option<TypeDefID> {
        self.type_defs()
            .find_map(|(id, def)| match def {
                TypeDef::Function(decl_sig) => (*decl_sig == *sig).then_some(id),
                _ => None,
            })
    }

    fn get_function_ptr_sig(&self, id: TypeDefID) -> Option<&FunctionSig> {
        self.get_type_decl(id).and_then(|decl| match decl {
            TypeDecl::Def(TypeDef::Function(ptr_def)) => Some(ptr_def),
            _ => None,
        })
    }

    fn interfaces(&self) -> impl Iterator<Item = (InterfaceID, &InterfaceDef)>;
    fn get_iface_def(&self, iface_id: InterfaceID) -> Option<&InterfaceDef>;
    fn find_iface_impl(&'_ self, func_id: FunctionID) -> Option<InterfaceMethodImplRef<'_>>;
    fn find_virtual_impl(&self, impl_type: &Type, iface_id: InterfaceID, method_id: MethodID) -> Option<FunctionID>;

    fn find_iface(&self, name: &NamePath) -> Option<InterfaceID> {
        self.interfaces()
            .filter_map(|(id, def)| {
                (def.name == *name).then_some(id)
            })
            .next()
    }

    fn iface_name(&self, iface_id: InterfaceID) -> String {
        self.get_iface_def(iface_id)
            .map(|def| def.name.to_pretty_string(self.as_formatter()))
            .unwrap_or_else(|| format!("interface({})", iface_id))
    }

    fn methods(&self) -> impl Iterator<Item = &MethodInfo>;

    fn find_variable(&self, name: &NamePath) -> Option<(VariableID, &VariableInfo)>;
    fn get_variable(&self, id: VariableID) -> Option<&VariableInfo>;
    
    fn find_constant(&self, name: &NamePath) -> Option<&ConstInfo>;
    fn constants(&self) -> impl Iterator<Item = &ConstInfo>;

    fn all_tags(&self) -> impl Iterator<Item = (TagLocation, &[TagInfo])> {
        let type_tags = self.type_defs().map(|(id, def)| {
            let tags = match def {
                TypeDef::Struct(struct_def) => struct_def.tags.as_slice(),
                TypeDef::Variant(struct_def) => struct_def.tags.as_slice(),
                TypeDef::Function(_alias_sig) => &[],
            };

            (TagLocation::TypeDef(id), tags)
        });

        let iface_tags = self
            .interfaces()
            .map(|(id, iface_def)| (TagLocation::Interface(id), iface_def.tags.as_slice()));

        let func_tags = self
            .functions()
            .map(|(id, func_info)| (TagLocation::Function(id), func_info.tags.as_slice()));

        let method_tags = self.methods().map(|method_info| {
            let loc = match &method_info.instance_ty {
                Type::Object(ObjectID::Interface(iface_id)) => TagLocation::InterfaceMethod {
                    iface_id: *iface_id,
                    method_index: method_info.index,
                },

                Type::Object(ObjectID::Class(type_id))
                | Type::Variant(type_id)
                | Type::Struct { id: type_id, .. }
                | Type::Flags(type_id) => TagLocation::Method {
                    type_id: *type_id,
                    method_index: method_info.index,
                },

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
            Type::Struct { id, .. } | Type::Variant(id) => {
                match self.get_type_name(*id) {
                    Some(name) => Cow::Owned(name.to_pretty_string(self)),
                    None => Cow::Owned(id.to_string())
                }
            },

            Type::Array { element, dim } => {
                let elem_name = self.pretty_type_name(element);
                Cow::Owned(format!("array [{}] of {}", dim, elem_name))
            },

            Type::WeakObject(class_id) => {
                let resource_name = self.pretty_object_type_name(class_id);
                Cow::Owned(format!("*weak {}", resource_name))
            },

            Type::Object(class_id) => {
                let resource_name = self.pretty_object_type_name(class_id);
                Cow::Owned(format!("*{}", resource_name))
            },

            Type::Function(func_ty_id) => {
                let text = match self.get_function_ptr_sig(*func_ty_id) {
                    Some(sig) => self.pretty_func_sig(sig),
                    None => format!("function pointer {}", *func_ty_id),
                };
                Cow::Owned(text)
            },

            Type::Pointer(ty) => {
                Cow::Owned(format!("^{}", self.pretty_type_name(ty)))
            },

            Type::TempRef(ty) => {
                Cow::Owned(format!("&{}", self.pretty_type_name(ty)))
            },

            Type::Flags(id) => {
                let name = match self.get_type_decl(*id) {
                    Some(TypeDecl::Def(def)) => {
                        format!("flags[{}]", def.to_pretty_string(self.as_formatter()))
                    },

                    _ => ty.to_string(),
                };
                Cow::Owned(name)
            },

            ty => Cow::Owned(ty.to_string()),
        }
    }

    fn pretty_object_type_name(&self, id: &ObjectID) -> Cow<'_, str> {
        match id {
            ObjectID::Any => Cow::Borrowed("any"),

            ObjectID::Interface(iface_id) => Cow::Owned(self.iface_name(*iface_id)),

            ObjectID::AnyClosure(func_ty_id) => Cow::Owned(match self.get_function_ptr_sig(*func_ty_id) {
                Some(sig) => format!("closure of {}", self.pretty_func_sig(sig)),
                None => format!("closure of {}", func_ty_id),
            }),

            ObjectID::Class(struct_id) => {
                self.pretty_type_name(&struct_id.to_struct_type([]))
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

        for (i, param_ty) in sig.param_tys.iter().enumerate() {
            if i > 0 {
                pretty.push_str("; ");
            }

            pretty.push_str(self.pretty_type_name(param_ty).as_ref());
        }

        pretty.push_str("): ");
        pretty.push_str(self.pretty_type_name(&sig.return_ty).as_ref());

        pretty
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InterfaceMethodImplRef<'a> {
    pub interface: &'a NamePath,
    pub impl_type: &'a Type,
    pub method_name: &'a str,
}
