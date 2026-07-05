use crate::metadata::vars::ConstInfo;
use crate::IRFormatter;
use crate::InterfaceDef;
use crate::InterfaceID;
use crate::InterfaceImpl;
use crate::InterfaceMethodImplRef;
use crate::InterfaceRef;
use crate::Metadata;
use crate::MetadataSource;
use crate::MethodID;
use crate::MethodInfo;
use crate::StringID;
use crate::StructDef;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::TypeDefID;
use crate::TypeInfo;
use crate::VariableID;
use crate::VariableInfo;
use crate::VariantDef;
use crate::{DeclPath, FunctionID};
use crate::{FunctionInfo, StringPath};
use std::rc::Rc;

pub trait MetadataCollection {
    fn all_metadata(&self) -> impl Iterator<Item=&Metadata>;

    fn find_in_self_or_refs<'a, T, F>(&'a self, f: F) -> Option<T>
    where
        F: Fn(&'a Metadata) -> Option<T>,
        T: 'a
    {
        self.all_metadata().find_map(f)
    }

    fn iter_in_self_or_refs<'a, T, F, Iter>(&'a self, f: F) -> impl Iterator<Item=T>
    where
        F: Fn(&'a Metadata) -> Iter + 'a,
        Iter: Iterator<Item=T> + 'a,
        T: 'a
    {
        self.all_metadata().flat_map(f)
    }
}

impl<T: MetadataCollection> MetadataSource for T {
    fn as_formatter(&self) -> &impl IRFormatter {
        self
    }

    fn get_string(&self, id: StringID) -> Option<&String> {
        self.find_in_self_or_refs(move |metadata| metadata.get_string(id))
    }

    fn get_struct_def(&self, def_id: TypeDefID) -> Option<&StructDef> {
        self.find_in_self_or_refs(move |metadata| metadata.get_struct_def(def_id))
    }

    fn find_struct_def(&self, name_path: &DeclPath) -> Option<(TypeDefID, &StructDef)> {
        self.find_in_self_or_refs(move |metadata| metadata.find_struct_def(name_path))
    }

    fn get_variant_def(&self, def_id: TypeDefID) -> Option<&VariantDef> {
        self.find_in_self_or_refs(move |metadata| metadata.get_variant_def(def_id))
    }

    fn find_variant_def(&self, name_path: &DeclPath) -> Option<(TypeDefID, &VariantDef)> {
        self.find_in_self_or_refs(move |metadata| metadata.find_variant_def(name_path))
    }

    fn type_decls(&self) -> impl Iterator<Item=(TypeDefID, &TypeDecl)> {
        self.iter_in_self_or_refs(move |metadata| metadata.type_decls())
    }

    fn get_type_decl(&self, id: TypeDefID) -> Option<&TypeDecl> {
        self.find_in_self_or_refs(move |metadata| metadata.get_type_decl(id))
    }

    fn get_type_def(&self, id: TypeDefID) -> Option<&TypeDef> {
        self.find_in_self_or_refs(move |metadata| metadata.get_type_def(id))
    }

    fn get_type_name(&self, id: TypeDefID) -> Option<&DeclPath> {
        self.find_in_self_or_refs(move |metadata| metadata.get_type_name(id))
    }

    fn find_type_decl(&self, name: &DeclPath) -> Option<TypeDefID> {
        self.find_in_self_or_refs(move |metadata| metadata.find_type_decl(name))
    }

    fn get_type_info(&self, of_type: &Type) -> Option<Rc<TypeInfo>> {
        self.find_in_self_or_refs(move |metadata| metadata.get_type_info(of_type))
    }

    fn functions(&self) -> impl Iterator<Item=(FunctionID, &FunctionInfo)> {
        self.iter_in_self_or_refs(move |metadata| metadata.functions())
    }

    fn get_function_info(&self, id: FunctionID) -> Option<&FunctionInfo> {
        self.find_in_self_or_refs(move |metadata| metadata.get_function_info(id))
    }

    fn interface_defs(&self) -> impl Iterator<Item=(InterfaceID, &InterfaceDef)> {
        self.iter_in_self_or_refs(move |metadata| metadata.interface_defs())
    }

    fn get_interface_def(&self, iface_id: InterfaceID) -> Option<&InterfaceDef> {
        self.find_in_self_or_refs(move |metadata| metadata.get_interface_def(iface_id))
    }

    fn is_impl(&self, ty: &Type, iface_ref: &InterfaceRef) -> bool {
        self.find_in_self_or_refs(
            move |metadata| metadata.is_impl(ty, iface_ref).then_some(true)
        ).unwrap_or(false)
    }

    fn iface_impls(&self) -> impl Iterator<Item=(&Type, impl Iterator<Item=(&InterfaceRef, &InterfaceImpl)>)> {
        self.iter_in_self_or_refs(move |metadata| metadata.iface_impls())
    }

    fn type_impls(&self, ty: &Type) -> Vec<(&InterfaceRef, &InterfaceImpl)> {
        let mut impls = Vec::new();
        for metadata in self.all_metadata() {
            impls.extend(metadata.type_impls(ty));
        }
        impls
    }

    fn find_impl(&'_ self, func_id: FunctionID) -> Option<InterfaceMethodImplRef<'_>> {
        self.find_in_self_or_refs(move |metadata| metadata.find_impl(func_id))
    }

    fn get_interface_method(&self, impl_type: &Type, iface_ref: &InterfaceRef, method_id: MethodID) -> Option<FunctionID> {
        self.find_in_self_or_refs(move |metadata| metadata.get_interface_method(impl_type, iface_ref, method_id))
    }

    fn methods(&self) -> impl Iterator<Item=&MethodInfo> {
        self.iter_in_self_or_refs(move |metadata| metadata.methods())
    }

    fn get_dtor_method(&self, for_type: &Type) -> Option<FunctionID> {
        self.find_in_self_or_refs(move |metadata| metadata.get_dtor_method(for_type))
    }

    fn find_variable(&self, name: &StringPath) -> Option<(VariableID, &VariableInfo)> {
        self.find_in_self_or_refs(move |metadata| metadata.find_variable(name))
    }

    fn get_variable(&self, id: VariableID) -> Option<&VariableInfo> {
        self.find_in_self_or_refs(move |metadata| metadata.get_variable(id))
    }

    fn find_constant(&self, name: &StringPath) -> Option<&ConstInfo> {
        self.find_in_self_or_refs(move |metadata| metadata.find_constant(name))
    }

    fn constants(&self) -> impl Iterator<Item=&ConstInfo> {
        self.iter_in_self_or_refs(move |metadata| metadata.constants())
    }
}