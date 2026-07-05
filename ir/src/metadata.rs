mod builder;
pub(crate) mod ids;
mod source;
mod tags;
mod vars;
mod format;
mod collection;

pub use self::builder::MetadataBuilder;
pub use self::collection::MetadataCollection;
pub use self::ids::*;
pub use self::source::InterfaceMethodImplRef;
pub use self::source::MetadataSource;
pub use self::tags::TagInfo;
pub use self::vars::ConstInfo;
pub use self::vars::VariableInfo;

use crate::typeinfo::TypeInfo;
use crate::FunctionID;
use crate::FunctionIdentity;
use crate::FunctionInfo;
use crate::DeclPath;
use crate::FunctionSig;
use crate::IRFormatter;
use crate::InterfaceDecl;
use crate::InterfaceDef;
use crate::InterfaceImpl;
use crate::MethodInfo;
use crate::NamePath;
use crate::StructDef;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::VariantDef;
use linked_hash_map::LinkedHashMap;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    type_decls: LinkedHashMap<TypeDefID, TypeDecl>,
    string_literals: LinkedHashMap<StringID, String>,
    interface_defs: LinkedHashMap<InterfaceID, InterfaceDecl>,
    iface_impls: HashMap<Type, HashMap<InterfaceRef, InterfaceImpl>>,

    variables: BTreeMap<VariableID, VariableInfo>,
    constants: HashMap<NamePath, ConstInfo>,

    type_info: HashMap<Type, Rc<TypeInfo>>,
    function_info: LinkedHashMap<FunctionID, FunctionInfo>,

    // function pointer type ID -> closure class IDs
    closures: HashMap<Rc<FunctionSig>, Vec<TypeDefID>>,
}

impl Metadata {
    pub fn new() -> Self {
        Self {
            type_decls: LinkedHashMap::new(),
            string_literals: LinkedHashMap::new(),
            interface_defs: LinkedHashMap::new(),
            iface_impls: HashMap::new(),

            variables: BTreeMap::new(),
            constants: HashMap::new(),

            function_info: LinkedHashMap::new(),

            closures: HashMap::new(),

            type_info: HashMap::new(),
        }
    }

    pub fn merge_from(&mut self, other: &Metadata) {
        for (id, decl) in &other.type_decls {
            match (self.type_decls.get(id), decl) {
                // any decl replaces an existing reserved ID
                (Some(TypeDecl::Reserved), new_decl) => {
                    self.type_decls.insert(*id, new_decl.clone());
                },

                // reserved ID does not replace any existing decl
                (Some(..), TypeDecl::Reserved) => {},

                // forward def replaced by a full def of the same name
                (Some(TypeDecl::Forward(forward_name)), TypeDecl::Def(new_def))
                    if new_def.name() == Some(forward_name) =>
                {
                    self.type_decls.insert(*id, TypeDecl::Def(new_def.clone()));
                },

                // forward def does not replace a full def of the same name
                (Some(TypeDecl::Def(old_def)), TypeDecl::Forward(new_name))
                    if old_def.name() == Some(new_name) => {},

                // error if forward def doesn't match actual def in either direction
                (Some(TypeDecl::Forward(forward_name)), TypeDecl::Def(def))
                | (Some(TypeDecl::Def(def)), TypeDecl::Forward(forward_name))
                    if def.name() != Some(forward_name) =>
                {
                    let def_name = def
                        .name()
                        .map(|path| {
                            path.to_pretty_string(self)
                        })
                        .unwrap_or_else(|| "<unnamed>".to_string());

                    panic!(
                        "mismatched forward type decl {id} in metadata (forward: {forward_name}, def: {def_name})"
                    );
                },

                (Some(conflict), decl) => {
                    Self::check_conflict("type ID", id, conflict.name(), decl.name());
                    self.type_decls.insert(*id, decl.clone());
                },

                // adding a new item
                (None, _) => {
                    self.type_decls.insert(*id, decl.clone());
                },
            }
        }

        for (id, string_lit) in &other.string_literals {
            if let Some(existing_str) = self.string_literals.get(id) {
                Self::check_conflict("string ID", id, Some(existing_str), Some(string_lit));
            }

            self.string_literals.insert(*id, string_lit.clone());
        }

        for (id, iface_decl) in &other.interface_defs {
            if let Some(conflict) = self.interface_defs.get(id) {
                Self::check_conflict(
                    "interface ID",
                    id,
                    Some(conflict.name()),
                    Some(iface_decl.name()),
                );
            }

            self.interface_defs.insert(*id, iface_decl.clone());
        }

        for (impl_ty, other_impls) in &other.iface_impls {
            let impls = self
                .iface_impls
                .entry(impl_ty.clone())
                .or_insert_with(Default::default);

            for (iface_ref, other_iface_impl) in other_impls {
                let iface_impl = impls
                    .entry(iface_ref.clone())
                    .or_insert_with(|| InterfaceImpl::new(other_iface_impl.methods.len()));

                let conflict_name = format!("method of interface {}", iface_ref);
                for (method_id, impl_func_id) in &other_iface_impl.methods {
                    if let Some(existing) = iface_impl.methods.get(method_id) {
                        if *existing != *impl_func_id {
                            panic!(
                                "incompatible function for {conflict_name}: was {existing}, linked metadata contains {impl_func_id}"
                            );
                        }
                    } else {
                        iface_impl.methods.insert(*method_id, *impl_func_id);
                    }
                }
            }
        }

        for (id, func_info) in &other.function_info {
            if self.function_info.contains_key(id) {
                let existing = &self.function_info[id];

                Self::check_conflict(
                    "function ID",
                    id,
                    Some(&func_info.identity),
                    Some(&existing.identity),
                );
            }
            self.function_info.insert(*id, func_info.clone());
        }

        for (id, var) in &other.variables {
            if let Some(old_def) = self.variables.get(id) {
                Self::check_conflict("variable ID", id, var.name.as_ref(), old_def.name.as_ref());
            };

            self.variables.insert(*id, var.clone());
        }

        for (sig, other_closure_ids) in &other.closures {
            let func_closures = self.closures
                .entry(sig.clone())
                .or_insert_with(Vec::new);

            for id in other_closure_ids {
                if !func_closures.contains(id) {
                    func_closures.push(*id);
                }
            }
        }

        for (ty, type_info) in &other.type_info {
            if let Some(existing) = self.type_info.get(ty) {
                if existing != type_info {
                    panic!("duplicate RTTI definitions for type {}", self.pretty_type_name(ty));
                }
            } else {
                self.type_info.insert(ty.clone(), type_info.clone());
            }
        }
    }

    fn check_conflict<T>(
        desc: &str,
        target: impl fmt::Display + ToString,
        old_name: Option<T>,
        new_name: Option<T>,
    ) where
        T: PartialEq + fmt::Display,
    {
        match (&old_name, &new_name) {
            (Some(old_name), Some(new_name)) if new_name == old_name => {},

            _ => {
                let a_name = old_name
                    .as_ref()
                    .map(ToString::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());
                let b_name = new_name
                    .as_ref()
                    .map(ToString::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());

                panic!("duplicate {desc} {target} in metadata (existing: {a_name}, new: {b_name})");
            },
        }
    }

    pub fn get_class_def(&self, id: TypeDefID) -> Option<&StructDef> {
        let decl = self.type_decls.get(&id)?;

        if let TypeDecl::Def(def) = decl
            && let TypeDef::Struct(struct_def) = def
            && struct_def.is_class()
        {
            Some(struct_def)
        } else {
            None
        }
    }

    pub fn class_defs(&self) -> impl Iterator<Item = (TypeDefID, &StructDef)> {
        self.type_decls.iter().filter_map(|(id, decl)| {
            if let TypeDecl::Def(def) = decl
                && let TypeDef::Struct(struct_def) = def
                && struct_def.is_class()
            {
                Some((*id, struct_def))
            } else {
                None
            }
        })
    }

    pub fn type_info(&self) -> impl Iterator<Item = (&Type, &Rc<TypeInfo>)> {
        self.type_info.iter()
    }

    pub fn find_runtime_method(
        &self,
        type_name: &str,
        method_name: &str,
    ) -> Option<(&Type, &Rc<TypeInfo>, usize)> {
        let type_name_str = self.find_string_id(type_name)?;

        let (ty, runtime_type) = self
            .type_info
            .iter()
            .find(|(_, t)| t.name == type_name_str)?;

        let method_name_str = self.find_string_id(method_name)?;

        let method_index = runtime_type
            .methods
            .iter()
            .position(|m| m.name == method_name_str)?;

        Some((ty, runtime_type, method_index))
    }

    pub fn get_runtime_methods(&self, ty: &Type) -> impl Iterator<Item = &MethodInfo> {
        self.type_info
            .get(ty)
            .into_iter()
            .flat_map(|src_ty| src_ty.methods.iter())
    }

    pub fn find_function(&self, name: &DeclPath) -> Option<FunctionID> {
        // do a linear search for now because we don't want to store a redundant map of names,
        // and a user can always make a hashmap themselves if looking up names this way is too slow.
        self.function_info.iter().find_map(|(id, func)| {
            let func_name = func.identity.global_name()?;
            if func_name == name { Some(*id) } else { None }
        })
    }

    pub fn variables(&self) -> impl Iterator<Item = (VariableID, &VariableInfo)> {
        self.variables
            .iter()
            .map(|(var_id, var)| (*var_id, var))
    }

    pub fn get_variable(&self, id: VariableID) -> Option<&VariableInfo> {
        self.variables.get(&id)
    }

    pub fn is_defined(&self, ty: &Type) -> bool {
        let id = match ty {
            Type::Struct(type_ref)
            | Type::Variant(type_ref) => {
                type_ref.def_id
            }

            Type::Object(object_id) | Type::WeakObject(object_id) => {
                match object_id {
                    ObjectID::Class(generic_id) => generic_id.def_id,

                    ObjectID::Interface(iface_ref) => {
                        return self.interface_defs.contains_key(&iface_ref.def_id);
                    }

                    | ObjectID::Any
                    | ObjectID::AnyClosure(..)
                    | ObjectID::Array(..)
                    | ObjectID::Box(..) => {
                        return true;
                    },
                }
            }

            _ => return true,
        };

        match self.type_decls.get(&id) {
            Some(decl) => !decl.is_forward(),
            None => false,
        }
    }

    pub fn insert_closure(&mut self, virtual_sig: impl Into<Rc<FunctionSig>>, closure_id: TypeDefID) {
        let closures = self.closures
            .entry(virtual_sig.into())
            .or_insert_with(Vec::new);

        closures.push(closure_id);
    }

    pub fn closures_by_sig(&self) -> &HashMap<Rc<FunctionSig>, Vec<TypeDefID>> {
        &self.closures
    }

    pub fn closures(&self) -> impl Iterator<Item = TypeDefID> {
        self.closures.values().flat_map(|ids| ids.iter().cloned())
    }

    pub fn find_closure_sig(&self, closure_class_id: TypeDefID) -> Option<Rc<FunctionSig>> {
        for (sig, closure_class_ids) in &self.closures {
            if closure_class_ids.contains(&closure_class_id) {
                return Some(sig.clone());
            }
        }
        None
    }

    pub fn find_string_id(&self, string: &str) -> Option<StringID> {
        self.string_literals.iter().find_map(|(id, string_lit)| {
            if string_lit == string {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn strings(&self) -> impl Iterator<Item = (StringID, &str)> + '_ {
        self.string_literals.iter().map(|(id, s)| (*id, s.as_str()))
    }
}

impl MetadataSource for Metadata {
    fn as_formatter(&self) -> &impl IRFormatter {
        self
    }

    fn get_string(&self, id: StringID) -> Option<&String> {
        self.string_literals.get(&id)
    }

    fn get_struct_def(&self, id: TypeDefID) -> Option<&StructDef> {
        match self.type_decls.get(&id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Struct(s)) => Some(s),

            TypeDecl::Def(..) => None,
        }
    }

    // find the declared ID and definition of a struct. if the struct is only forward-declared
    // when this call is made, the definition part of the result will be None
    fn find_struct_def(&self, name: &NamePath) -> Option<(TypeDefID, &StructDef)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name() == Some(name) => {
                Some((*id, struct_def))
            },

            _ => None,
        })
    }

    fn get_variant_def(&self, id: TypeDefID) -> Option<&VariantDef> {
        match self.type_decls.get(&id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Variant(v)) => Some(v),

            TypeDecl::Def(..) => None,
        }
    }

    fn find_variant_def(&self, name: &NamePath) -> Option<(TypeDefID, &VariantDef)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => {
                Some((*id, variant_def))
            },

            _ => None,
        })
    }

    fn type_decls(&self) -> impl Iterator<Item=(TypeDefID, &TypeDecl)> {
        self.type_decls.iter().map(|(id, decl)| (*id, decl))
    }

    fn get_type_decl(&self, id: TypeDefID) -> Option<&TypeDecl> {
        self.type_decls.get(&id)
    }

    fn get_type_def(&self, id: TypeDefID) -> Option<&TypeDef> {
        let decl = self.type_decls.get(&id)?;

        match decl {
            TypeDecl::Def(def) => Some(def),
            _ => None,
        }
    }

    fn get_type_name(&self, id: TypeDefID) -> Option<&NamePath> {
        let decl = self.get_type_decl(id)?;
        decl.name()
    }

    fn find_type_decl(&self, name: &NamePath) -> Option<TypeDefID> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name() == Some(name) => {
                Some(*id)
            },

            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => Some(*id),

            TypeDecl::Forward(forward_name) if *forward_name == *name => Some(*id),

            _ => None,
        })
    }

    fn get_type_info(&self, of_type: &Type) -> Option<Rc<TypeInfo>> {
        self.type_info.get(of_type).cloned()
    }

    fn functions(&self) -> impl Iterator<Item = (FunctionID, &FunctionInfo)> + use<'_> {
        self.function_info.iter().map(|(id, decl)| (*id, decl))
    }

    fn get_function_info(&self, id: FunctionID) -> Option<&FunctionInfo> {
        self.function_info.get(&id)
    }

    fn interface_defs(&self) -> impl Iterator<Item=(InterfaceID, &InterfaceDef)> {
        self.interface_defs
            .iter()
            .filter_map(|(id, iface_decl)| match iface_decl {
                InterfaceDecl::Def(iface_def) => Some((*id, iface_def)),
                InterfaceDecl::Forward(..) => None,
            })
    }

    fn get_interface_def(&self, iface_id: InterfaceID) -> Option<&InterfaceDef> {
        match self.interface_defs.get(&iface_id)? {
            InterfaceDecl::Def(def) => Some(def),
            InterfaceDecl::Forward(..) => None,
        }
    }

    fn is_impl(&self, ty: &Type, iface_id: &InterfaceRef) -> bool {
        let Some(impls) = self.iface_impls.get(ty) else {
            return false;
        };

        impls.contains_key(&iface_id)
    }

    fn type_impls(&self, ty: &Type) -> Vec<(&InterfaceRef, &InterfaceImpl)> {
        let Some(impls) = self.iface_impls.get(ty) else {
            return Vec::new();
        };

        impls
            .iter()
            .map(|(iface_ref, iface_impl)| (iface_ref, iface_impl))
            .collect()
    }

    fn iface_impls(&self) -> impl Iterator<Item=(&Type, impl Iterator<Item=(&InterfaceRef, &InterfaceImpl)>)> {
        self.iface_impls
            .iter()
            .map(|(ty, impls)| (ty, impls.iter()))
    }

    fn find_impl(&'_ self, func_id: FunctionID) -> Option<InterfaceMethodImplRef<'_>> {
        for (impl_type, impls) in &self.iface_impls {
            for (iface_ref, iface_impl) in impls {
                for (method_id, method_func_id) in &iface_impl.methods {
                    if *method_func_id == func_id {
                        let iface = self.get_interface_def(iface_ref.def_id).unwrap();
                        let method = iface.get_method(*method_id).unwrap();

                        return Some(InterfaceMethodImplRef {
                            interface: &iface.name,
                            impl_type,
                            method_name: method.name.as_str(),
                        });
                    }
                }
            }
        }

        None
    }

    /// Find the method instance that implements the given interface method for `ty`
    fn get_interface_method(
        &self,
        ty: &Type,
        iface_ref: &InterfaceRef,
        method: MethodID,
    ) -> Option<FunctionID> {
        let type_impls = self.iface_impls.get(ty)?;
        let iface_impl = type_impls.get(iface_ref)?;

        iface_impl.methods.get(&method).cloned()
    }

    fn methods(&self) -> impl Iterator<Item=&MethodInfo> {
        self.type_info.iter()
            .flat_map(|(_, type_info)| type_info.methods.iter())
    }

    fn get_dtor_method(&self, for_type: &Type) -> Option<FunctionID> {
        self.functions().find_map(|(id, def)| {
            match &def.identity {
                FunctionIdentity::Destructor {
                    declaring_type, ..
                } if declaring_type == for_type => {
                    Some(id)
                }
                _ => None,
            }
        })
    }

    fn find_variable(&self, name: &NamePath) -> Option<(VariableID, &VariableInfo)> {
        self.variables.iter().find_map(|(id, var_info)| {
            let Some(var_name) = var_info.name.as_ref() else {
                return None;
            };

            if *var_name != *name {
                return None;
            }

            Some((*id, var_info))
        })
    }

    fn get_variable(&self, id: VariableID) -> Option<&VariableInfo> {
        self.variables.get(&id)
    }

    fn find_constant(&self, name: &NamePath) -> Option<&ConstInfo> {
        self.constants.get(name)
    }

    fn constants(&self) -> impl Iterator<Item=&ConstInfo> {
        self.constants.values()
    }
}