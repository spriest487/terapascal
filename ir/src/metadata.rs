mod builder;
pub(crate) mod ids;
mod source;
mod tags;
mod vars;

use crate::typeinfo::TypeInfo;
use crate::FunctionID;
use crate::FunctionInfo;
use crate::FunctionSig;
use crate::GlobalRef;
use crate::IRFormatter;
use crate::InterfaceDecl;
use crate::InterfaceDef;
use crate::InterfaceImpl;
use crate::MethodInfo;
use crate::NamePath;
use crate::RawInstructionFormatter;
use crate::Ref;
use crate::StaticClosureID;
use crate::StructDef;
use crate::StructIdentity;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::Value;
use crate::VariantDef;
use linked_hash_map::LinkedHashMap;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub use builder::MetadataBuilder;
pub use ids::*;
pub use source::MetadataSource;
pub use tags::TagInfo;
pub use vars::VariableInfo;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    type_decls: LinkedHashMap<TypeDefID, TypeDecl>,
    string_literals: LinkedHashMap<StringID, String>,
    ifaces: LinkedHashMap<InterfaceID, InterfaceDecl>,
    iface_impls: HashMap<Type, BTreeMap<InterfaceID, InterfaceImpl>>,

    variables: BTreeMap<VariableID, VariableInfo>,

    type_info: HashMap<Type, Rc<TypeInfo>>,
    function_info: LinkedHashMap<FunctionID, FunctionInfo>,

    // function pointer type ID -> closure class IDs
    closures: BTreeMap<TypeDefID, Vec<TypeDefID>>,

    function_static_closures: HashMap<FunctionID, StaticClosureID>,
}

impl Metadata {
    pub fn new() -> Self {
        let mut metadata = Self {
            type_decls: LinkedHashMap::new(),
            string_literals: LinkedHashMap::new(),
            ifaces: LinkedHashMap::new(),
            iface_impls: HashMap::new(),

            variables: BTreeMap::new(),

            function_info: LinkedHashMap::new(),

            closures: BTreeMap::new(),
            function_static_closures: HashMap::new(),

            type_info: HashMap::new(),
        };

        metadata
            .string_literals
            .insert(EMPTY_STRING_ID, String::new());

        for reserved_id in RESERVED_TYPES {
            metadata.type_decls.insert(reserved_id, TypeDecl::Reserved);
        }

        metadata
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
                            path.to_pretty_string(|ty| Cow::Owned(ty.to_pretty_string(self)))
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

        for (id, iface_decl) in &other.ifaces {
            if let Some(conflict) = self.ifaces.get(id) {
                Self::check_conflict(
                    "interface ID",
                    id,
                    Some(conflict.name()),
                    Some(iface_decl.name()),
                );
            }

            self.ifaces.insert(*id, iface_decl.clone());
        }

        for (impl_ty, other_impls) in &other.iface_impls {
            let impls = self
                .iface_impls
                .entry(impl_ty.clone())
                .or_insert_with(|| BTreeMap::new());

            for (iface_id, other_iface_impl) in other_impls {
                let iface_impl = impls
                    .entry(*iface_id)
                    .or_insert_with(|| InterfaceImpl::new(other_iface_impl.methods.len()));

                let conflict_name = format!("method of interface {}", iface_id);
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
                    func_info.global_name.as_ref(),
                    existing.global_name.as_ref(),
                );
            }
            self.function_info.insert(*id, func_info.clone());
        }

        for (id, var) in &other.variables {
            if let Some(old_def) = self.variables.get(id) {
                Self::check_conflict("variable ID", id, Some(&var.name), Some(&old_def.name));
            };

            self.variables.insert(*id, var.clone());
        }

        for (func_type_id, other_closure_ids) in &other.closures {
            let func_closures = self.closures.entry(*func_type_id).or_insert_with(Vec::new);

            for id in other_closure_ids {
                if !func_closures.contains(id) {
                    func_closures.push(*id);
                }
            }
        }

        for (func_id, static_closure) in &other.function_static_closures {
            if self.function_static_closures.contains_key(func_id) {
                panic!("duplicate static closure ID for function {func_id}");
            }

            self.function_static_closures
                .insert(*func_id, *static_closure);
        }

        for (ty, funcs) in &other.type_info {
            if self.type_info.contains_key(ty) {
                panic!(
                    "duplicate RTTI definitions for type {}",
                    self.pretty_ty_name(ty)
                );
            }

            self.type_info.insert(ty.clone(), funcs.clone());
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

    // find the struct used as the backing type for set types with at least the given number of bits
    // the same struct definition may represent multiple defined set types
    pub fn find_set_repr_struct(&self, bits: usize) -> Option<TypeDefID> {
        let mut result = None;
        let mut min_bits: Option<usize> = None;

        for (id, decl) in &self.type_decls {
            let TypeDecl::Def(TypeDef::Struct(struct_def)) = decl else {
                continue;
            };

            if let StructIdentity::SetFlags { bits: def_bits } = &struct_def.identity {
                if *def_bits < bits {
                    continue;
                }

                let is_min = match min_bits {
                    Some(prev_min) => *def_bits < prev_min,
                    None => true,
                };

                if is_min {
                    result = Some(*id);
                    min_bits = Some(*def_bits);
                }
            }
        }

        result
    }

    pub fn get_iface_def(&self, iface_id: InterfaceID) -> Option<&InterfaceDef> {
        match self.ifaces.get(&iface_id)? {
            InterfaceDecl::Def(def) => Some(def),
            InterfaceDecl::Forward(..) => None,
        }
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
            .find(|(_, t)| t.name == Some(type_name_str))?;

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

    pub fn find_function(&self, name: &NamePath) -> Option<FunctionID> {
        // do a linear search for now because we don't want to store a redundant map of names,
        // and a user can always make a hashmap themselves if looking up names this way is too slow.
        self.function_info.iter().find_map(|(id, func)| {
            let func_name = func.global_name.as_ref()?;
            if func_name == name { Some(*id) } else { None }
        })
    }

    pub fn func_desc(&self, id: FunctionID) -> Option<String> {
        self.function_info
            .get(&id)
            .and_then(|decl| decl.global_name.as_ref())
            .map(NamePath::to_string)
            .or_else(|| {
                self.iface_impls.iter().find_map(|(impl_ty, impls)| {
                    impls.iter().find_map(|(iface_id, iface_impl)| {
                        iface_impl.methods.iter().find_map(|(method, impl_id)| {
                            if *impl_id != id {
                                return None;
                            }

                            let iface_name =
                                iface_id.to_interface_ptr_type().to_pretty_string(self);

                            let mut desc = format!("impl of {}.", iface_name);
                            let _ = self.format_method(*iface_id, *method, &mut desc);
                            desc.push_str(" for ");
                            let _ = self.format_type(impl_ty, &mut desc);

                            Some(desc)
                        })
                    })
                })
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

    pub fn iface_name(&self, iface_id: InterfaceID) -> String {
        self.get_iface_def(iface_id)
            .map(|def| def.name.to_pretty_string(|ty| self.pretty_ty_name(ty)))
            .unwrap_or_else(|| format!("interface({})", iface_id))
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<'_, str> {
        match ty {
            Type::Struct(id) | Type::Variant(id) => match self.type_decls.get(id) {
                Some(TypeDecl::Forward(name)) => {
                    let pretty_name = name.to_pretty_string(|ty| self.pretty_ty_name(ty));
                    Cow::Owned(pretty_name)
                },
                Some(TypeDecl::Def(def)) => {
                    let pretty_name = def.to_pretty_string(|ty| self.pretty_ty_name(ty));
                    Cow::Owned(pretty_name)
                },
                Some(TypeDecl::Reserved) | None => Cow::Owned(id.to_string()),
            },

            Type::Array { element, dim } => {
                let elem_name = self.pretty_ty_name(element);
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

            Type::Function(func_ty_id) => Cow::Owned(match self.get_func_ptr_ty(*func_ty_id) {
                Some(sig) => self.pretty_func_sig(sig),
                None => format!("function pointer {}", *func_ty_id),
            }),

            Type::Pointer(ty) => Cow::Owned(format!("^{}", self.pretty_ty_name(ty))),
            Type::TempRef(ty) => Cow::Owned(format!("&{}", self.pretty_ty_name(ty))),

            ty => Cow::Owned(ty.to_string()),
        }
    }

    fn pretty_object_type_name(&self, id: &ObjectID) -> Cow<'_, str> {
        match id {
            ObjectID::Any => Cow::Borrowed("any"),

            ObjectID::Interface(iface_id) => Cow::Owned(self.iface_name(*iface_id)),

            ObjectID::Closure(func_ty_id) => Cow::Owned(match self.get_func_ptr_ty(*func_ty_id) {
                Some(sig) => format!("closure of {}", self.pretty_func_sig(sig)),
                None => format!("closure of {}", func_ty_id),
            }),

            ObjectID::Class(struct_id) => self.pretty_ty_name(&Type::Struct(*struct_id)),

            ObjectID::Array(element_type) => {
                Cow::Owned(format!("array of {}", self.pretty_ty_name(element_type)))
            },

            ObjectID::Box(element_type) => {
                Cow::Owned(format!("box of {}", self.pretty_ty_name(element_type)))
            },
        }
    }

    pub fn pretty_func_sig(&self, sig: &FunctionSig) -> String {
        let mut pretty = String::new();

        pretty.push_str("function(");

        for (i, param_ty) in sig.param_tys.iter().enumerate() {
            if i > 0 {
                pretty.push_str("; ");
            }

            pretty.push_str(self.pretty_ty_name(param_ty).as_ref());
        }

        pretty.push_str("): ");
        pretty.push_str(self.pretty_ty_name(&sig.return_ty).as_ref());

        pretty
    }

    pub fn is_defined(&self, ty: &Type) -> bool {
        let id = match ty {
            Type::Struct(id) | Type::Variant(id) | Type::Function(id) | Type::Flags(id, ..) => *id,

            Type::Object(virt_id) | Type::WeakObject(virt_id) => match virt_id {
                ObjectID::Class(id) | ObjectID::Closure(id) => *id,

                ObjectID::Interface(id) => {
                    return self.ifaces.contains_key(id);
                },

                ObjectID::Any | ObjectID::Array(..) | ObjectID::Box(..) => {
                    return true;
                },
            },

            _ => return true,
        };

        !self.type_decls[&id].is_forward()
    }

    pub fn get_static_closure(&self, func_id: FunctionID) -> Option<StaticClosureID> {
        self.function_static_closures.get(&func_id).cloned()
    }

    pub fn get_func_ptr_ty(&self, id: TypeDefID) -> Option<&FunctionSig> {
        self.type_decls.get(&id).and_then(|decl| match decl {
            TypeDecl::Def(TypeDef::Function(ptr_def)) => Some(ptr_def),
            _ => None,
        })
    }

    /// Find the method instance that implements the given interface method for `ty`
    pub fn find_virtual_impl(
        &self,
        ty: &Type,
        iface_id: InterfaceID,
        method: MethodID,
    ) -> Option<FunctionID> {
        let iface_impl = self.iface_impls.get(ty)?.get(&iface_id)?;

        iface_impl.methods.get(&method).cloned()
    }

    pub fn impls(&self, ty: &Type) -> Vec<InterfaceID> {
        let Some(impls) = self.iface_impls.get(ty) else {
            return Vec::new();
        };

        impls.keys().cloned().collect()
    }

    pub fn is_impl(&self, ty: &Type, iface_id: InterfaceID) -> bool {
        let Some(impls) = self.iface_impls.get(ty) else {
            return false;
        };

        impls.contains_key(&iface_id)
    }

    pub fn find_impls(&self, ty: &Type) -> Vec<(InterfaceID, &InterfaceImpl)> {
        let Some(impls) = self.iface_impls.get(ty) else {
            return Vec::new();
        };

        impls
            .iter()
            .map(|(iface_id, iface_impl)| (*iface_id, iface_impl))
            .collect()
    }

    pub fn insert_closure(&mut self, func_type_id: TypeDefID, closure_id: TypeDefID) {
        let closures = self.closures.entry(func_type_id).or_insert_with(Vec::new);

        closures.push(closure_id);
    }

    pub fn closures_by_function(&self) -> &BTreeMap<TypeDefID, Vec<TypeDefID>> {
        &self.closures
    }

    pub fn closures(&self) -> impl Iterator<Item = TypeDefID> {
        self.closures.values().flat_map(|ids| ids.iter().cloned())
    }

    pub fn find_closure_func_type_id(&self, closure_class_id: TypeDefID) -> Option<TypeDefID> {
        for (func_id, closure_class_ids) in &self.closures {
            if closure_class_ids.contains(&closure_class_id) {
                return Some(*func_id);
            }
        }
        None
    }

    // find the declared ID and definition of a struct. if the struct is only forward-declared
    // when this call is made, the definition part of the result will be None
    pub fn find_struct_def(&self, name: &NamePath) -> Option<(TypeDefID, &StructDef)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name() == Some(name) => {
                Some((*id, struct_def))
            },

            _ => None,
        })
    }

    pub fn find_variant_def(&self, name: &NamePath) -> Option<(TypeDefID, &VariantDef)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => {
                Some((*id, variant_def))
            },

            _ => None,
        })
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

impl IRFormatter for Metadata {
    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", self.pretty_ty_name(ty))
    }

    fn format_type_def(&self, id: TypeDefID, f: &mut dyn fmt::Write) -> fmt::Result {
        match self.type_decls.get(&id) {
            Some(TypeDecl::Def(def)) => {
                write!(f, "{}", def.to_pretty_string(|ty| self.pretty_ty_name(ty)))
            }
            _ => {
                write!(f, "{}", id)
            },
        }
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        match val {
            Value::Ref(r) => self.format_ref(r, f),
            Value::SizeOf(ty) => {
                write!(f, "sizeof({})", self.pretty_ty_name(ty))
            },
            _ => RawInstructionFormatter.format_val(val, f),
        }
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        match r {
            Ref::Field(field_ref) => {
                write!(f, "(")?;
                self.format_ref(&field_ref.instance, f)?;
                write!(f, " as ")?;
                self.format_type(&field_ref.instance_type, f)?;
                write!(f, ").")?;

                let struct_def = match &field_ref.instance_type {
                    Type::Struct(id) | Type::Flags(id) => self.get_struct_def(*id),
                    Type::Object(ObjectID::Class(id)) => self.get_struct_def(*id),
                    _ => None,
                };
                let field_name = struct_def
                    .and_then(|def| def.fields.get(&field_ref.field))
                    .and_then(|field_def| field_def.name.as_ref());

                if let Some(name) = field_name {
                    write!(f, "{}", name)
                } else {
                    write!(f, "{}", field_ref.field.0)
                }
            }

            Ref::Global(GlobalRef::StringLiteral(string_id)) => match self.get_string(*string_id) {
                Some(string_lit) => write!(f, "'{}'", string_lit.escape_default()),
                None => write!(f, "{}", r),
            },

            Ref::Global(GlobalRef::Function(id)) => {
                let func_name = self
                    .get_function_info(*id)
                    .and_then(|f| f.global_name.as_ref());

                match func_name {
                    Some(name) => write!(f, "{}", name),

                    None => {
                        let find_iface_impl =
                            self.iface_impls.iter().find_map(|(impl_ty, impls)| {
                                impls.iter().find_map(|(iface_id, iface_impl)| {
                                    let method_id = iface_impl.methods.iter().find_map(
                                        |(method_id, func_id)| {
                                            if *func_id == *id {
                                                Some(method_id)
                                            } else {
                                                None
                                            }
                                        },
                                    )?;

                                    let iface = self.get_iface_def(*iface_id).unwrap();
                                    let method = iface.get_method(*method_id).unwrap();

                                    Some((&iface.name, impl_ty, &method.name))
                                })
                            });

                        match find_iface_impl {
                            Some((iface_name, impl_ty, method_name)) => {
                                write!(f, "{}.{} impl for ", iface_name, method_name)?;
                                self.format_type(impl_ty, f)
                            },

                            None => write!(f, "{}", r),
                        }
                    },
                }
            },

            _ => RawInstructionFormatter.format_ref(r, f),
        }
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        let field_name = of_ty
            .as_struct()
            .or_else(|| match of_ty.rc_resource_class_id()? {
                ObjectID::Class(struct_id) => Some(*struct_id),
                _ => None,
            })
            .and_then(|struct_id| self.get_struct_def(struct_id))
            .and_then(|struct_def| struct_def.fields.get(&field))
            .and_then(|field| field.name.as_ref());

        match field_name {
            Some(name) => write!(f, "{}", name),
            _ => RawInstructionFormatter.format_field(of_ty, field, f),
        }
    }

    fn format_method(
        &self,
        iface_id: InterfaceID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        let iface = match self.get_iface_def(iface_id) {
            Some(iface) => iface,
            None => return RawInstructionFormatter.format_method(iface_id, method, f),
        };

        let method = match iface.get_method(method) {
            Some(method) => method,
            None => {
                return RawInstructionFormatter.format_method(iface_id, method, f);
            },
        };

        write!(f, "{}", method.name)
    }

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result {
        let case_name = match of_ty {
            Type::Variant(id) => self
                .get_variant_def(*id)
                .and_then(|variant| variant.cases.get(tag))
                .map(|case| &case.name),
            _ => None,
        };

        match case_name {
            Some(name) => write!(f, "{}", name),
            _ => RawInstructionFormatter.format_variant_case(of_ty, tag, f),
        }
    }
}

impl MetadataSource for Metadata {
    fn as_formatter(&self) -> &impl IRFormatter {
        self
    }

    fn get_string(&self, id: StringID) -> Option<&String> {
        self.string_literals.get(&id)
    }
    
    fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&StructDef> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Struct(s)) => Some(s),

            TypeDecl::Def(..) => None,
        }
    }

    fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Variant(v)) => Some(v),

            TypeDecl::Def(..) => None,
        }
    }

    fn type_defs(&self) -> impl Iterator<Item = (TypeDefID, &TypeDef)> {
        self.type_decls.iter().filter_map(|(id, decl)| match decl {
            TypeDecl::Def(def) => Some((*id, def)),

            TypeDecl::Reserved | TypeDecl::Forward(..) => None,
        })
    }

    fn get_type_decl(&self, id: TypeDefID) -> Option<&TypeDecl> {
        self.type_decls.get(&id)
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

    fn interfaces(&self) -> impl Iterator<Item=(InterfaceID, &InterfaceDef)> {
        self.ifaces
            .iter()
            .filter_map(|(id, iface_decl)| match iface_decl {
                InterfaceDecl::Def(iface_def) => Some((*id, iface_def)),
                InterfaceDecl::Forward(..) => None,
            })
    }

    fn methods(&self) -> impl Iterator<Item=&MethodInfo> {
        self.type_info.iter()
            .flat_map(|(_, type_info)| type_info.methods.iter())
    }

    fn find_variable(&self, name: &NamePath) -> Option<(VariableID, &VariableInfo)> {
        self.variables.iter().find_map(|(id, var_info)|
            (var_info.name == *name).then(|| {
                (*id, var_info)
            })
        )
    }

    fn get_variable(&self, id: VariableID) -> Option<&VariableInfo> {
        self.variables.get(&id)
    }
}
