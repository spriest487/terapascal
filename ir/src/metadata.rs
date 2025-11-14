mod builder;

use crate::rtti::RuntimeType;
use crate::ty::FieldID;
use crate::ty::VirtualTypeID;
use crate::ty_decl::TagLocation;
use crate::FunctionID;
use crate::FunctionSig;
use crate::GlobalRef;
use crate::IRFormatter;
use crate::InterfaceDecl;
use crate::InterfaceDef;
use crate::InterfaceID;
use crate::InterfaceImpl;
use crate::NamePath;
use crate::RawInstructionFormatter;
use crate::Ref;
use crate::RuntimeMethod;
use crate::SetAliasDef;
use crate::SetAliasID;
use crate::StaticClosureID;
use crate::StructDef;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::TypeDefID;
use crate::Value;
use crate::VariantDef;
use crate::FunctionDecl;
use linked_hash_map::LinkedHashMap;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub use builder::MetadataBuilder;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct StringID(pub usize);

impl fmt::Display for StringID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "string literal #{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct MethodID(pub usize);

impl fmt::Display for InterfaceID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct VariableID(pub usize);

impl fmt::Display for VariableID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub const EMPTY_STRING_ID: StringID = StringID(0);

pub const STRING_ID: TypeDefID = TypeDefID(1);
pub const STRING_VTYPE_ID: VirtualTypeID = VirtualTypeID::Class(STRING_ID);
pub const STRING_TYPE: Type = Type::RcPointer(STRING_VTYPE_ID);
pub const STRING_CHARS_FIELD: FieldID = FieldID(0);
pub const STRING_LEN_FIELD: FieldID = FieldID(1);

pub const CLOSURE_PTR_FIELD: FieldID = FieldID(0);

pub const TYPEINFO_ID: TypeDefID = TypeDefID(2);
pub const TYPEINFO_VTYPE_ID: VirtualTypeID = VirtualTypeID::Class(TYPEINFO_ID);
pub const TYPEINFO_TYPE: Type = Type::RcPointer(TYPEINFO_VTYPE_ID);
pub const TYPEINFO_NAME_FIELD: FieldID = FieldID(0);
pub const TYPEINFO_METHODS_FIELD: FieldID = FieldID(1);
pub const TYPEINFO_TAGS_FIELD: FieldID = FieldID(2);

pub const METHODINFO_ID: TypeDefID = TypeDefID(3);
pub const METHODINFO_VTYPE_ID: VirtualTypeID = VirtualTypeID::Class(METHODINFO_ID);
pub const METHODINFO_TYPE: Type = Type::RcPointer(METHODINFO_VTYPE_ID);
pub const METHODINFO_NAME_FIELD: FieldID = FieldID(0);
pub const METHODINFO_OWNER_FIELD: FieldID = FieldID(1);
pub const METHODINFO_IMPL_FIELD: FieldID = FieldID(2);
pub const METHODINFO_TAGS_FIELD: FieldID = FieldID(3);

pub const FUNCINFO_ID: TypeDefID = TypeDefID(4);
pub const FUNCINFO_VTYPE_ID: VirtualTypeID = VirtualTypeID::Class(FUNCINFO_ID);
pub const FUNCINFO_TYPE: Type = Type::RcPointer(FUNCINFO_VTYPE_ID);
pub const FUNCINFO_NAME_FIELD: FieldID = FieldID(0);
pub const FUNCINFO_IMPL_FIELD: FieldID = FieldID(1);
pub const FUNCINFO_TAGS_FIELD: FieldID = FieldID(2);

pub const ANY_TYPE: Type = Type::RcPointer(VirtualTypeID::Any);

pub const RESERVED_TYPES: [TypeDefID; 4] = [
    STRING_ID,
    TYPEINFO_ID,
    METHODINFO_ID,
    FUNCINFO_ID,
];

pub const RESERVED_STRINGS: [StringID; 1] = [
    EMPTY_STRING_ID,
];

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    type_decls: LinkedHashMap<TypeDefID, TypeDecl>,
    string_literals: LinkedHashMap<StringID, String>,
    ifaces: LinkedHashMap<InterfaceID, InterfaceDecl>,

    variables: BTreeMap<VariableID, Type>,

    set_aliases: LinkedHashMap<SetAliasID, SetAliasDef>,

    functions: LinkedHashMap<FunctionID, Rc<FunctionDecl>>,

    // function pointer type ID -> closure class IDs
    closures: BTreeMap<TypeDefID, Vec<TypeDefID>>,

    function_static_closures: HashMap<FunctionID, StaticClosureID>,

    runtime_types: HashMap<Type, Rc<RuntimeType>>,

    tag_counts: HashMap<TagLocation, usize>,
}

impl Metadata {
    pub fn new() -> Self {
        let mut metadata = Self {
            type_decls: LinkedHashMap::new(),
            string_literals: LinkedHashMap::new(),
            ifaces: LinkedHashMap::new(),
            
            variables: BTreeMap::new(),

            set_aliases: LinkedHashMap::new(),

            functions: LinkedHashMap::new(),

            closures: BTreeMap::new(),
            function_static_closures: HashMap::new(),

            runtime_types: HashMap::new(),

            tag_counts: HashMap::new(),
        };
        
        metadata.string_literals.insert(EMPTY_STRING_ID, String::new());

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
                }

                // reserved ID does not replace any existing decl
                (Some(..), TypeDecl::Reserved) => {}

                // forward def replaced by a full def of the same name 
                (Some(TypeDecl::Forward(forward_name)), TypeDecl::Def(new_def))
                if new_def.name() == Some(forward_name) => {
                    self.type_decls.insert(*id, TypeDecl::Def(new_def.clone()));
                }

                // forward def does not replace a full def of the same name    
                (Some(TypeDecl::Def(old_def)), TypeDecl::Forward(new_name))
                if old_def.name() == Some(new_name) => {}

                // error if forward def doesn't match actual def in either direction 
                (Some(TypeDecl::Forward(forward_name)), TypeDecl::Def(def))
                | (Some(TypeDecl::Def(def)), TypeDecl::Forward(forward_name))
                if def.name() != Some(forward_name) => {
                    let def_name = def.name()
                        .map(|path| path.to_pretty_string(|ty| Cow::Owned(ty.to_pretty_string(self))))
                        .unwrap_or_else(|| "<unnamed>".to_string());

                    panic!("mismatched forward type decl {id} in metadata (forward: {forward_name}, def: {def_name})");
                }

                (Some(conflict), decl) => {
                    Self::check_conflict("type ID", id, conflict.name(), decl.name());
                    self.type_decls.insert(*id, decl.clone());
                }

                // adding a new item
                (None, _) => {
                    self.type_decls.insert(*id, decl.clone());
                }
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
                Self::check_conflict("interface ID", id, Some(conflict.name()), Some(iface_decl.name()));
            }

            self.ifaces.insert(*id, iface_decl.clone());
        }

        for (id, func_decl) in &other.functions {
            if self.functions.contains_key(id) {
                let existing = &self.functions[id];

                Self::check_conflict("function ID", id, func_decl.global_name.as_ref(), existing.global_name.as_ref());
            }
            self.functions.insert(*id, func_decl.clone());
        }

        for (func_type_id, other_closure_ids) in &other.closures {
            let func_closures = self.closures.entry(*func_type_id)
                .or_insert_with(Vec::new);
            
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

            self.function_static_closures.insert(*func_id, *static_closure);
        }

        for (ty, funcs) in &other.runtime_types {
            if self.runtime_types.contains_key(ty) {
                panic!("duplicate RTTI definitions for type {}", self.pretty_ty_name(ty));
            }

            self.runtime_types.insert(ty.clone(), funcs.clone());
        }
        
        for (id, def) in &other.set_aliases {
            if !self.set_aliases.contains_key(id) {
                self.set_aliases.insert(*id, def.clone());
            }
        }
        
        for (loc, count) in &other.tag_counts {
            if !self.tag_counts.contains_key(loc) {
                self.tag_counts.insert(*loc, *count);
            }
        }
    }

    fn check_conflict<T>(
        desc: &str,
        target: impl fmt::Display + ToString,
        old_name: Option<T>,
        new_name: Option<T>,
    )
    where T: PartialEq + fmt::Display
    {
        match (&old_name, &new_name) {
            (Some(old_name), Some(new_name)) if new_name == old_name => {
            }

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
            }
        }
    }

    pub fn type_defs(&self) -> impl Iterator<Item = (TypeDefID, &TypeDef)> {
        self.type_decls
            .iter()
            .filter_map(|(id, decl)| match decl {
                TypeDecl::Def(def) => Some((*id, def)),
    
                TypeDecl::Reserved | TypeDecl::Forward(..) => None,
            })
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
        self.type_decls.iter()
            .filter_map(|(id, decl)| {
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
    
    pub fn set_alias_defs(&self) -> impl Iterator<Item = (SetAliasID, &SetAliasDef)> {
        self.set_aliases
            .iter()
            .map(|(id, def)| {
                (*id, def)
            })
    }

    pub fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&StructDef> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Struct(s)) => Some(s),

            TypeDecl::Def(..) => None,
        }
    }

    pub fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef> {
        match self.type_decls.get(&struct_id)? {
            TypeDecl::Reserved | TypeDecl::Forward(..) => None,

            TypeDecl::Def(TypeDef::Variant(v)) => Some(v),

            TypeDecl::Def(..) => None,
        }
    }

    pub fn get_iface_def(&self, iface_id: InterfaceID) -> Option<&InterfaceDef> {
        match self.ifaces.get(&iface_id)? {
            InterfaceDecl::Def(def) => Some(def),
            InterfaceDecl::Forward(..) => None,
        }
    }

    pub fn get_runtime_type(&self, ty: &Type) -> Option<Rc<RuntimeType>> {
        self.runtime_types.get(ty).cloned()
    }

    pub fn runtime_types(&self) -> impl Iterator<Item = (&Type, &Rc<RuntimeType>)> {
        self.runtime_types.iter()
    }

    pub fn find_runtime_method(&self, type_name: &str, method_name: &str) -> Option<(&Type, &Rc<RuntimeType>, usize)> {
        let type_name_str = self.find_string_id(type_name)?;
        
        let (ty, runtime_type) = self.runtime_types
            .iter()
            .find(|(_, t)| t.name == Some(type_name_str))?;
        
        let method_name_str = self.find_string_id(method_name)?;

        let method_index = runtime_type.methods
            .iter()
            .position(|m| m.name == method_name_str)?;
        
        Some((ty, runtime_type, method_index))
    }

    pub fn get_runtime_methods(&self, ty: &Type) -> impl Iterator<Item=&RuntimeMethod> {
        self.runtime_types
            .get(ty)
            .into_iter()
            .flat_map(|src_ty| src_ty.methods.iter())
    }

    pub fn functions(&self) -> impl Iterator<Item=(FunctionID, &Rc<FunctionDecl>)> + use<'_> {
        self.functions
            .iter()
            .map(|(id, decl)| (*id, decl))
    }

    pub fn find_function(&self, name: &NamePath) -> Option<FunctionID> {
        // do a linear search for now because we don't want to store a redundant map of names,
        // and a user can always make a hashmap themselves if looking up names this way is too slow.
        // this should probabl
        self.functions
            .iter()
            .find_map(|(id, func)| {
                let func_name = func.global_name.as_ref()?;
                if func_name == name {
                    Some(*id)
                } else {
                    None
                }
            })
    }

    pub fn get_function(&self, id: FunctionID) -> Option<&Rc<FunctionDecl>> {
        self.functions.get(&id)
    }

    pub fn func_desc(&self, id: FunctionID) -> Option<String> {
        self.functions
            .get(&id)
            .and_then(|decl| decl.global_name.as_ref())
            .map(NamePath::to_string)
            .or_else(|| {
                self.ifaces().find_map(|(iface_id, iface)| {
                    iface.impls.iter().find_map(|(impl_ty, iface_impl)| {
                        iface_impl.methods.iter().find_map(|(method, impl_id)| {
                            if *impl_id == id {
                                let mut desc = format!("impl of {}.", iface.name);
                                let _ = self.format_method(iface_id, *method, &mut desc);
                                desc.push_str(" for ");
                                let _ = self.format_type(impl_ty, &mut desc);

                                Some(desc)
                            } else {
                                None
                            }
                        })
                    })
                })
            })
    }
    
    pub fn variables(&self) -> impl Iterator<Item=(VariableID, &Type)> {
        self.variables
            .iter()
            .map(|(var_id, var_ty)| (*var_id, var_ty))
    }

    pub fn get_variable_type(&self, id: VariableID) -> Option<&Type> {
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
            
            Type::RcWeakPointer(class_id) => {
                let resource_name = self.pretty_virtual_type_name(class_id);
                Cow::Owned(format!("*weak {}", resource_name))
            }

            Type::RcPointer(class_id) => {
                let resource_name = self.pretty_virtual_type_name(class_id);
                Cow::Owned(format!("*{}", resource_name))
            },

            Type::Function(func_ty_id) => {
                Cow::Owned(match self.get_func_ptr_ty(*func_ty_id) {
                    Some(sig) => self.pretty_func_sig(sig),
                    None => format!("function {}", *func_ty_id),
                })
            }

            Type::Pointer(ty) => Cow::Owned(format!("^{}", self.pretty_ty_name(ty))),
            Type::TempRef(ty) => Cow::Owned(format!("&{}", self.pretty_ty_name(ty))),

            ty => Cow::Owned(ty.to_string()),
        }
    }
    
    fn pretty_virtual_type_name(&self, id: &VirtualTypeID) -> Cow<'_, str> {
        match id {
            VirtualTypeID::Any => Cow::Borrowed("any"),

            VirtualTypeID::Interface(iface_id) => {
                Cow::Owned(self.iface_name(*iface_id))
            },

            VirtualTypeID::Closure(func_ty_id) => {
                Cow::Owned(match self.get_func_ptr_ty(*func_ty_id) {
                    Some(sig) => format!("closure of {}", self.pretty_func_sig(sig)),
                    None => format!("closure of {}", func_ty_id),
                })
            }

            VirtualTypeID::Class(struct_id) => {
                self.pretty_ty_name(&Type::Struct(*struct_id))
            },
            
            VirtualTypeID::Array(element_type) => {
                Cow::Owned(format!("array of {}", self.pretty_ty_name(element_type)))
            }
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
            Type::Struct(id)
            | Type::Variant(id)
            | Type::Function(id)
            | Type::Flags(id, ..) => *id,

            Type::RcPointer(virt_id) | Type::RcWeakPointer(virt_id) => {
                match virt_id {
                    VirtualTypeID::Class(id)
                    | VirtualTypeID::Closure(id) => *id,
                    
                    VirtualTypeID::Interface(id) => {
                        return self.ifaces.contains_key(id);
                    },

                    VirtualTypeID::Any | VirtualTypeID::Array(..) => {
                        return true;
                    }
                }
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

    pub fn ifaces(&self) -> impl Iterator<Item = (InterfaceID, &InterfaceDef)> {
        self.ifaces
            .iter()
            .filter_map(|(id, iface_decl)| match iface_decl {
                InterfaceDecl::Def(iface_def) => Some((*id, iface_def)),
                InterfaceDecl::Forward(..) => None,
            })
    }

    /// Find the method instance that implements the given interface method for `ty`
    pub fn find_virtual_impl(
        &self,
        ty: &Type,
        iface_id: InterfaceID,
        method: MethodID,
    ) -> Option<FunctionID> {
        let iface = self.get_iface_def(iface_id)?;
        let ty_impl = iface.impls.get(ty)?;

        ty_impl.methods.get(&method).cloned()
    }

    pub fn impls(&self, ty: &Type) -> Vec<InterfaceID> {
        self.ifaces
            .iter()
            .filter_map(|(id, _decl)| {
                if self.is_impl(ty, *id) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn is_impl(&self, ty: &Type, iface_id: InterfaceID) -> bool {
        let impls = &self.get_iface_def(iface_id).unwrap().impls;
        impls.contains_key(ty)
    }

    pub fn find_impls(&self, ty: &Type) -> Vec<(InterfaceID, &InterfaceImpl)> {
        self.ifaces()
            .filter_map(|(id, iface)| {
                let impl_for_ty = iface.impls.get(ty)?;
                Some((id, impl_for_ty))
            })
            .collect()
    }

    pub fn insert_closure(&mut self, func_type_id: TypeDefID, closure_id: TypeDefID) {
        let closures = self.closures
            .entry(func_type_id)
            .or_insert_with(Vec::new);
        
        closures.push(closure_id);
    }

    pub fn closures_by_function(&self) -> &BTreeMap<TypeDefID, Vec<TypeDefID>> {
        &self.closures
    }

    pub fn closures(&self) -> impl Iterator<Item=TypeDefID> {
        self.closures
            .values()
            .flat_map(|ids| ids.iter().cloned())
    }

    pub fn find_type_decl(&self, name: &NamePath) -> Option<TypeDefID> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name() == Some(name) =>
                {
                    Some(*id)
                },

            TypeDecl::Def(TypeDef::Variant(variant_def)) if variant_def.name == *name => Some(*id),

            TypeDecl::Forward(forward_name) if *forward_name == *name => Some(*id),

            _ => None,
        })
    }

    // find the declared ID and definition of a struct. if the struct is only forward-declared
    // when this call is made, the definition part of the result will be None
    pub fn find_struct_def(&self, name: &NamePath) -> Option<(TypeDefID, &StructDef)> {
        self.type_decls.iter().find_map(|(id, def)| match def {
            TypeDecl::Def(TypeDef::Struct(struct_def)) if struct_def.name() == Some(name) =>
                {
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
    
    pub fn find_set_def(&self, name: &NamePath) -> Option<(SetAliasID, &SetAliasDef)> {
        self.set_aliases
            .iter()
            .find_map(|(id, def)| {
                match &def.name {
                    Some(def_name) if def_name == name => Some((*id, def)),
                    _ => None,
                }
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

    pub fn get_string(&self, id: StringID) -> Option<&String> {
        self.string_literals.get(&id)
    }

    pub fn strings(&self) -> impl Iterator<Item = (StringID, &str)> + '_ {
        self.string_literals.iter().map(|(id, s)| (*id, s.as_str()))
    }
    
    pub fn tag_counts(&self) -> impl Iterator<Item=(TagLocation, usize)> + use<'_> {
        self.tag_counts
            .iter()
            .map(|(loc, count)| (*loc, *count))
    }
}

impl IRFormatter for Metadata {
    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", self.pretty_ty_name(ty))
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        match val {
            Value::Ref(r) => self.format_ref(r, f),
            Value::SizeOf(ty) => {
                write!(f, "sizeof({})", self.pretty_ty_name(ty))
            }
            _ => RawInstructionFormatter.format_val(val, f),
        }
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        match r {
            Ref::Global(GlobalRef::StringLiteral(string_id)) => match self.get_string(*string_id) {
                Some(string_lit) => write!(f, "'{}'", string_lit.escape_default()),
                None => write!(f, "{}", r),
            },

            Ref::Global(GlobalRef::Function(id)) => {
                let func_name = self.get_function(*id).and_then(|f| f.global_name.as_ref());

                match func_name {
                    Some(name) => write!(f, "{}", name),

                    None => {
                        let find_iface_impl = self.ifaces().find_map(|(_id, iface)| {
                            iface.impls.iter().find_map(|(impl_ty, iface_impl)| {
                                let method_id = iface_impl.methods.iter().find_map(
                                    |(method_id, func_id)| {
                                        if *func_id == *id {
                                            Some(method_id)
                                        } else {
                                            None
                                        }
                                    },
                                )?;

                                let method = iface.get_method(*method_id).unwrap();

                                Some((&iface.name, impl_ty, &method.name))
                            })
                        });

                        match find_iface_impl {
                            Some((iface_name, impl_ty, method_name)) => {
                                write!(f, "{}.{} impl for ", iface_name, method_name)?;
                                self.format_type(impl_ty, f)
                            }

                            None => write!(f, "{}", r),
                        }
                    }
                }
            }

            _ => RawInstructionFormatter.format_ref(r, f),
        }
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        let field_name = of_ty
            .as_struct()
            .or_else(|| match of_ty.rc_resource_class_id()? {
                VirtualTypeID::Class(struct_id) => Some(*struct_id),
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
            }
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
