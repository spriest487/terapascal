use crate::dep_sort::sort_defs;
use crate::rtti::DynArrayRuntimeType;
use crate::rtti::RuntimeType;
use crate::ty::FieldID;
use crate::ty::VirtualTypeID;
use crate::ty_decl::TagLocation;
use crate::FunctionDecl;
use crate::FunctionID;
use crate::FunctionSig;
use crate::GlobalRef;
use crate::InstructionFormatter;
use crate::Interface;
use crate::InterfaceDecl;
use crate::InterfaceID;
use crate::InterfaceImpl;
use crate::NamePath;
use crate::RawInstructionFormatter;
use crate::Ref;
use crate::RuntimeMethod;
use crate::SetAliasDef;
use crate::SetAliasID;
use crate::StaticClosureID;
use crate::Struct;
use crate::StructFieldDef;
use crate::StructIdentity;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::TypeDefID;
use crate::Value;
use crate::VariantDef;
use linked_hash_map::LinkedHashMap;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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

pub const DYNARRAY_LEN_FIELD: FieldID = FieldID(0);
pub const DYNARRAY_PTR_FIELD: FieldID = FieldID(1);

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

pub const BUILTIN_TYPE_DEFS: [Type; 2] = [
    STRING_TYPE,
    TYPEINFO_TYPE,
];

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Metadata {
    type_decls: LinkedHashMap<TypeDefID, TypeDecl>,
    string_literals: LinkedHashMap<StringID, String>,
    ifaces: LinkedHashMap<InterfaceID, InterfaceDecl>,

    dtors: BTreeMap<TypeDefID, FunctionID>,
    
    class_ids: BTreeSet<TypeDefID>,

    dyn_array_structs: LinkedHashMap<Type, TypeDefID>,

    functions: LinkedHashMap<FunctionID, Rc<FunctionDecl>>,
    set_aliases: LinkedHashMap<SetAliasID, SetAliasDef>,

    closures: Vec<TypeDefID>,
    function_static_closures: HashMap<FunctionID, StaticClosureID>,

    runtime_types: HashMap<Type, Rc<RuntimeType>>,
    dyn_array_runtime_types: HashMap<Type, DynArrayRuntimeType>,
    
    bounds_check_functions: HashMap<Type, FunctionID>,

    tag_counts: HashMap<TagLocation, usize>,
}

impl Metadata {
    pub fn new() -> Self {
        let mut metadata = Self {
            ..Default::default()
        };
        
        metadata.string_literals.insert(EMPTY_STRING_ID, String::new());

        metadata
    }

    pub fn extend(&mut self, other: &Metadata) {
        for (id, decl) in &other.type_decls {
            if let Some(conflict) = self.type_decls.get(id) {
                panic!(
                    "duplicate struct ID {} in metadata (new: {}, existing: {})",
                    id, decl, conflict,
                );
            };

            self.type_decls.insert(*id, decl.clone());
        }

        for (id, string_lit) in &other.string_literals {
            if let Some(existing_str) = self.string_literals.get(id) {
                if existing_str != string_lit {
                    panic!("duplicate string ID {} in metadata", id);    
                }
            }

            self.string_literals.insert(*id, string_lit.clone());
        }

        for (id, iface_decl) in &other.ifaces {
            if let Some(conflict) = self.ifaces.get(id) {
                panic!(
                    "duplicate iface ID {} in metadata (new: {}, existing: {})",
                    id,
                    iface_decl.name(),
                    conflict.name()
                );
            }

            self.ifaces.insert(*id, iface_decl.clone());
        }

        for (id, func_decl) in &other.functions {
            if self.functions.contains_key(id) {
                let existing = &self.functions[id];

                let name = func_decl
                    .global_name
                    .as_ref()
                    .map(NamePath::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());
                let existing_name = existing
                    .global_name
                    .as_ref()
                    .map(NamePath::to_string)
                    .unwrap_or_else(|| "<unnamed>".to_string());

                panic!(
                    "duplicate function ID {} in metadata (new: {}, existing: {})",
                    id, name, existing_name
                );
            }
            self.functions.insert(*id, func_decl.clone());
        }

        for (owning_ty_id, dtor_func) in &other.dtors {
            if self.dtors.contains_key(&owning_ty_id) {
                panic!("duplicate destructor definition for type {owning_ty_id}");
            }
            self.dtors.insert(*owning_ty_id, *dtor_func);
        }

        for (ty, funcs) in &other.runtime_types {
            if self.runtime_types.contains_key(ty) {
                panic!("duplicate rc boilerplate definitions for type {}", ty);
            }

            self.runtime_types.insert(ty.clone(), funcs.clone());
        }

        for (el_ty, struct_id) in &other.dyn_array_structs {
            if !self.dyn_array_structs.contains_key(el_ty) {
                self.dyn_array_structs.insert(el_ty.clone(), *struct_id);
            }
        }
        for (el_ty, runtime_ty) in &other.dyn_array_runtime_types {
            if !self.dyn_array_runtime_types.contains_key(el_ty) {
                self.dyn_array_runtime_types.insert(el_ty.clone(), runtime_ty.clone());
            }
        }
        
        for (element_ty, func_id) in &other.bounds_check_functions {
            if !self.bounds_check_functions.contains_key(element_ty) {
                self.bounds_check_functions.insert(element_ty.clone(), *func_id);
            }
        }
        
        for (id, def) in &other.set_aliases {
            if !self.set_aliases.contains_key(&id) {
                self.set_aliases.insert(*id, def.clone());
            }
        }
        
        for (loc, count) in &other.tag_counts {
            if !self.tag_counts.contains_key(loc) {
                self.tag_counts.insert(*loc, *count);
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

    pub fn class_defs(&self) -> impl Iterator<Item = (TypeDefID, &TypeDef)> {
        self.class_ids
            .iter()
            .filter_map(|id| match self.type_decls.get(id)? {
                TypeDecl::Def(def) => Some((*id, def)),

                TypeDecl::Reserved | TypeDecl::Forward(..) => None,
            })
    }
    
    pub fn set_alias_defs(&self) -> impl Iterator<Item = (SetAliasID, &SetAliasDef)> {
        self.set_aliases
            .iter()
            .map(|(id, def)| {
                (*id, def)
            })
    }

    pub fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&Struct> {
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

    pub fn get_iface_def(&self, iface_id: InterfaceID) -> Option<&Interface> {
        match self.ifaces.get(&iface_id)? {
            InterfaceDecl::Def(def) => Some(def),
            InterfaceDecl::Forward(..) => None,
        }
    }

    fn next_type_def_id(&mut self) -> TypeDefID {
        (1..)
            .map(TypeDefID)
            .find(|id| !self.type_decls.contains_key(id) && *id != STRING_ID)
            .unwrap()
    }

    fn next_iface_id(&mut self) -> InterfaceID {
        (1..)
            .map(InterfaceID)
            .find(|id| !self.ifaces.contains_key(id))
            .unwrap()
    }

    fn next_function_id(&mut self) -> FunctionID {
        (1..)
            .map(FunctionID)
            .find(|id| !self.functions.contains_key(id))
            .unwrap()
    }

    fn next_set_id(&mut self) -> SetAliasID {
        let id = self.set_aliases
            .iter()
            .last()
            .map(|(id, _)| id.0)
            .unwrap_or(0);

        SetAliasID(id + 1)
    }

    pub fn insert_func(&mut self, global_name: Option<NamePath>) -> FunctionID {
        let id = self.next_function_id();
        
        let runtime_name = global_name
            .as_ref()
            .cloned()
            .map(|name_path| {
                let name = name_path.to_string();
                
                self.find_or_insert_string(&name)
            });

        let decl = FunctionDecl { global_name, runtime_name };
        self.functions.insert(id, Rc::new(decl));

        id
    }
    
    pub fn get_bounds_check_func(&self, type_id: &Type) -> Option<FunctionID> {
        self.bounds_check_functions.get(type_id).cloned()
    }

    pub fn insert_runtime_type(&mut self, ty: Type, runtime_type: RuntimeType) -> Rc<RuntimeType> {
        let runtime_type = Rc::new(runtime_type);

        // it's valid to replace existing entries
        // getting the runtime type info right is the responsibility of the frontend 
        self.runtime_types.insert(ty, runtime_type.clone());
        
        runtime_type
    }

    pub fn declare_dynarray_runtime_type(&mut self, element_ty: &Type) -> DynArrayRuntimeType {
        if self.dyn_array_runtime_types.contains_key(element_ty) {
            panic!("duplicate rc boilerplate declaration for type {}", self.pretty_ty_name(element_ty));
        }

        let runtime_type = DynArrayRuntimeType {
            alloc: self.insert_func(None),
            length: self.insert_func(None),
        };

        self.dyn_array_runtime_types.insert(element_ty.clone(), runtime_type.clone());
        runtime_type
    }

    pub fn get_runtime_type(&self, ty: &Type) -> Option<Rc<RuntimeType>> {
        self.runtime_types.get(ty).cloned()
    }

    pub fn get_dynarray_runtime_type(&self, elem_ty: &Type) -> Option<DynArrayRuntimeType> {
        self.dyn_array_runtime_types.get(elem_ty).cloned()
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
        self.functions
            .iter()
            .find(|(_id, func)| func.global_name.as_ref() == Some(name))
            .map(|(id, _func)| *id)
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

    pub fn iface_name(&self, iface_id: InterfaceID) -> String {
        self.get_iface_def(iface_id)
            .map(|def| def.name.to_pretty_string(|ty| self.pretty_ty_name(ty)))
            .unwrap_or_else(|| format!("interface({})", iface_id))
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<str> {
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
                let resource_name = self.pretty_virtual_type_name(*class_id);
                Cow::Owned(format!("*weak {}", resource_name))
            }

            Type::RcPointer(class_id) => {
                let resource_name = self.pretty_virtual_type_name(*class_id);
                Cow::Owned(format!("*{}", resource_name))
            },

            Type::Function(func_ty_id) => {
                Cow::Owned(match self.get_func_ptr_ty(*func_ty_id) {
                    Some(sig) => self.pretty_func_sig(sig),
                    None => format!("function {}", *func_ty_id),
                })
            }

            Type::Pointer(ty) => Cow::Owned(format!("^{}", self.pretty_ty_name(ty))),

            ty => Cow::Owned(ty.to_string()),
        }
    }
    
    fn pretty_virtual_type_name(&self, id: VirtualTypeID) -> Cow<str> {
        match id {
            VirtualTypeID::Any => Cow::Borrowed("any"),

            VirtualTypeID::Interface(iface_id) => {
                Cow::Owned(self.iface_name(iface_id))
            },

            VirtualTypeID::Closure(func_ty_id) => {
                Cow::Owned(match self.get_func_ptr_ty(func_ty_id) {
                    Some(sig) => format!("closure of {}", self.pretty_func_sig(sig)),
                    None => format!("closure of {}", func_ty_id),
                })
            }

            VirtualTypeID::Class(struct_id) => {
                self.pretty_ty_name(&Type::Struct(struct_id))
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

    pub fn reserve_new_struct(&mut self) -> TypeDefID {
        let id = self.next_type_def_id();
        self.type_decls.insert(id, TypeDecl::Reserved);
        id
    }

    pub fn reserve_struct(&mut self, id: TypeDefID) {
        if self.type_decls.contains_key(&id) {
            panic!("reserving existing struct ID {}", id);
        }

        self.type_decls.insert(id, TypeDecl::Reserved);
    }

    // turn a reserved struct ID into a forward decl by name
    pub fn declare_struct(&mut self, id: TypeDefID, name: &NamePath, is_class: bool) {
        if is_class && self.class_ids.contains(&id) {
            panic!("class {id} is already declared (new declaration: {name})");
        }
        
        match &mut self.type_decls[&id] {
            reserved @ TypeDecl::Reserved => {
                *reserved = TypeDecl::Forward(name.clone());
            },

            TypeDecl::Forward(prev_name) => {
                assert_eq!(
                    prev_name, name,
                    "can't declare same struct multiple times with different names"
                );
            },

            TypeDecl::Def(def) => {
                assert_eq!(
                    def.name(),
                    Some(name),
                    "can't declare same struct multiple times with different names"
                );
            },
        }
        
        if is_class {
            self.class_ids.insert(id);
        }
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
                    VirtualTypeID::Interface(id) => return self.ifaces.contains_key(id),
                    VirtualTypeID::Any => return true,
                }
            },

            _ => return true,
        };
        
        !self.type_decls[&id].is_forward()
    }

    pub fn define_struct(&mut self, id: TypeDefID, struct_def: Struct) {
        match &self.type_decls[&id] {
            TypeDecl::Forward(name) => {
                assert_eq!(Some(name), struct_def.name());
                let type_def = TypeDecl::Def(TypeDef::Struct(struct_def));
                self.type_decls.insert(id, type_def);
            },

            TypeDecl::Reserved => {
                let type_def = TypeDecl::Def(TypeDef::Struct(struct_def));
                self.type_decls.insert(id, type_def);
            }

            _other => {
                panic!("expected named declaration to exist when defining {}", struct_def);
            },
        }
    }

    pub fn define_variant(&mut self, id: TypeDefID, variant_def: VariantDef) {
        match &mut self.type_decls[&id] {
            TypeDecl::Forward(name) => {
                assert_eq!(*name, variant_def.name);

                self.type_decls
                    .insert(id, TypeDecl::Def(TypeDef::Variant(variant_def)));
            },

            _other => {
                panic!(
                    "expected named declaration to exist when defining {}",
                    variant_def.name
                );
            },
        }
    }
    
    pub fn insert_static_closure(&mut self, func_id: FunctionID, closure: StaticClosureID) {
        let replaced = self.function_static_closures.insert(func_id, closure);
        assert!(replaced.is_none(), "static closure for function {func_id} must not have been inserted already");
    }

    pub fn get_static_closure(&self, p0: FunctionID) -> Option<StaticClosureID> {
        self.function_static_closures.get(&p0).cloned()
    }

    pub fn define_closure_ty(&mut self, id: TypeDefID, closure_def: Struct) {
        self.define_struct(id, closure_def);
        self.closures.push(id);
    }

    pub fn get_func_ptr_ty(&self, id: TypeDefID) -> Option<&FunctionSig> {
        self.type_decls.get(&id).and_then(|decl| match decl {
            TypeDecl::Def(TypeDef::Function(ptr_def)) => Some(ptr_def),
            _ => None,
        })
    }

    pub fn insert_type_decl(&mut self, decl: TypeDecl) -> TypeDefID {
        let id = self.next_type_def_id();

        let replaced = self.type_decls.insert(id, decl);
        assert!(replaced.is_none());

        id
    }

    pub fn ifaces(&self) -> impl Iterator<Item = (InterfaceID, &Interface)> {
        self.ifaces
            .iter()
            .filter_map(|(id, iface_decl)| match iface_decl {
                InterfaceDecl::Def(iface_def) => Some((*id, iface_def)),
                InterfaceDecl::Forward(..) => None,
            })
    }

    pub fn declare_iface(&mut self, name: &NamePath) -> InterfaceID {
        let existing = self.ifaces.iter().find_map(|(id, decl)| match decl {
            InterfaceDecl::Forward(decl_name) if decl_name == name => Some(*id),
            InterfaceDecl::Def(iface) if iface.name == *name => Some(*id),
            _ => None,
        });

        if let Some(existing) = existing {
            return existing;
        }

        let id = self.next_iface_id();
        self.ifaces.insert(id, InterfaceDecl::Forward(name.clone()));
        id
    }

    pub fn define_iface(&mut self, iface_def: Interface) -> InterfaceID {
        let id = self.declare_iface(&iface_def.name);

        self.ifaces.insert(id, InterfaceDecl::Def(iface_def));

        id
    }

    pub fn impl_method(
        &mut self,
        iface_id: InterfaceID,
        for_ty: Type,
        method_name: impl Into<String>,
        func_id: FunctionID,
    ) {
        let method_name = method_name.into();

        match self.ifaces.get_mut(&iface_id) {
            Some(InterfaceDecl::Def(iface_def)) => {
                let index = iface_def
                    .method_index(&method_name)
                    .unwrap_or_else(|| panic!("expected {} to contain method {}", iface_def.name, method_name));

                iface_def.add_impl(for_ty, index, func_id);
            },

            Some(InterfaceDecl::Forward(name)) => panic!(
                "trying to impl method {} for interface {} which isn't defined yet",
                method_name, name
            ),

            None => panic!(
                "trying to impl method {} for interface {} which doesn't exist",
                method_name, iface_id
            ),
        }
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
    
    pub fn insert_dtor(&mut self, owning_type: TypeDefID, dtor_func: FunctionID) {
        self.dtors.insert(owning_type, dtor_func);
    }
    
    pub fn find_dtor(&self, owning_type: TypeDefID) -> Option<FunctionID> {
        self.dtors.get(&owning_type).cloned()
    }

    pub fn find_dyn_array_struct(&self, element: &Type) -> Option<TypeDefID> {
        self.dyn_array_structs.get(element).cloned()
    }

    pub fn define_dyn_array_struct(&mut self, element: Type) -> TypeDefID {
        assert!(
            !self.dyn_array_structs.contains_key(&element),
            "duplicate IR struct definition for dynamic array with element {}",
            element
        );

        let mut fields = LinkedHashMap::new();
        fields.insert(
            DYNARRAY_LEN_FIELD,
            StructFieldDef {
                name: Some("len".to_string()),
                ty: Type::I32,
                rc: false,
            },
        );
        fields.insert(
            DYNARRAY_PTR_FIELD,
            StructFieldDef {
                name: Some("ptr".to_string()),
                ty: element.clone().ptr(),
                rc: false,
            },
        );

        let struct_id = self.next_type_def_id();
        self.type_decls.insert(
            struct_id,
            TypeDecl::Def(TypeDef::Struct(Struct {
                identity: StructIdentity::DynArray(element.clone()),
                fields,
            })),
        );
        
        self.class_ids.insert(struct_id);

        self.dyn_array_structs.insert(element.clone(), struct_id);

        // the rc boilerplate impls for a dynarray should be empty
        // dyn array structs are heap-allocated and don't need structural ref-counting
        // (but they do need custom finalization to clean up references they hold)
        let release_id = self.insert_func(None);

        let mut rtt = RuntimeType::new(None);
        rtt.release = Some(release_id);
        
        self.insert_runtime_type(Type::Struct(struct_id), rtt);
        self.declare_dynarray_runtime_type(&element);

        struct_id
    }

    pub fn dyn_array_structs(&self) -> &LinkedHashMap<Type, TypeDefID> {
        &self.dyn_array_structs
    }

    pub fn dyn_array_element_ty(&self, array_class_id: TypeDefID) -> Option<&Type> {
        let (el_ty, _) = self
            .dyn_array_structs()
            .iter()
            .find(|(_el_ty, struct_id)| array_class_id == **struct_id)?;

        Some(el_ty)
    }

    pub fn closures(&self) -> &[TypeDefID] {
        &self.closures
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
    pub fn find_struct_def(&self, name: &NamePath) -> Option<(TypeDefID, &Struct)> {
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
    
    pub fn define_set_type(&mut self, name: Option<NamePath>, flags_struct: TypeDefID) -> SetAliasID {
        let set_id = self.next_set_id();
        
        self.set_aliases.insert(set_id, SetAliasDef {
            name,
            flags_struct,
        });
        
        set_id
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        let existing =
            self.string_literals
                .iter()
                .find_map(|(id, literal)| if literal == s { Some(*id) } else { None });

        match existing {
            Some(id) => id,
            None => {
                let next_id = self
                    .string_literals
                    .keys()
                    .max_by_key(|id| id.0)
                    .map(|id| StringID(id.0 + 1))
                    .unwrap_or(StringID(0));
                self.string_literals.insert(next_id, s.to_string());
                next_id
            },
        }
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

    // hack: we don't always end up with types properly ordered by structural dependencies
    // as a result of the order we encounter types in, so this gets called to sort them before
    // finishing the module (assuming backends expect the types to be ordered e.g. like in C)
    pub fn sort_type_defs_by_deps(&mut self) {
        let mut unsorted = self.type_decls.clone();

        // remove all defs into a separate collection
        let mut defs = Vec::new();
        let mut decls = LinkedHashMap::new();

        while let Some((id, decl)) = unsorted.pop_front() {
            match decl {
                TypeDecl::Reserved => {
                    decls.insert(id, TypeDecl::Reserved);
                },
                TypeDecl::Forward(name) => {
                    decls.insert(id, TypeDecl::Forward(name));
                },

                TypeDecl::Def(def) => {
                    defs.push((id, def));
                },
            }
        }

        let sorted_defs = sort_defs(defs, self);

        self.type_decls = decls;
        for (id, def) in sorted_defs {
            self.type_decls.insert(id, TypeDecl::Def(def));
        }
    }

    pub fn alloc_tag_array(&mut self, loc: TagLocation, len: usize) {
        self.tag_counts.insert(loc, len);
    }
    
    pub fn tag_counts(&self) -> impl Iterator<Item=(TagLocation, usize)> + use<'_> {
        self.tag_counts
            .iter()
            .map(|(loc, count)| (*loc, *count))
    }
}

impl InstructionFormatter for Metadata {
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
                Some(string_lit) => write!(f, "'{}'", string_lit),
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
                VirtualTypeID::Class(struct_id) => Some(struct_id),
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
