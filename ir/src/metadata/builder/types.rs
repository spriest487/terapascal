use crate::FunctionID;
use crate::InterfaceDecl;
use crate::InterfaceDef;
use crate::InterfaceID;
use crate::InterfaceImpl;
use crate::MetadataBuilder;
use crate::MetadataCollection;
use crate::MetadataSource;
use crate::MethodID;
use crate::NamePath;
use crate::StructDef;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::TypeDefID;
use crate::TypeInfo;
use crate::VariantDef;
use linked_hash_map::Entry;
use std::collections::BTreeMap;
use std::rc::Rc;

impl MetadataBuilder {
    pub fn type_info(&self) -> impl Iterator<Item=(&Type, &Rc<TypeInfo>)> {
        self.iter_in_self_or_refs(move |metadata| metadata.type_info())
    }
    
    pub fn is_defined(&self, ty: &Type) -> bool {
        self.find_in_self_or_refs(move |metadata| metadata.is_defined(ty).then_some(true))
            .unwrap_or(false)
    }
    
    pub fn insert_type_decl(&mut self, id: TypeDefID, decl: TypeDecl) -> TypeDefID {
        let replaced = self.metadata.type_decls.insert(id, decl);
        match replaced {
            None | Some(TypeDecl::Reserved) => {
                self.next_type_id.0 += 1;

                id
            }

            Some(other) => {
                panic!("insert_type_decl: unexpectedly replacing type decl {}", other)
            }
        }
    }

    pub fn new_type(&mut self) -> TypeDefID {
        let id = self.next_type_id;
        self.metadata.type_decls.insert(id, TypeDecl::Reserved);

        self.next_type_id.0 += 1;
        id
    }

    /// If the type with this name already exists, return its ID, otherwise forward declare it
    pub fn forward_declare_type(&mut self, name: &NamePath) -> TypeDefID {
        if let Some(existing_id) = self.find_type_decl(name) {
            return existing_id;
        }

        let id = self.new_type();
        self.insert_type_decl(id, TypeDecl::Forward(name.clone()));

        id
    }

    pub fn reserve_type(&mut self, id: TypeDefID) {
        match self.metadata.type_decls.entry(id) {
            Entry::Occupied(entry) => {
                let existing = entry.get(); 
                if !matches!(existing, TypeDecl::Reserved) {
                    panic!("reserving existing type ID {} which is already in use ({})", id, existing);
                }
            }

            Entry::Vacant(entry) => {
                entry.insert(TypeDecl::Reserved);
            }
        }

        self.next_type_id.0 = usize::max(self.next_type_id.0, id.0 + 1);
    }

    // turn a reserved ID into a forward decl by name
    pub fn declare_type(&mut self, id: TypeDefID, name: &NamePath) {
        match &mut self.metadata.type_decls[&id] {
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
    }

    pub fn remove_type_def(&mut self, id: TypeDefID) -> bool {
        let removed = self.metadata.type_decls.remove(&id).is_some();

        removed
    }

    pub fn find_struct_def(&self, name_path: &NamePath) -> Option<(TypeDefID, &StructDef)> {
        self.find_in_self_or_refs(move |metadata| metadata.find_struct_def(name_path))
    }

    pub fn define_struct(&mut self, id: TypeDefID, struct_def: StructDef) {
        match self.metadata.type_decls.get(&id) {
            Some(TypeDecl::Forward(name)) => {
                assert_eq!(Some(name), struct_def.name());
                let type_def = TypeDecl::Def(TypeDef::Struct(struct_def));
                self.metadata.type_decls.insert(id, type_def);
            },

            Some(TypeDecl::Reserved) => {
                let type_def = TypeDecl::Def(TypeDef::Struct(struct_def));
                self.metadata.type_decls.insert(id, type_def);
            }

            None => {
                if id < self.next_type_id {
                    panic!("ID {id} is unavailable for type {struct_def}");
                }

                self.metadata.type_decls.insert(id, TypeDecl::Def(TypeDef::Struct(struct_def)));
            }

            Some(TypeDecl::Def(..)) => {
                panic!("already defined: {}", struct_def);
            }
        }
    }

    pub fn define_variant(&mut self, id: TypeDefID, variant_def: VariantDef) {
        match &mut self.metadata.type_decls[&id] {
            TypeDecl::Forward(name) => {
                assert_eq!(*name, variant_def.name);

                self.metadata.type_decls
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

    pub fn declare_iface(&mut self, name: &NamePath) -> InterfaceID {
        let existing = self.metadata.ifaces.iter().find_map(|(id, decl)| match decl {
            InterfaceDecl::Forward(decl_name) if decl_name == name => Some(*id),
            InterfaceDecl::Def(iface) if iface.name == *name => Some(*id),
            _ => None,
        });

        if let Some(existing) = existing {
            return existing;
        }

        let id = self.next_iface_id;
        self.metadata.ifaces.insert(id, InterfaceDecl::Forward(name.clone()));
        self.next_iface_id.0 += 1;
        id
    }

    pub fn define_iface(&mut self, iface_def: InterfaceDef) -> InterfaceID {
        let id = self.declare_iface(&iface_def.name);

        self.metadata.ifaces.insert(id, InterfaceDecl::Def(iface_def));

        id
    }

    pub fn get_iface_def(&self, id: InterfaceID) -> Option<&InterfaceDef> {
        self.find_in_self_or_refs(move |metadata| metadata.get_iface_def(id))
    }

    pub fn find_iface(&self, name: &NamePath) -> Option<InterfaceID> {
        self.find_in_self_or_refs(move |metadata| metadata
            .interfaces()
            .find_map(|(id, def)| {
                (def.name == *name).then_some(id)
            }))
    }

    pub fn declare_empty_impl(&mut self, iface_id: InterfaceID, implementor: Type) {
        let impl_ty_entry = self.metadata.iface_impls
            .entry(implementor)
            .or_insert_with(|| BTreeMap::new());

        impl_ty_entry.entry(iface_id)
            .or_insert_with(|| InterfaceImpl::new(0));
    }

    pub fn impl_method(
        &mut self,
        iface_id: InterfaceID,
        for_ty: Type,
        method_name: impl Into<String>,
        func_id: FunctionID,
    ) {
        let method_name = method_name.into();
        
        let iface_def = self
            .interfaces()
            .find_map(|(id, def)| (id == iface_id).then(|| {
                def
            }));
        
        match iface_def {
            Some(iface_def) => {
                let index = iface_def
                    .method_index(&method_name)
                    .unwrap_or_else(|| panic!("expected {} (interface {}) to contain method {}", iface_def.name, iface_id, method_name));

                self.add_impl(for_ty, iface_id, index, func_id);
            },

            None => panic!(
                "trying to impl method {} for missing interface {}",
                method_name, iface_id
            ),
        }
    }

    fn add_impl(&mut self,
        implementor: Type,
        iface_id: InterfaceID,
        method_id: MethodID,
        func_id: FunctionID,
    ) {
        let Some(def) = self.get_iface_def(iface_id) else {
            panic!("add_impl: missing definition for interface {iface_id}");
        };

        assert!(method_id.0 < def.methods.len());

        let methods_len = def.methods.len();
        
        let type_impls = self.metadata.iface_impls
            .entry(implementor.clone())
            .or_insert_with(|| BTreeMap::new());

        let impl_entry = type_impls
            .entry(iface_id)
            .or_insert_with(|| InterfaceImpl::new(methods_len));

        assert!(
            !impl_entry.methods.contains_key(&method_id),
            "adding duplicate impl ({}) of iface {}/method {} for {}, already defined as {}",
            func_id,
            iface_id,
            method_id.0,
            implementor,
            impl_entry.methods[&method_id],
        );

        impl_entry.methods.insert(method_id, func_id);
    }
    
    pub fn declare_iface_impl(&mut self, iface_id: InterfaceID, self_ty: Type) {
        self.declare_empty_impl(iface_id, self_ty);
    }
}
