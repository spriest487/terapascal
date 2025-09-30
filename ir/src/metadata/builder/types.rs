use crate::FunctionID;
use crate::Interface;
use crate::InterfaceDecl;
use crate::InterfaceID;
use crate::MetadataBuilder;
use crate::NamePath;
use crate::RuntimeType;
use crate::SetAliasDef;
use crate::SetAliasID;
use crate::Struct;
use crate::StructFieldDef;
use crate::StructIdentity;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::TypeDefID;
use crate::VariantDef;
use crate::DYNARRAY_LEN_FIELD;
use crate::DYNARRAY_PTR_FIELD;
use linked_hash_map::Entry;
use linked_hash_map::LinkedHashMap;

impl MetadataBuilder {
    pub fn insert_type_decl(&mut self, decl: TypeDecl) -> TypeDefID {
        let id = self.next_type_id;

        let replaced = self.metadata.type_decls.insert(id, decl);
        assert!(replaced.is_none());

        self.next_type_id.0 += 1;

        id
    }

    pub fn new_type(&mut self) -> TypeDefID {
        let id = self.next_type_id;
        self.metadata.type_decls.insert(id, TypeDecl::Reserved);

        self.next_type_id.0 += 1;
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

    pub fn define_struct(&mut self, id: TypeDefID, struct_def: Struct) {
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
    
    pub fn get_struct_def(&self, id: TypeDefID) -> Option<&Struct> {
        self.find_in_self_or_refs(move |metadata| metadata.get_struct_def(id))
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

    pub fn get_variant_def(&self, id: TypeDefID) -> Option<&VariantDef> {
        self.find_in_self_or_refs(move |metadata| metadata.get_variant_def(id))
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

    pub fn define_iface(&mut self, iface_def: Interface) -> InterfaceID {
        let id = self.declare_iface(&iface_def.name);

        self.metadata.ifaces.insert(id, InterfaceDecl::Def(iface_def));

        id
    }

    pub fn get_iface_def(&self, id: InterfaceID) -> Option<&Interface> {
        self.find_in_self_or_refs(move |metadata| metadata.get_iface_def(id))
    }

    pub fn impl_method(
        &mut self,
        iface_id: InterfaceID,
        for_ty: Type,
        method_name: impl Into<String>,
        func_id: FunctionID,
    ) {
        let method_name = method_name.into();

        match self.metadata.ifaces.get_mut(&iface_id) {
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

    pub fn define_dyn_array_struct(&mut self, element: Type) -> TypeDefID {
        assert!(
            !self.metadata.dyn_array_structs.contains_key(&element),
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

        let struct_id = self.next_type_id;
        self.metadata.type_decls.insert(
            struct_id,
            TypeDecl::Def(TypeDef::Struct(Struct {
                identity: StructIdentity::DynArray(element.clone()),
                fields,
            })),
        );

        self.next_type_id.0 += 1;
        
        self.metadata.dyn_array_structs.insert(element.clone(), struct_id);

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

    pub fn define_set_type(&mut self, name: Option<NamePath>, flags_struct: TypeDefID) -> SetAliasID {
        let set_id = self.next_set_id;

        self.metadata.set_aliases.insert(set_id, SetAliasDef {
            name,
            flags_struct,
        });
        
        self.next_set_id.0 += 1;

        set_id
    }
    
    pub fn type_defs(&self) -> impl Iterator<Item=(TypeDefID, &TypeDef)> {
        self.iter_in_self_or_refs(move |metadata| metadata.type_defs())
    }
}
