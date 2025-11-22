use crate::FunctionID;
use crate::InterfaceDecl;
use crate::InterfaceDef;
use crate::InterfaceID;
use crate::MetadataBuilder;
use crate::NamePath;
use crate::StructDef;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDef;
use crate::TypeDefID;
use crate::TypeInfo;
use crate::VariantDef;
use linked_hash_map::Entry;
use std::rc::Rc;

impl MetadataBuilder {
    pub fn type_info(&self) -> impl Iterator<Item=(&Type, &Rc<TypeInfo>)> {
        self.iter_in_self_or_refs(move |metadata| metadata.type_info())
    }
    
    pub fn is_defined(&self, ty: &Type) -> bool {
        self.find_in_self_or_refs(move |metadata| Some(metadata.is_defined(ty)))
            .unwrap_or(false)
    }
    
    pub fn find_type_decl(&self, name: &NamePath) -> Option<TypeDefID> {
        self.find_in_self_or_refs(move |metadata| metadata.find_type_decl(name))
    }
    
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
    
    pub fn get_struct_def(&self, id: TypeDefID) -> Option<&StructDef> {
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
    
    pub fn find_variant_def(&self, name_path: &NamePath) -> Option<(TypeDefID, &VariantDef)> {
        self.find_in_self_or_refs(move |metadata| metadata.find_variant_def(name_path))
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

    pub fn ifaces(&self) -> impl Iterator<Item=(InterfaceID, &InterfaceDef)> {
        self.iter_in_self_or_refs(move |metadata| metadata.ifaces())
    }

    pub fn define_iface(&mut self, iface_def: InterfaceDef) -> InterfaceID {
        let id = self.declare_iface(&iface_def.name);

        self.metadata.ifaces.insert(id, InterfaceDecl::Def(iface_def));

        id
    }

    pub fn get_iface_def(&self, id: InterfaceID) -> Option<&InterfaceDef> {
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
    
    pub fn declare_iface_impl(&mut self, iface_id: InterfaceID, self_ty: Type) {
        let Some(InterfaceDecl::Def(iface_def)) = self.metadata.ifaces.get_mut(&iface_id) else {
            panic!("declare_iface_impl: interface {iface_id} has not been defined");
        };

        iface_def.declare_empty_impl(self_ty);
    }
    
    pub fn type_defs(&self) -> impl Iterator<Item=(TypeDefID, &TypeDef)> {
        self.iter_in_self_or_refs(move |metadata| metadata.type_defs())
    }
}
