use crate::FunctionID;
use crate::FunctionInfo;
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
use std::rc::Rc;

pub trait MetadataSource {
    fn as_formatter(&self) -> &impl IRFormatter;

    fn get_string(&self, id: StringID) -> Option<&String>;
    
    fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&StructDef>;
    fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef>;
    fn type_defs(&self) -> impl Iterator<Item=(TypeDefID, &TypeDef)>;
    fn get_type_decl(&self, id: TypeDefID) -> Option<&TypeDecl>;
    fn find_type_decl(&self, name: &NamePath) -> Option<TypeDefID>;
    fn get_type_info(&self, of_type: &Type) -> Option<Rc<TypeInfo>>;

    fn functions(&self) -> impl Iterator<Item=(FunctionID, &FunctionInfo)>;
    fn get_function_info(&self, id: FunctionID) -> Option<&FunctionInfo>;
    
    fn interfaces(&self) -> impl Iterator<Item=(InterfaceID, &InterfaceDef)>;
    fn methods(&self) -> impl Iterator<Item=&MethodInfo>;
    
    fn find_variable(&self, name: &NamePath) -> Option<(VariableID, &VariableInfo)>;
    fn get_variable(&self, id: VariableID) -> Option<&VariableInfo>;

    fn all_tags(&self) -> impl Iterator<Item=(TagLocation, &[TagInfo])> {
        let type_tags = self.type_defs()
            .map(|(id, def)| {
                let tags = match def {
                    TypeDef::Struct(struct_def) => struct_def.tags.as_slice(),
                    TypeDef::Variant(struct_def) => struct_def.tags.as_slice(),
                    TypeDef::Function(_alias_sig) => &[],
                };

                (TagLocation::TypeDef(id), tags)
            });

        let iface_tags = self.interfaces()
            .map(|(id, iface_def)| {
                (TagLocation::Interface(id), iface_def.tags.as_slice())
            });

        let func_tags = self.functions()
            .map(|(id, func_info)| {
                (TagLocation::Function(id), func_info.tags.as_slice())
            });

        let method_tags = self.methods()
            .map(|method_info| {
                let loc = match &method_info.instance_ty {
                    Type::Object(ObjectID::Interface(iface_id)) => {
                        TagLocation::InterfaceMethod {
                            iface_id: *iface_id,
                            method_index: method_info.index,
                        }
                    }
                    
                    Type::Object(ObjectID::Class(type_id)) 
                    | Type::Variant(type_id)
                    | Type::Struct(type_id)
                    | Type::Flags(type_id)=> {
                        TagLocation::Method {
                            type_id: *type_id,
                            method_index: method_info.index,
                        }
                    }
                    
                    _ => {
                        let instance_ty_name = method_info.instance_ty.to_pretty_string(self.as_formatter());
                        panic!("unexpected base type for method: {}", instance_ty_name)
                    }
                };
                
                (loc, method_info.tags.as_slice())
            });
        
        type_tags
            .chain(iface_tags)
            .chain(func_tags)
            .chain(method_tags)
    }
}
