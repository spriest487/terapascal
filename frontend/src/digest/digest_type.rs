use std::sync::Arc;
use terapascal_ir::MetadataSource;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::StructKind;
use crate::digest::DigestBuilder;
use crate::digest::DigestError;
use crate::digest::DigestResult;
use crate::typ::TypeParam;
use crate::typ::TypeName;
use crate::typ::Type;
use crate::typ::Symbol;
use crate::typ::Primitive;
use crate::typ::TypeParamList;
use crate::ir;

impl DigestBuilder<'_> {
    pub fn digest_type(&mut self, ir_type: &ir::Type) -> DigestResult<Type> {
        let result = match ir_type {
            ir::Type::Nothing => {
                Type::Nothing
            },
            ir::Type::Generic(name) => {
                Type::generic_param(Ident::new(name.as_str(), self.span()))
            },
            ir::Type::Pointer(deref_ty) | ir::Type::TempRef(deref_ty) => {
                self.digest_type(deref_ty)?.ptr()
            }

            ir::Type::Struct(type_ref) | ir::Type::Variant(type_ref) => {
                self.digest_type_ref(type_ref)?
            }

            ir::Type::Flags(type_id) => {
                self.digest_type_ref(&ir::TypeRef::new(*type_id, []))?
            }

            ir::Type::Array { element, dim } => {
                let element_type = self.digest_type(element)?;
                Type::array(element_type, *dim)
            }

            ir::Type::Object(object_id) => {
                self.digest_object_type(object_id)?
            }
            ir::Type::WeakObject(object_id) => {
                Type::Weak(Arc::new(self.digest_object_type(object_id)?))
            }

            ir::Type::Function(sig) => {
                Type::Function(Arc::new(self.digest_sig(sig)?))
            },

            ir::Type::Bool => Type::Primitive(Primitive::Boolean),
            ir::Type::U8 => Type::Primitive(Primitive::UInt8),
            ir::Type::I8 => Type::Primitive(Primitive::Int8),
            ir::Type::I16 => Type::Primitive(Primitive::Int16),
            ir::Type::U16 => Type::Primitive(Primitive::UInt16),
            ir::Type::I32 => Type::Primitive(Primitive::Int32),
            ir::Type::U32 => Type::Primitive(Primitive::UInt32),
            ir::Type::I64 => Type::Primitive(Primitive::Int64),
            ir::Type::U64 => Type::Primitive(Primitive::UInt64),
            ir::Type::USize => Type::Primitive(Primitive::NativeUInt),
            ir::Type::ISize => Type::Primitive(Primitive::NativeInt),
            ir::Type::F32 => Type::Primitive(Primitive::Real32),
            ir::Type::F64 => Type::Primitive(Primitive::Real64),
        };

        Ok(result)
    }

    #[expect(unused)]
    fn digest_struct_ref(&mut self, type_ref: &ir::TypeRef) -> DigestResult<Symbol> {
        todo!()
    }

    fn digest_object_type(&mut self, object_id: &ir::ObjectID) -> DigestResult<Type> {
        match object_id {
            ir::ObjectID::Any => Ok(Type::Any),

            ir::ObjectID::Class(class_ref) => {
                self.digest_type_ref(class_ref)
            }

            ir::ObjectID::Interface(iface_id) => {
                self.digest_interface_type(*iface_id)
            }

            ir::ObjectID::AnyClosure(sig) => {
                let sig = self.digest_sig(sig)?;
                Ok(Type::Function(Arc::new(sig)))
            },

            ir::ObjectID::Array(element_type) => {
                let element_type = self.digest_type(element_type)?;
                Ok(element_type.dyn_array())
            },

            ir::ObjectID::Box(value_type) => {
                let value_type = self.digest_type(value_type)?;
                Ok(value_type.boxed())
            }
        }
    }

    fn digest_type_ref(&mut self, type_ref: &ir::TypeRef) -> DigestResult<Type> {
        let mut type_args = Vec::with_capacity(type_ref.args.len());
        for type_arg in &type_ref.args {
            type_args.push(TypeName::Unspecified(self.digest_type(type_arg)?));
        }

        let type_decl = self.library.metadata
            .get_type_decl(type_ref.def_id)
            .ok_or_else(|| DigestError::InvalidData)?;

        match type_decl {
            ir::TypeDecl::Reserved | ir::TypeDecl::Forward(..) => {
                Err(DigestError::InvalidData)
            }

            ir::TypeDecl::Def(ir::TypeDef::Struct(struct_def)) => {
                match &struct_def.identity {
                    ir::StructIdentity::Record(name)
                    | ir::StructIdentity::Class(name) => {
                        let path = self.digest_def_path(name)?;
                        let kind = if struct_def.identity.is_ref_type() {
                            StructKind::Class
                        } else {
                            StructKind::Record
                        };

                        Ok(Type::from_struct_type(path, kind))
                    }

                    ir::StructIdentity::ClosureObject(..) => {
                        // shouldn't appear in library interface
                        Err(DigestError::UnsupportedFeature("closure types".to_string()))
                    }

                    ir::StructIdentity::SetFlags { bits: _ } => {
                        Err(DigestError::UnsupportedFeature("set types".to_string()))
                    }
                }
            }

            ir::TypeDecl::Def(ir::TypeDef::Variant(variant_def)) => {
                let path = self.digest_def_path(&variant_def.name)?;
                Ok(Type::variant(path))
            }
        }
    }

    fn digest_interface_type(&mut self, id: ir::InterfaceID) -> DigestResult<Type> {
        let def = self.library.metadata
            .get_iface_def(id)
            .ok_or_else(|| DigestError::InvalidData)?;

        let name = self.digest_def_path(&def.name)?;

        Ok(Type::interface(name))
    }

    fn digest_def_path(&self, name_path: &ir::NamePath) -> DigestResult<Symbol> {
        let ident_path = IdentPath::from_parts(name_path.path
            .iter()
            .map(|part| Ident::new(part, self.span())));

        if name_path.type_args.is_empty() {
            return Ok(Symbol::from(ident_path));
        }

        let mut type_params = Vec::new();
        for path_arg in &name_path.type_args {
            let ir::Type::Generic(param_name) = path_arg else {
                // all params
                return Err(DigestError::InvalidData);
            };

            type_params.push(TypeParam::new(Ident::new(param_name, self.span())));
        }

        let param_list = TypeParamList::new(type_params, self.span());
        Ok(Symbol::from(ident_path).with_ty_params(Some(param_list)))
    }
}
