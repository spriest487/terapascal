use crate::FieldID;
use crate::GlobalRef;
use crate::IRFormatter;
use crate::InterfaceID;
use crate::MetadataSource;
use crate::MethodID;
use crate::ObjectID;
use crate::RawFormatter;
use crate::Ref;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDefID;
use crate::Value;
use crate::VariableInfo;
use std::fmt;


impl<T: MetadataSource> IRFormatter for T {
    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", self.pretty_type_name(ty))
    }

    fn format_type_def(&self, id: TypeDefID, f: &mut dyn fmt::Write) -> fmt::Result {
        match self.get_type_decl(id) {
            Some(TypeDecl::Def(def)) => {
                write!(f, "{}", def.to_pretty_string(self))
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
                write!(f, "sizeof({})", self.pretty_type_name(ty))
            },
            _ => RawFormatter.format_val(val, f),
        }
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        match r {
            Ref::Deref(inner) => {
                write!(f, "(")?;
                self.format_val(inner, f)?;
                write!(f, ")^")?;
                Ok(())
            }

            Ref::Field(field_ref) => {
                // write!(f, "(as ")?;
                // self.format_type(&field_ref.instance_type, f)?;
                // write!(f, ") &(")?;

                write!(f, "&")?;
                self.format_ref(&field_ref.instance, f)?;
                write!(f, ".")?;

                let struct_def = match &field_ref.instance_type {
                    Type::Flags(id) => {
                        self.get_struct_def(*id)
                    }

                    Type::Struct(id)
                    | Type::Object(ObjectID::Class(id))
                    | Type::WeakObject(ObjectID::Class(id)) => {
                        self.get_struct_def(id.def_id)
                    },

                    _ => {
                        None
                    },
                };

                let field_name = struct_def
                    .and_then(|def| def.fields.get(&field_ref.field))
                    .and_then(|field_def| field_def.name.as_ref());

                if let Some(name) = field_name {
                    write!(f, "{}", name)?;
                } else {
                    write!(f, "{}", field_ref.field.0)?;
                }
                // write!(f, ")")?;

                Ok(())
            }

            Ref::Element(el_ref) => {
                // write!(f, "(as ")?;
                // self.format_type(&el_ref.instance_type, f)?;
                // write!(f, ") &(")?;

                write!(f, "&")?;
                self.format_ref(&el_ref.instance, f)?;
                write!(f, "[")?;
                self.format_val(&el_ref.index, f)?;
                write!(f, "]")?;
                // write!(f, "])")?;

                Ok(())
            }

            Ref::VariantTag(tag_ref) => {
                // write!(f, "(as ")?;
                // self.format_type(&tag_ref.instance_type, f)?;
                // write!(f, ") &(tag of ")?;

                write!(f, "&tag of ")?;
                self.format_ref(&tag_ref.instance, f)?;
                // write!(f, ")")?;
                Ok(())
            }

            Ref::VariantData(data_ref) => {
                write!(f, "(as ")?;
                self.format_type(&data_ref.instance_type, f)?;
                write!(f, ") &(")?;

                self.format_ref(&data_ref.instance, f)?;
                write!(f, ".")?;

                let case_name = data_ref.instance_type
                    .as_variant()
                    .and_then(|id| self.get_variant_def(id.def_id))
                    .and_then(|def| def.cases.get(data_ref.case_index))
                    .map(|case_def| &case_def.name);

                match case_name {
                    Some(name) => write!(f, "{}", name)?,
                    None => write!(f, "{}", data_ref.case_index)?,
                }

                write!(f, ")")?;
                Ok(())
            }

            Ref::Global(GlobalRef::StringLiteral(string_id)) => match self.get_string(*string_id) {
                Some(string_lit) => write!(f, "'{}'", string_lit.escape_default()),
                None => write!(f, "{}", r),
            },

            Ref::Global(GlobalRef::Function(key)) => {
                let func_name = self
                    .get_function_info(key.id)
                    .and_then(|f| f.identity.as_path());

                match func_name {
                    Some(name) => write!(f, "{}", name.to_pretty_string(self)),

                    None => {
                        match self.find_iface_impl(key.id) {
                            Some(impl_ref) => {
                                let iface_pretty_name = impl_ref.interface.to_pretty_string(self);
                                write!(f, "{}.{} impl for ", iface_pretty_name, impl_ref.method_name)?;

                                self.format_type(impl_ref.impl_type, f)
                            },

                            None => write!(f, "{}", r),
                        }
                    },
                }
            },

            Ref::Global(GlobalRef::Variable(id)) => {
                match self.get_variable(*id) {
                    Some(VariableInfo { name: Some(var_name), .. }) => {
                        write!(f, "{}", var_name.to_pretty_string(self.as_formatter()))
                    },

                    _ => {
                        write!(f, "{id}")
                    },
                }
            }

            Ref::Global(GlobalRef::StaticTypeInfo(ty)) => {
                write!(f, "typeinfo(")?;
                self.format_type(ty, f)?;
                write!(f, ")")
            }

            Ref::Global(GlobalRef::StaticFuncInfo(id)) => {
                write!(f, "funcinfo({})", id)
            }

            _ => {
                RawFormatter.format_ref(r, f)
            },
        }
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        let field_name = of_ty
            .as_struct()
            .or_else(|| match of_ty.as_object()? {
                ObjectID::Class(id) => Some(&id),
                _ => None,
            })
            .and_then(|type_id| self.get_struct_def(type_id.def_id))
            .and_then(|struct_def| struct_def.fields.get(&field))
            .and_then(|field| field.name.as_ref());

        match field_name {
            Some(name) => write!(f, "{}", name),
            _ => RawFormatter.format_field(of_ty, field, f),
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
            None => return RawFormatter.format_method(iface_id, method, f),
        };

        let method = match iface.get_method(method) {
            Some(method) => method,
            None => {
                return RawFormatter.format_method(iface_id, method, f);
            },
        };

        write!(f, "{}", method.name)
    }

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result {
        let case_name = match of_ty {
            Type::Variant(id) => self
                .get_variant_def(id.def_id)
                .and_then(|variant| variant.cases.get(tag))
                .map(|case| &case.name),
            _ => None,
        };

        match case_name {
            Some(name) => write!(f, "{}", name),
            _ => RawFormatter.format_variant_case(of_ty, tag, f),
        }
    }
}
