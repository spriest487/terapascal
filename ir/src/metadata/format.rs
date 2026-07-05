use crate::FieldID;
use crate::FunctionIdentity;
use crate::FunctionRef;
use crate::GlobalRef;
use crate::IRFormatter;
use crate::InterfaceRef;
use crate::MetadataSource;
use crate::MethodID;
use crate::ObjectID;
use crate::RawFormatter;
use crate::Ref;
use crate::TagLocation;
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

            Ref::Global(GlobalRef::Function(func_ref)) => {
                self.format_func_ref(func_ref, f)
            }

            Ref::Global(GlobalRef::Variable(id)) => {
                match self.get_variable(*id) {
                    Some(VariableInfo { name: Some(var_name), .. }) => {
                        write!(f, "{}", var_name)
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
            
            Ref::Global(GlobalRef::StaticTagArray(tag_loc)) => {
                write!(f, "tagarray(")?;
                match tag_loc {
                    TagLocation::TypeDef(type_def_id) => {
                        write!(f, "{}", type_def_id.to_pretty_string(self))?;
                    }
                    TagLocation::Interface(iface_id) => {
                        write!(f, "{}", iface_id.to_object_id([]).to_object_type().to_pretty_string(self))?;
                    }
                    TagLocation::Method { type_id, method_index } => {
                        self.format_type_def_method(*type_id, MethodID(*method_index), f)?;
                    }
                    TagLocation::InterfaceMethod { iface_id, method_index } => {
                        let iface_ref = InterfaceRef::new(*iface_id, []);
                        self.format_iface_method(&iface_ref, MethodID(*method_index), f)?;
                    }
                    TagLocation::Function(id) => {
                        write!(f, "{}", FunctionRef::new(*id).to_pretty_string(self))?;
                    },
                }
                write!(f, ")")?;
                Ok(())
            }

            Ref::Global(GlobalRef::StaticFuncInfo(id)) => {
                write!(f, "funcinfo({})", id)
            }

            _ => {
                RawFormatter.format_ref(r, f)
            },
        }
    }

    fn format_func_ref(&self, r: &FunctionRef, f: &mut dyn fmt::Write) -> fmt::Result {
        let func_name = self
            .get_function_info(r.def_id)
            .and_then(|f| f.identity.global_name());

        match func_name {
            Some(name) => {
                write!(f, "{}", name)?
            },

            None => {
                match self.find_impl(r.def_id) {
                    Some(impl_ref) => {
                        self.format_type(impl_ref.impl_type, f)?;
                        write!(f, ".{}", impl_ref.method_name)?;
                    },

                    None => {
                        write!(f, "{}", r.def_id)?
                    },
                }
            },
        }

        if !r.args.is_empty() {
            self.format_type_args(&r.args, f)?;
        }

        Ok(())
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        let def_id  = match of_ty {
            Type::Struct(type_ref) => Some(type_ref.def_id),
            Type::Object(object_id) | Type::WeakObject(object_id) => {
                match object_id {
                    ObjectID::Class(type_ref) => Some(type_ref.def_id),
                    _ => None,
                }
            }
            _ => None,
        };

        let struct_def = def_id.and_then(|id| self.get_struct_def(id));
        let field_def = struct_def.and_then(|def| def.get_field(field));
        let field_name = field_def.and_then(|field_def| field_def.name.as_ref());

        match field_name {
            Some(name) => write!(f, "{}", name)?,
            None => RawFormatter.format_field(of_ty, field, f)?,
        };

        Ok(())
    }

    fn format_iface_method(
        &self,
        iface_id: &InterfaceRef,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        let iface = match self.get_interface_def(iface_id.def_id) {
            Some(iface) => iface,
            None => return RawFormatter.format_iface_method(iface_id, method, f),
        };

        let method = match iface.get_method(method) {
            Some(method) => method,
            None => {
                return RawFormatter.format_iface_method(iface_id, method, f);
            },
        };

        write!(f, "{}", method.name)
    }

    fn format_type_def_method(
        &self,
        def_id: TypeDefID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        let Some(name) = self
            .functions()
            .find_map(|(_id, m)| {
                let (declaring_type_ref, name) = match &m.identity {
                    FunctionIdentity::Method { declaring_type, name, .. } => {
                        (declaring_type.definition_ref()?, name)
                    }

                    FunctionIdentity::Destructor { declaring_type, name, .. } => {
                        (declaring_type.definition_ref()?, name)
                    }

                    _ => {
                        return None;
                    }
                };

                (declaring_type_ref.def_id == def_id).then_some(name)
            })
        else {
            return RawFormatter.format_type_def_method(def_id, method, f);
        };

        write!(f, "{}", name)
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
