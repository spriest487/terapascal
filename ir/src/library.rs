use crate::write_instruction_list;
use crate::ExternalFunctionRef;
use crate::Function;
use crate::FunctionDef;
use crate::FunctionID;
use crate::IRFormatter;
use crate::InstructionList;
use crate::Metadata;
use crate::MetadataSource;
use crate::StructIdentity;
use crate::TagInfo;
use crate::Type;
use crate::TypeDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fmt;
use std::io;
use std::sync::Arc;
use terapascal_common::version::Version;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Library {
    pub name: String,
    pub version: Version,

    pub references: Vec<String>,

    pub metadata: Arc<Metadata>,

    pub functions: BTreeMap<FunctionID, Function>,

    pub init: InstructionList,
}

impl Library {
    pub fn new(
        name: impl Into<String>,
        version: Version,
        references: impl IntoIterator<Item=String>,
        metadata: impl Into<Arc<Metadata>>,
    ) -> Self {
        Self {
            name: name.into(),
            version,

            references: references.into_iter().collect(),

            metadata: metadata.into(),

            functions: BTreeMap::new(),

            init: InstructionList::new(),
        }
    }

    pub fn init(&self) -> &InstructionList {
        &self.init
    }

    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    pub fn functions(&self) -> &BTreeMap<FunctionID, Function> {
        &self.functions
    }
}

impl fmt::Display for Library {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Type Definitions")?;
        writeln!(f)?;

        let mut defs: Vec<_> = self.metadata.type_defs().collect();
        defs.sort_by_key(|(id, _)| *id);

        for (id, def) in &defs {
            writeln!(f, "{id}: {}", def.to_pretty_string(self.metadata()))?;

            match def {
                TypeDef::Struct(s) => {
                    write_tag_list(&s.tags, self.metadata(), f)?;

                    if !s.fields. is_empty() {
                        writeln!(f, "Fields:")?;
                    }
                    for (field_id, field) in &s.fields {
                        write!(f, "  {:8>}: ", field_id)?;
                        self.metadata.format_type(&field.ty, f)?;

                        if let Some(field_name) = &field.name {
                            write!(f, " (`{}`)", field_name)?;
                        }

                        writeln!(f)?;
                    }
                },

                TypeDef::Variant(v) => {
                    write_tag_list(&v.tags, self.metadata(), f)?;
                    writeln!(f, "Discriminator: {}", v.tag_type.to_pretty_string(self.metadata()))?;

                    if !v.cases. is_empty() {
                        writeln!(f, "Cases:")?;
                    }

                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name,)?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": ")?;
                            self.metadata.format_type(ty, f)?;
                        }
                        writeln!(f)?;
                    }
                },

                TypeDef::Function(sig) => {
                    writeln!(f, "{}", sig.to_pretty_string(self.metadata()))?;
                },
            }

            let impl_self_ty = match def {
                TypeDef::Variant(..) => Some(Type::Variant(*id)),
                TypeDef::Struct(struct_def) => match &struct_def.identity {
                    StructIdentity::Record(..) => Some(id.to_struct_type()),
                    StructIdentity::Class(..) => Some(id.to_class_ptr_type()),
                    _ => None,
                }
                TypeDef::Function(..) => None,
            };

            if let Some(self_ty) = impl_self_ty {
                let iface_impls = self.metadata.impls(&self_ty);

                if !iface_impls.is_empty() {
                    writeln!(f, "  Implements:")?;
                    for iface_id in iface_impls {
                        writeln!(f, "    {}", self.metadata.iface_name(iface_id))?;
                    }
                }
            }

            writeln!(f)?;
        }

        writeln!(f, "* Interfaces: ")?;

        let mut ifaces: Vec<_> = self.metadata.interfaces().collect();
        ifaces.sort_by_key(|(id, _)| *id);

        for (id, iface) in &ifaces {
            writeln!(f, "{}: {}", id, iface.name)?;

            write_tag_list(&iface.tags, self.metadata(), f)?;

            for (i, method) in iface.methods.iter().enumerate() {
                let sig_params: Vec<_> = method
                    .params
                    .iter()
                    .map(|param| self.metadata.pretty_type_name(param))
                    .collect();
                let return_ty = self.metadata.pretty_type_name(&method.return_ty);

                let sig = format!("function ({}): {}", sig_params.join("; "), return_ty);

                let index = format!("  .{}", i);
                write!(f, "{:8>} ({}): {}", index, method.name, sig)?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;

        writeln!(f, "* Constants")?;
        for const_info in self.metadata.constants() {
            writeln!(f, "{} = ", const_info.name)?;
            self.metadata.format_val(&const_info.value, f)?;
        }
        writeln!(f)?;
        
        writeln!(f, "* Variables")?;
        for (var_id, var_info) in self.metadata.variables() {
            writeln!(f, "{}: {}", var_id.0, self.metadata.pretty_type_name(&var_info.r#type))?;

            if let Some(name) = &var_info.name {
                writeln!(f, " ({})", name)?;
            }
        }
        writeln!(f)?;
        
        writeln!(f, "* String literals")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "{}: '{}'", id.0, lit.escape_default())?;
        }
        writeln!(f)?;

        let mut funcs: Vec<_> = self.functions.iter().collect();
        funcs.sort_by_key(|(id, _)| **id);

        writeln!(f, "* Functions")?;
        for (id, func) in funcs {
            write!(f, "{}: {}", id.0, func.sig().to_pretty_string(self.metadata.as_ref()))?;

            match self.metadata.func_desc(*id) {
                Some(desc_name) => {
                    writeln!(f, " ({})", desc_name)?;
                },

                None => {
                    if let Some(debug_name) = func.debug_name() {
                        writeln!(f, " ({})", debug_name)?;
                    } else {
                        writeln!(f)?;
                    }
                },
            }

            if let Some(func_info) = self.metadata().get_function_info(*id) {
                write_tag_list(&func_info.tags, self.metadata(), f)?;
            }

            match func {
                Function::Local(FunctionDef { body, .. }) => {
                    write_instruction_list(f, &self.metadata, &body.instructions)?;
                },

                Function::External(ExternalFunctionRef { symbol, src, .. }) => {
                    writeln!(f, "<external function '{}' in module '{}'>", symbol, src)?;
                },
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        write_instruction_list(f, &self.metadata, &self.init.instructions)?;
        Ok(())
    }
}

fn write_tag_list(tags: &[TagInfo], formatter: &impl IRFormatter, f: &mut impl fmt::Write) -> fmt::Result {
    if tags.is_empty() {
        return Ok(());
    }

    writeln!(f, "Tags:")?;

    for tag in tags {
        write!(f, "  ")?;
        tag.format_pretty(formatter, f)?;
        writeln!(f)?;
    }
    Ok(())
}

pub fn encode_lib(lib: &crate::Library) -> Result<Vec<u8>, io::Error> {
    let data = rmp_serde::encode::to_vec_named(&lib)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;

    Ok(data)
}

pub fn decode_lib(data: &[u8]) -> Result<crate::Library, io::Error> {
    rmp_serde::decode::from_slice(data)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))
}
