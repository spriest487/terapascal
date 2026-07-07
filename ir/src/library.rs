use crate::write_instruction_list;
use crate::ExternalFunctionRef;
use crate::Function;
use crate::FunctionDef;
use crate::FunctionID;
use crate::IRFormatter;
use crate::InstructionList;
use crate::Metadata;
use crate::MetadataSource;
use crate::TagInfo;
use crate::TypeDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fmt;
use std::io;
use std::iter;
use std::sync::Arc;
use terapascal_common::version::Version;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Library {
    pub name: String,
    pub version: Version,

    pub tags: Vec<TagInfo>,

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
            
            tags: Vec::new(),

            references: references.into_iter().collect(),

            metadata: metadata.into(),

            functions: BTreeMap::new(),

            init: InstructionList::new(),
        }
    }

    pub fn merge<'a>(&'a self, ref_libs: impl IntoIterator<Item=&'a Self>) -> Self {
        let mut ref_libs = ref_libs.into_iter();
        let Some(first_lib) = ref_libs.next() else {
            return self.clone();
        };

        let mut merged_lib = first_lib.clone();
        let mut merged_metadata = first_lib.metadata.as_ref().clone();

        let mut merged_libs_names = vec![first_lib.name.clone()];

        let mut all_libs = ref_libs.chain(iter::once(self));

        while let Some(ref_lib) = all_libs.next() {
            merged_lib.references.retain(|r| {
                *r != ref_lib.name && !merged_libs_names.contains(&ref_lib.name)
            });

            merged_libs_names.push(ref_lib.name.clone());

            // add any new references (that aren't this library)
            for transitive_ref in &ref_lib.references {
                if !merged_libs_names.contains(&transitive_ref)
                    && !merged_lib.references.contains(transitive_ref)
                {
                    merged_lib.references.push(transitive_ref.clone());
                }
            }

            merged_metadata.merge_from(&ref_lib.metadata);

            // we should be able to assume that if merging the metadata doesn't raise an error,
            // merging the libraries is valid
            merged_lib.functions.extend(ref_lib.functions.clone());
            merged_lib.tags.extend(ref_lib.tags.clone());
            merged_lib.init.extend(&ref_lib.init);

            merged_libs_names.push(ref_lib.name.clone());
        }

        merged_lib.metadata = Arc::new(merged_metadata);
        merged_lib.version = self.version;
        merged_lib.name = self.name.clone();

        merged_lib
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

    pub fn format(&self, f: &mut dyn fmt::Write, formatter: &impl IRFormatter) -> fmt::Result {
        writeln!(f, "Name: {}", self.name)?;
        writeln!(f, "Version: {}", self.version)?;

        if !self.references.is_empty() {
            writeln!(f, "References:")?;
            for ref_name in &self.references {
                writeln!(f, "\t{}", ref_name)?;
            }
            writeln!(f)?;
        }

        if !self.tags.is_empty() {
            writeln!(f, "Tags:")?;
            for tag in &self.tags {
                tag.format_pretty(formatter, f)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "Type Definitions:")?;
        writeln!(f)?;

        let mut defs: Vec<_> = self.metadata.type_defs().collect();
        defs.sort_by_key(|(id, _)| *id);

        for (id, def) in &defs {
            writeln!(f, "{id}: {}", def.to_pretty_string(formatter))?;

            match def {
                TypeDef::Struct(s) => {
                    write_tag_list(&s.tags, formatter, f)?;

                    if !s.fields. is_empty() {
                        writeln!(f, "Fields:")?;
                    }
                    for (field_id, field) in &s.fields {
                        write!(f, "  {:8>}: ", field_id)?;
                        formatter.format_type(&field.ty, f)?;

                        if let Some(field_name) = &field.name {
                            write!(f, " (`{}`)", field_name)?;
                        }

                        writeln!(f)?;
                    }
                },

                TypeDef::Variant(v) => {
                    write_tag_list(&v.tags, formatter, f)?;
                    writeln!(f, "Discriminator: {}", v.tag_type.to_pretty_string(formatter))?;

                    if !v.cases. is_empty() {
                        writeln!(f, "Cases:")?;
                    }

                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name)?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": ")?;
                            formatter.format_type(ty, f)?;
                        }
                        writeln!(f)?;
                    }
                },
            }

            let impl_self_ty = match def {
                TypeDef::Variant(def) => {
                    Some(id.to_variant_type(def.name.generic_args()))
                },
                TypeDef::Struct(struct_def) => {
                    Some(struct_def.identity.to_definition_type(*id))
                },
            };

            if let Some(self_ty) = impl_self_ty {
                let iface_impls = self.metadata.type_impls(&self_ty);

                if !iface_impls.is_empty() {
                    writeln!(f, "  Implements:")?;
                    for (iface_id, _) in iface_impls {
                        write!(f, "    ")?;

                        formatter.format_type(&iface_id.to_object_id().to_object_type(), f)?;

                        writeln!(f)?;
                    }
                }
            }

            writeln!(f)?;
        }

        writeln!(f, "Interfaces:")?;

        let mut ifaces: Vec<_> = self.metadata.interface_defs().collect();
        ifaces.sort_by_key(|(id, _)| *id);

        for (id, iface) in &ifaces {
            writeln!(f, "{}: {}", id, iface.name)?;

            write_tag_list(&iface.tags, formatter, f)?;

            for (i, method) in iface.methods.iter().enumerate() {
                let sig_params: Vec<_> = method
                    .params
                    .iter()
                    .map(|param| param.param_type.to_pretty_string(formatter))
                    .collect();
                let return_ty = method.result_type.to_pretty_string(formatter);

                let sig = format!("function ({}): {}", sig_params.join("; "), return_ty);

                let index = format!("  .{}", i);
                write!(f, "{:8>} ({}): {}", index, method.name, sig)?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;

        writeln!(f, "Constants:")?;
        for const_info in self.metadata.constants() {
            write!(f, "{}", const_info.name)?;

            write!(f," (")?;
            formatter.format_type(&const_info.value_type, f)?;
            writeln!(f, "):")?;

            write_tag_list(&const_info.tags, formatter, f)?;
            write!(f, "Value: ")?;
            formatter.format_val(&const_info.value, f)?;

            writeln!(f)?;
            writeln!(f)?;
        }
        writeln!(f)?;

        writeln!(f, "Variables:")?;
        for (var_id, var_info) in self.metadata.variables() {
            writeln!(f, "{}: {}", var_id.0, var_info.value_type.to_pretty_string(formatter))?;

            if let Some(name) = &var_info.name {
                writeln!(f, " ({})", name)?;
            }
        }
        writeln!(f)?;

        writeln!(f, "String literals:")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "{}: '{}'", id.0, lit.escape_default())?;
        }
        writeln!(f)?;

        writeln!(f, "Type Info:")?;

        for (ty, type_info) in self.metadata.type_info() {
            self.metadata.format_type(ty, f)?;

            let type_name = self.metadata
                .get_string(type_info.name)
                .map(String::as_str)
                .unwrap_or("");

            writeln!(f, ": '{}'", type_name)?;
        }

        writeln!(f)?;

        let mut funcs: Vec<_> = self.functions.iter().collect();
        funcs.sort_by_key(|(id, _)| **id);

        writeln!(f, "Functions:")?;
        for (id, func) in funcs {
            let func_desc = self.metadata.func_desc(*id);
            let sig_display = func.sig().to_pretty_string(formatter);

            if let Some(desc_name) = func_desc {
                writeln!(f, "{}: {} ({})", id.0, desc_name, sig_display)?;
            } else {
                writeln!(f, "{}: {}", id.0, sig_display)?;
            }

            if let Some(func_info) = self.metadata().get_function_info(*id) {
                for (i, param) in func_info.params.iter().enumerate() {
                    if !param.tags.is_empty() {
                        write!(f, "Param {i} ")?;
                        write_tag_list(&param.tags, formatter, f)?;
                    }
                }

                write_tag_list(&func_info.tags, formatter, f)?;

                let type_params = func_info.identity.type_params();
                if !type_params.is_empty() {
                    writeln!(f, "Type parameters:")?;
                    for param in type_params {
                        write!(f, "\t{}", param.name)?;

                        if let Some(constraint) = &param.constraint {
                            write!(f, " = ")?;
                            formatter.format_type(constraint, f)?;
                        }

                        writeln!(f)?;
                    }
                }
            }

            match func {
                Function::Local(FunctionDef { body, .. }) => {
                    write_instruction_list(f, formatter, &body.instructions)?;
                },

                Function::External(ExternalFunctionRef { symbol, src, .. }) => {
                    writeln!(f, "<external function '{}' in module '{}'>", symbol, src)?;
                },
            }
            writeln!(f)?;
        }

        writeln!(f, "Init:")?;
        write_instruction_list(f, formatter, &self.init.instructions)?;
        Ok(())
    }
}

impl fmt::Display for Library {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format(f, self.metadata.as_formatter())
    }
}

fn write_tag_list(tags: &[TagInfo], formatter: &impl IRFormatter, f: &mut dyn fmt::Write) -> fmt::Result {
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

pub fn encode_lib(lib: &Library) -> Result<Vec<u8>, io::Error> {
    let data = rmp_serde::encode::to_vec_named(&lib)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;

    Ok(data)
}

pub fn decode_lib(data: &[u8]) -> Result<Library, io::Error> {
    rmp_serde::decode::from_slice(data)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))
}
