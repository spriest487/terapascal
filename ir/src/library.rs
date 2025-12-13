use crate::write_instruction_list;
use crate::ExternalFunctionRef;
use crate::Function;
use crate::FunctionDef;
use crate::FunctionID;
use crate::IRFormatter;
use crate::Instruction;
use crate::Metadata;
use crate::MetadataSource;
use crate::StaticClosure;
use crate::StructIdentity;
use crate::Type;
use crate::TypeDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fmt;
use std::io;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Library {
    pub metadata: Metadata,

    pub functions: BTreeMap<FunctionID, Function>,

    pub static_closures: Vec<StaticClosure>,

    pub init: Vec<Instruction>,
}

impl Library {
    pub fn new(metadata: Metadata) -> Self {
        Self {
            init: Vec::new(),

            functions: BTreeMap::new(),

            static_closures: Vec::new(),

            metadata,
        }
    }

    pub fn static_closures(&self) -> &[StaticClosure] {
        &self.static_closures
    }

    pub fn init(&self) -> &[Instruction] {
        self.init.as_slice()
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
        let mut defs: Vec<_> = self.metadata.type_defs().collect();
        defs.sort_by_key(|(id, _)| *id);

        for (id, def) in &defs {
            match def {
                TypeDef::Struct(s) => {
                    write!(f, "{}: ", id.0)?;

                    match &s.identity {
                        StructIdentity::Class(name) | StructIdentity::Record(name) => {
                            self.metadata.format_name(name, f)?;
                        },

                        StructIdentity::Closure(identity) => {
                            let func_ty_name = self
                                .metadata
                                .pretty_ty_name(&Type::Function(identity.virt_func_ty));
                            write!(
                                f,
                                "closure of {} @ ({})",
                                func_ty_name, identity.id
                            )?;
                        },

                        StructIdentity::Array(element, dim) => {
                            write!(
                                f,
                                "array[{dim}] of {}",
                                self.metadata.pretty_ty_name(element)
                            )?;
                        },

                        StructIdentity::SetFlags { bits } => {
                            write!(f, "set<{}>", bits)?;
                        }
                    }

                    writeln!(f)?;

                    for (id, field) in &s.fields {
                        write!(f, "  {:8>}: ", id)?;
                        self.metadata.format_type(&field.ty, f)?;

                        if let Some(field_name) = &field.name {
                            write!(f, " (`{}`)", field_name)?;
                        }

                        writeln!(f)?;
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
                },

                TypeDef::Variant(v) => {
                    writeln!(f, "{}: {}", id, v.name)?;
                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name,)?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": {}", ty)?;
                        }
                        writeln!(f)?;
                    }
                },

                TypeDef::Function(def) => {
                    write!(f, "{}: {}", id.0, self.metadata.pretty_func_sig(def))?;
                },
            }

            writeln!(f)?;
        }

        writeln!(f, "* Interfaces: ")?;
        let mut ifaces: Vec<_> = self.metadata.interfaces().collect();
        ifaces.sort_by_key(|(id, _)| *id);

        for (id, iface) in &ifaces {
            writeln!(f, "{}: {}", id, iface.name)?;

            for (i, method) in iface.methods.iter().enumerate() {
                let sig_params: Vec<_> = method
                    .params
                    .iter()
                    .map(|param| self.metadata.pretty_ty_name(param))
                    .collect();
                let return_ty = self.metadata.pretty_ty_name(&method.return_ty);

                let sig = format!("function ({}): {}", sig_params.join("; "), return_ty);

                let index = format!("  .{}", i);
                write!(f, "{:8>} ({}): {}", index, method.name, sig)?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;

        writeln!(f, "* Global Variables")?;
        for (var_id, var_info) in self.metadata.variables() {
            writeln!(f, "{}: {} ({})", var_id.0, self.metadata.pretty_ty_name(&var_info.r#type), var_info.name)?;
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
            write!(f, "{}: {}", id.0, func.sig().to_pretty_string(&self.metadata))?;
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

            match func {
                Function::Local(FunctionDef { body, .. }) => {
                    write_instruction_list(f, &self.metadata, body)?;
                },

                Function::External(ExternalFunctionRef { symbol, src, .. }) => {
                    writeln!(f, "<external function '{}' in module '{}'>", symbol, src)?;
                },
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        write_instruction_list(f, &self.metadata, &self.init)?;
        Ok(())
    }
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
