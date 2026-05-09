use crate::c::type_map::TypeID;
use crate::c::Type;
use crate::c::TypeDecl;
use crate::c::TypeDefName;
use crate::c::{FieldName, Unit};
use crate::ir;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

#[derive(Clone, Eq)]
pub struct VariantCaseDef {
    pub ty: Option<Type>,
    pub comment: Option<String>,
}

impl PartialEq for VariantCaseDef {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

impl Hash for VariantCaseDef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
    }
}

#[derive(Clone, Eq)]
pub struct VariantDef {
    pub decl: TypeDecl,
    pub cases: Vec<VariantCaseDef>,

    pub comment: Option<String>,
}

impl PartialEq for VariantDef {
    fn eq(&self, other: &Self) -> bool {
        self.decl == other.decl && self.cases == other.cases
    }
}

impl Hash for VariantDef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.decl.hash(state);
        self.cases.hash(state);
    }
}

impl VariantDef {
    pub fn translate(
        index: TypeID,
        def: &ir::VariantDef,
        comment: Option<String>,
        unit: &mut Unit,
    ) -> Self {
        let cases = def
            .cases
            .iter()
            .map(|case| {
                let ty = case
                    .ty
                    .as_ref()
                    .map(|data_ty| unit.translate_type(data_ty));
                VariantCaseDef {
                    ty,
                    comment: Some(case.name.clone()),
                }
            })
            .collect();

        Self {
            decl: TypeDecl {
                name: TypeDefName::Variant(index),
            },
            cases,
            comment,
        }
    }
}

impl fmt::Display for VariantDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            writeln!(f, "/** {} */", comment)?;
        }

        writeln!(f, "{} {{", self.decl)?;

        writeln!(
            f,
            "  {};",
            Type::Int32.to_decl_string(&FieldName::VariantTag)
        )?;

        if self.cases.iter().any(|c| c.ty.is_some()) {
            writeln!(f, "  union {}_Data {{", self.decl.name)?;
            for (index, case) in self.cases.iter().enumerate() {
                let name = FieldName::VariantDataCase(index);

                if let Some(case_ty) = &case.ty {
                    if let Some(comment) = &case.comment {
                        writeln!(f, "  /** {} */", comment)?;
                    }

                    writeln!(f, "    {};", case_ty.to_decl_string(&name))?;
                }
            }

            writeln!(f, "  }} {};", FieldName::VariantData)?;
        }

        write!(f, "}}")
    }
}
