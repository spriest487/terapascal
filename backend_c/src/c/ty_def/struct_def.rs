use crate::c::type_map::TypeID;
use crate::c::Type;
use crate::c::TypeDecl;
use crate::c::TypeDefName;
use crate::c::Unit;
use crate::ir;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;

#[derive(Clone, Eq)]
pub struct StructMember {
    pub name: FieldName,
    pub ty: Type,

    pub comment: Option<String>,
}

impl PartialEq for StructMember {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl Hash for StructMember {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.ty.hash(state);
    }
}

#[derive(Clone)]
pub struct StructDef {
    pub decl: TypeDecl,
    pub members: Vec<StructMember>,

    pub packed: bool,

    pub ir_def: Option<Rc<ir::StructDef>>,
    pub comment: Option<String>,
}

impl StructDef {
    pub fn new(name: TypeDefName, packed: bool) -> Self {
        StructDef {
            decl: TypeDecl { name },
            packed,
            members: Vec::new(),
            ir_def: None,
            comment: None,
        }
    }
    
    pub fn with_member(mut self, member: StructMember) -> Self {
        self.members.push(member);
        self
    }

    pub fn with_comment(mut self, comment: impl Into<String>) -> Self {
        self.comment = Some(comment.into());
        self
    }
}

impl Hash for StructDef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.decl.hash(state);
        self.members.hash(state);
    }
}

impl StructDef {
    pub fn translate(
        index: TypeID,
        ir_struct: Rc<ir::StructDef>,
        comment: Option<String>,
        unit: &mut Unit,
    ) -> Self {
        let mut members = Vec::new();

        if ir_struct.identity.is_ref_type() {
            members.push(StructMember {
                name: FieldName::Rc,
                ty: Type::Rc,
                comment: None,
            });
        }

        // fields should be written in the numerical order of their field IDs in the IR
        let mut sorted_fields: Vec<_> = ir_struct.fields.iter().collect();
        sorted_fields.sort_by_key(|(id, _)| id.0);

        for (id, field) in sorted_fields {
            let ty = unit.translate_type(&field.ty);

            members.push(StructMember {
                name: FieldName::ID(*id),
                ty,
                comment: None,
            });
        }

        let packed = ir_struct.layout == ir::StructLayout::Packed;

        Self {
            decl: TypeDecl {
                name: TypeDefName::Struct(index),
            },
            ir_def: Some(ir_struct),
            packed,
            members,
            comment,
        }
    }
}

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            writeln!(f, "/** {} */", comment)?;
        }

        if self.packed {
            write!(f, "PACKED_DECL(")?;
        }

        writeln!(f, "{} {{", self.decl)?;
        for member in self.members.iter() {
            if let Some(comment) = &member.comment {
                writeln!(f, "/** {} */", comment)?;
            }

            let name = format!("{}", member.name);
            writeln!(f, "{};", member.ty.to_decl_string(&name))?;
        }
        write!(f, "}}")?;

        if self.packed {
            write!(f, ")")?;
        }

        Ok(())
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FieldName {
    // ID from metadata
    ID(ir::FieldID),

    // rc state for rc types
    Rc,

    // builtin name: ref count field of RC internal struct
    RcStrongCount,
    RcWeakCount,

    // builtin name: class info pointer field of RC internal struct
    ObjectClass,
    
    // members of the DynArrayClass struct
    DynArrayClassAlloc,
    DynArrayClassElement,
    DynArrayClassLength,

    DynArrayElements,
    DynArrayLength,

    // builtin name: static array inner array
    StaticArrayElements,

    BoxClassValue,
    BoxValue,

    VariantTag,
    VariantData,
    VariantDataCase(usize),
    
    // the function pointer field of the AnonymousClosure struct
    ClosureFunctionPointer,
}

impl fmt::Display for FieldName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FieldName::ID(id) => write!(f, "field_{}", id.0),
            FieldName::Rc => write!(f, "rc"),
            FieldName::RcStrongCount => write!(f, "strong_count"),
            FieldName::RcWeakCount => write!(f, "weak_count"),
            FieldName::ObjectClass => write!(f, "class"),
            FieldName::StaticArrayElements => write!(f, "elements"),
            FieldName::DynArrayClassAlloc => write!(f, "alloc"),
            FieldName::DynArrayClassElement => write!(f, "get_element"),
            FieldName::DynArrayClassLength => write!(f, "length"),
            FieldName::DynArrayLength => write!(f, "length"),
            FieldName::DynArrayElements => write!(f, "elements"),
            FieldName::BoxClassValue => write!(f, "get_value"),
            FieldName::BoxValue => write!(f, "value"),
            FieldName::VariantTag => write!(f, "tag"),
            FieldName::VariantData => write!(f, "data"),
            FieldName::VariantDataCase(case) => write!(f, "data_{}", case),
            FieldName::ClosureFunctionPointer => write!(f, "func_ptr"),
        }
    }
}
