mod class_def;
mod struct_def;
mod type_def;
mod variant_def;

pub use self::class_def::*;
pub use self::struct_def::*;
pub use self::type_def::*;
pub use self::variant_def::*;
use crate::ast::Unit;
use crate::ir;
use std::fmt;

#[allow(unused)]
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    // primitive tyeps
    Void,
    Int,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    PtrDiffType,
    SizeType,
    Bool,
    Float,
    UChar,
    SChar,
    Pointer(Box<Type>),

    // internal struct for class vtable etc
    Class,

    // rc state struct for ref types
    Rc,

    // custom type defined in source language
    DefinedType(TypeDefName),

    // fixed-size array type
    SizedArray(Box<Type>, usize),

    // literal function pointer (mostly for internal use)
    FunctionPointer {
        return_ty: Box<Type>,
        params: Vec<Type>,
    },
}

impl Type {
    pub fn from_ir_struct(id: ir::TypeDefID) -> Type {
        Type::DefinedType(TypeDefName::Struct(id))
    }

    pub fn from_ir_variant(id: ir::TypeDefID) -> Type {
        Type::DefinedType(TypeDefName::Variant(id))
    }
    
    pub fn from_metadata(ty: &ir::Type, module: &mut Unit) -> Type {
        match ty {
            ir::Type::Pointer(target) => Type::from_metadata(target.as_ref(), module).ptr(),
            ir::Type::Function(id) => Type::DefinedType(TypeDefName::Alias(*id)),
            ir::Type::RcPointer(ir::VirtualTypeID::Class(id))
            | ir::Type::RcWeakPointer(ir::VirtualTypeID::Class(id)) => {
                Type::DefinedType(TypeDefName::Struct(*id)).ptr()
            },
            ir::Type::RcPointer(..) | ir::Type::RcWeakPointer(..) => Type::Void.ptr(),
            ir::Type::Struct(id) => Type::from_ir_struct(*id),
            ir::Type::Variant(id) => Type::from_ir_variant(*id),
            ir::Type::Flags(repr_id, _set_id) => Type::from_ir_struct(*repr_id),
            
            ir::Type::Nothing => Type::Void,
            ir::Type::Bool => Type::Bool,
            ir::Type::F32 => Type::Float,
            ir::Type::I8 => Type::SChar,
            ir::Type::U8 => Type::UChar,
            ir::Type::I16 => Type::Int16,
            ir::Type::U16 => Type::UInt16,
            ir::Type::I32 => Type::Int32,
            ir::Type::U32 => Type::UInt32,
            ir::Type::I64 => Type::Int64,
            ir::Type::U64 => Type::UInt64,
            ir::Type::ISize => Type::PtrDiffType,
            ir::Type::USize => Type::SizeType,

            ir::Type::Array { element, dim } => {
                let element = Type::from_metadata(element, module);
                module.make_array_type(element, *dim)
            },
        }
    }

    pub fn ptr(self) -> Self {
        Type::Pointer(Box::new(self))
    }

    pub fn sized_array(self, size: usize) -> Self {
        Type::SizedArray(Box::new(self), size)
    }

    fn build_decl_string(&self, left: &mut String, right: &mut String) {
        match self {
            Type::Pointer(ty) => {
                ty.build_decl_string(left, right);
                left.push('*');
            },
            Type::Void => {
                left.push_str("void");
            },
            Type::Int => {
                left.push_str("int");
            },
            Type::Int16 => {
                left.push_str("int16_t");
            },
            Type::UInt16 => {
                left.push_str("uint16_t");
            },
            Type::Int32 => {
                left.push_str("int32_t");
            },
            Type::UInt32 => {
                left.push_str("uint32_t");
            },
            Type::Int64 => {
                left.push_str("int64_t");
            },
            Type::UInt64 => {
                left.push_str("uint64_t");
            },
            Type::Bool => {
                left.push_str("bool");
            },
            Type::Float => {
                left.push_str("float");
            },
            Type::UChar => {
                left.push_str("unsigned char");
            },
            Type::SChar => {
                left.push_str("signed char");
            },
            Type::SizeType => {
                left.push_str("size_t");
            },
            Type::PtrDiffType => {
                left.push_str("ptrdiff_t");
            },

            Type::DefinedType(name) => {
                name.build_decl_string(left, right);
            },

            Type::SizedArray(el, size) => {
                el.build_decl_string(left, right);
                right.push('[');
                right.push_str(&size.to_string());
                right.push(']');
            },

            Type::FunctionPointer { return_ty, params } => {
                left.push_str(&return_ty.typename());
                left.push_str("(*");

                right.push_str(")(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        right.push_str(", ");
                    }
                    right.push_str(&param.typename());
                }
                right.push(')');
            },

            Type::Rc => {
                left.push_str("struct Rc");
            },

            Type::Class => {
                left.push_str("struct Class");
            },
        }
    }

    pub fn collect_type_def_deps(&self, deps: &mut Vec<TypeDefName>) {
        match self {
            // direct structural reference
            Type::DefinedType(name) => deps.push(name.clone()),
            Type::SizedArray(element, ..) => element.collect_type_def_deps(deps),
            Type::FunctionPointer { params, return_ty } => {
                for param in params {
                    param.collect_type_def_deps(deps);
                }
                return_ty.collect_type_def_deps(deps);
            },

            // no custom types or types referenced by pointer (can use forward decl)
            _ => {},
        }
    }

    pub fn type_def_deps(&self) -> Vec<TypeDefName> {
        let mut deps = Vec::new();
        self.collect_type_def_deps(&mut deps);
        deps
    }

    pub fn to_decl_string<Name>(&self, name: &Name) -> String
    where
        Name: ?Sized + fmt::Display,
    {
        let mut left = String::new();
        let mut right = String::new();

        self.build_decl_string(&mut left, &mut right);

        format!("{} {} {}", left, name, right).trim().to_string()
    }

    pub fn typename(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int => "int".to_string(),
            Type::Int16 => "int16_t".to_string(),
            Type::UInt16 => "uint16_t".to_string(),
            Type::Int32 => "int32_t".to_string(),
            Type::UInt32 => "uint32_t".to_string(),
            Type::Int64 => "int64_t".to_string(),
            Type::UInt64 => "uint64_t".to_string(),
            Type::PtrDiffType => "ptrdiff_t".to_string(),
            Type::SizeType => "size_t".to_string(),
            Type::DefinedType(name) => match name {
                TypeDefName::Alias(..) => name.to_string(),
                TypeDefName::Struct(ir::STRING_ID) => String::from("STRING_STRUCT"),
                TypeDefName::Struct(ir::TYPEINFO_ID) => String::from("TYPEINFO_STRUCT"),
                TypeDefName::Struct(ir::METHODINFO_ID) => String::from("METHODINFO_STRUCT"),
                TypeDefName::Struct(ir::FUNCINFO_ID) => String::from("FUNCINFO_STRUCT"),
                _ => format!("struct {}", name),
            },
            Type::Rc => "struct Rc".to_string(),
            Type::Class => "struct Class".to_string(),
            Type::SizedArray(ty, ..) | Type::Pointer(ty) => format!("{}*", ty.typename()),
            Type::Bool => "bool".to_string(),
            Type::Float => "float".to_string(),
            Type::UChar => "unsigned char".to_string(),
            Type::SChar => "signed char".to_string(),
            Type::FunctionPointer { return_ty, params } => {
                let mut name = format!("{} (*)(", return_ty.typename());
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        name.push_str(", ");
                    }
                    name.push_str(&param.typename());
                }
                name.push(')');
                name
            },
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct TypeDecl {
    pub name: TypeDefName,
}

impl fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.name {
            TypeDefName::Alias(..) => write!(f, "{}", self.name),
            _ => write!(f, "struct {}", self.name),
        }
    }
}
