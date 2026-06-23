use crate::codegen::builder::IRBuilder;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::metadata::NamePathExt;
use crate::ir;
use crate::typ::SetType;
use crate::typ::Type;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::Operator;
use bigdecimal::ToPrimitive;
use ir::InstructionBuilder as _;
use ir::StructLayout;
use std::num::Wrapping;
use std::rc::Rc;

pub const WORD_TYPE: ir::Type = ir::Type::U64;
const WORD_BITS: usize = u64::BITS as usize;

pub const SET_TAG_NAME: &str = "PascalSetType";
pub const SET_TAG_MIN_FIELD: ir::FieldID = ir::FieldID(0);
pub const SET_TAG_MAX_FIELD: ir::FieldID = ir::FieldID(1);
pub const SET_TAG_ITEM_TYPE_FIELD: ir::FieldID = ir::FieldID(2);

#[derive(Copy, Clone)]
pub struct SetTypeTagInfo {
    pub class_id: ir::TypeDefID,

    pub min_field: ir::FieldID,
    pub max_field: ir::FieldID,

    pub item_type_field: ir::FieldID,
}

impl SetTypeTagInfo {
    pub fn find_in_metadata(metadata: &impl ir::MetadataSource) -> Option<Self> {
        let name_path = ir::NamePath::new([SYSTEM_UNIT_NAME.to_string()], SET_TAG_NAME);

        let (id, _def) = metadata.find_struct_def(&name_path)?;

        Some(Self {
            class_id: id,
            item_type_field: SET_TAG_ITEM_TYPE_FIELD,
            max_field: SET_TAG_MAX_FIELD,
            min_field: SET_TAG_MIN_FIELD,
        })
    }
}

fn flags_repr_type(word_count: usize) -> ir::Type {
    match word_count {
        1 => ir::Type::U64,
        _ => ir::Type::U64.array(word_count),
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct SetFlagsType {
    pub struct_id: ir::TypeDefID,

    pub repr_type: FlagsReprType,
}

impl SetFlagsType {
    // full-size 256-bit flag struct, the max number of values supported by
    // delphi/FPC sets
    pub fn translate(lib: &mut LibraryBuilder, set_type: &SetType) -> Self {
        let struct_id = match &set_type.name {
            Some(ident_path) => {
                let name_path = ir::NamePath::from_ident_path(ident_path, []);
                lib.metadata_mut().forward_declare_type(&name_path)
            }

            None => {
                lib.metadata_mut().new_type()
            }
        };


        // this only needs to be defined the first time this is called for any given set type
        if !lib.metadata().is_defined(&struct_id.to_struct_type([])) {
            Self::define_set_struct(set_type, struct_id, lib);
        }

        let flags_type = lib.get_flags_repr_type(set_type.flags_type_bits());

        Self {
            struct_id,
            repr_type: flags_type,
        }
    }

    fn define_set_struct(set_type: &SetType, id: ir::TypeDefID, lib: &mut LibraryBuilder) {
        let flags_type = lib.get_flags_repr_type(set_type.flags_type_bits());

        let struct_identity = match &set_type.name {
            Some(ident_path) => {
                let name_path = ir::NamePath::from_parts(ident_path.to_string_path());
                ir::StructIdentity::Record(name_path)
            }

            None => {
                ir::StructIdentity::Internal(format!("<{}-bit set>", set_type.flags_type_bits()))
            }
        };

        let mut set_struct = ir::StructDef::new(struct_identity, StructLayout::Packed);
        set_struct.fields.insert(ir::FieldID(0), ir::StructFieldDef::new(flags_type.repr_type()));

        if let Some(set_tag_info) = Self::build_tag(lib, set_type) {
            set_struct.tags.push(set_tag_info);
        }

        lib.metadata_mut().define_struct(id, set_struct);
    }

    fn build_tag(lib: &mut LibraryBuilder, set_type: &SetType) -> Option<ir::TagInfo> {
        let min_val = Wrapping(set_type.min.as_i128()).to_i64()?;
        let max_val = Wrapping(set_type.max.as_i128()).to_i64()?;

        // enums don't have an IR type def, but do reserve a unique ID. when reading a library,
        // we assume any struct type referenced as a set item type is referring to an enum ID
        let item_type = match &set_type.item_type {
            Type::Enum(path) => {
                let enum_name_path = ir::NamePath::from_ident_path(path, []);
                let enum_id= lib.metadata_mut().forward_declare_type(&enum_name_path);

                enum_id.to_struct_type([])
            },

            ty => {
                lib.translate_type(ty)
            },
        };

        let item_type_ref = ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(Rc::new(item_type)));

        let tag_path = ir::NamePath::new([SYSTEM_UNIT_NAME.to_string()], SET_TAG_NAME);

        let tag_class_id = lib.metadata_mut().forward_declare_type(&tag_path);

        let mut tag_info = ir::TagInfo::new(tag_class_id);
        tag_info.fields.insert(SET_TAG_MIN_FIELD, ir::Value::LiteralI64(min_val));
        tag_info.fields.insert(SET_TAG_MAX_FIELD, ir::Value::LiteralI64(max_val));
        tag_info.fields.insert(SET_TAG_ITEM_TYPE_FIELD, item_type_ref.value());

        Some(tag_info)
    }

    pub fn flags_ref(&self, self_ref: impl Into<ir::Ref>) -> ir::Ref {
        let set_type = self.struct_id.to_struct_type([]);

        self_ref
            .into()
            .field_ref(set_type, ir::FieldID(0))
            .to_deref()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FlagsReprType {
    pub word_count: usize,

    // function (self: ^Self; bit: UInt8);
    pub include_func: ir::FunctionID,

    // function (self: ^Self; bit: UInt8);
    pub exclude_func: ir::FunctionID,
    
    // function (self: ^Self, bit: UInt8): Boolean;
    pub contains_func: ir::FunctionID,

    // function Equals(self, other: ^Self): Boolean;
    pub eq_func: ir::FunctionID,

    // these functions all mutate "self" flags
    
    // function (self: ^Self);
    pub bit_not_func: ir::FunctionID,
    
    // function (self, other: ^Self);
    pub bit_and_func: ir::FunctionID,
    pub bit_or_func: ir::FunctionID,
    pub bit_xor_func: ir::FunctionID,
}

pub fn set_word_count(bit_count: usize) -> usize {
    usize::div_ceil(bit_count, WORD_BITS)
}

impl FlagsReprType {
    pub fn repr_type(&self) -> ir::Type {
        flags_repr_type(self.word_count)
    }

    pub fn word_ref(&self, self_ref: impl Into<ir::Ref>, word: usize) -> ir::Ref {
        Self::make_word_ref(self_ref, self.word_count, word)
    }

    pub fn build(lib: &mut LibraryBuilder, bit_count: usize) -> FlagsReprType {
        let word_count = set_word_count(bit_count);

        let include_func = Self::define_include(word_count, lib);
        let exclude_func = Self::define_exclude(word_count, lib);
        let contains_func = Self::define_contains(word_count, lib);

        let bit_and_func = Self::define_bitwise_bin_op(
            word_count,
            Operator::BitAnd,
            lib,
            |builder, out, a, b| builder.bit_and(out, a, b)
        );
        let bit_or_func = Self::define_bitwise_bin_op(
            word_count,
            Operator::BitOr,
            lib,
            |builder, out, a, b| builder.bit_or(out, a, b)
        );
        let bit_xor_func = Self::define_bitwise_bin_op(
            word_count,
            Operator::Caret,
            lib,
            |builder, out, a, b| builder.bit_xor(out, a, b)
        );

        let bit_not_func = Self::define_bit_not(word_count, lib);

        let eq_func = Self::define_eq(word_count, lib);
        
        FlagsReprType {
            word_count,
            contains_func,
            include_func,
            exclude_func,
            bit_and_func,
            bit_or_func,
            bit_xor_func,
            eq_func,
            bit_not_func,
        }
    }

    fn define_func(
        name: String,
        body: ir::InstructionList,
        sig: ir::FunctionSig,
        lib: &mut LibraryBuilder,
    ) -> ir::FunctionID {
        let debug_name = lib.opts().debug.then(|| name.clone());
        let identity = ir::FunctionIdentity::internal(name);

        let sig = Rc::new(sig);

        let func = ir::Function::Local(ir::FunctionDef {
            sig: sig.clone(),
            type_params: Vec::new(),
            debug_name,
            body,
        });

        let func_id = lib.metadata_mut().insert_func(identity, sig, false, []);
        lib.insert_function(func_id, func);

        func_id
    }

    fn define_include(
        word_count: usize,
        lib: &mut LibraryBuilder
    ) -> ir::FunctionID {
        let flags_ty = flags_repr_type(word_count);

        let mut builder = IRBuilder::new(lib);
        let self_arg = ir::ArgID(0);
        let bit_arg = ir::ArgID(1);

        builder.bind_param(self_arg, flags_ty.temp_ref(), "flags");
        builder.bind_param(bit_arg, ir::Type::U8, "bit");

        let (word_ref, word_bit) = Self::find_word_bit(
            &mut builder,
            self_arg.to_deref(),
            bit_arg,
            word_count
        );

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_ref := word_ref | (1 shl (word_bit as u64))  
        builder.cast(word_mask, word_bit, WORD_TYPE);
        builder.shl(word_mask, ir::Value::LiteralU64(1), word_mask);
        builder.bit_or(word_ref.clone(), word_mask, word_ref);
        
        let name = format!("operator include ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            flags_ty.temp_ref(),
            ir::Type::U8,
        ], ir::Type::Nothing);
        
        Self::define_func(name, builder.finish(), sig, lib)
    }
    
    fn define_exclude(word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let flags_ty = flags_repr_type(word_count);

        let mut builder = IRBuilder::new(lib);
        let self_arg = ir::ArgID(0);
        let bit_arg = ir::ArgID(1);

        builder.bind_param(self_arg, flags_ty.temp_ref(), "flags");
        builder.bind_param(bit_arg, ir::Type::U8, "bit");

        let (word_ref, word_bit) = Self::find_word_bit(
            &mut builder,
            self_arg.to_deref(),
            bit_arg,
            word_count);

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_ref := word_ref & ~(1 shl (word_bit as u64))  
        builder.cast(word_mask, word_bit, WORD_TYPE);
        builder.shl(word_mask, ir::Value::LiteralU64(1), word_mask);
        builder.bit_not(word_mask, word_mask);
        builder.bit_and(word_ref.clone(), word_mask, word_ref);
        
        let name = format!("operator exclude ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            flags_ty.temp_ref(),
            ir::Type::U8,
        ], ir::Type::Nothing);
        
        Self::define_func(name, builder.finish(), sig, lib)
    }

    fn define_contains(word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let flags_ty = flags_repr_type(word_count);

        let mut builder = IRBuilder::new(lib);
        builder.bind_result(ir::Type::Bool);

        let self_arg = ir::ArgID(0);
        let bit_arg = ir::ArgID(1);

        builder.bind_param(self_arg, flags_ty.temp_ref(), "flags");
        builder.bind_param(bit_arg, ir::Type::U8, "bit");

        let (word_ref, word_bit) = Self::find_word_bit(
            &mut builder,
            self_arg.to_deref(),
            bit_arg,
            word_count);

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_mask := 1 shl (word_bit as u64)
        builder.cast(word_mask, word_bit, WORD_TYPE);
        builder.shl(word_mask, ir::Value::LiteralU64(1), word_mask);
        
        // result := (word_mask & word_ref) = word_mask
        let val_and_mask = builder.local_temp(WORD_TYPE);
        builder.bit_and(val_and_mask, word_mask, word_ref);
        builder.eq(ir::RESULT_REF, word_mask, val_and_mask);
        
        let name = format!("operator in ({}-bit flags)", word_count * WORD_BITS);
        
        let sig = ir::FunctionSig::new([
            flags_ty.temp_ref(),
            ir::Type::U8,
        ], ir::Type::Bool);

        Self::define_func(name, builder.finish(), sig, lib)
    }
    
    fn define_bitwise_bin_op(
        word_count: usize,
        op: Operator,
        lib: &mut LibraryBuilder,
        build_op: impl Fn(&mut IRBuilder, ir::Ref, ir::Value, ir::Value)
    ) -> ir::FunctionID {
        let flags_ty = flags_repr_type(word_count);

        let mut builder = IRBuilder::new(lib);
        let flags_arg = ir::ArgID(0);
        let other_arg = ir::ArgID(1);

        builder.bind_param(flags_arg, flags_ty.temp_ref(), "flags");
        builder.bind_param(other_arg, flags_ty.temp_ref(), "other");

        for word in 0..word_count {
            let word_field_ref = Self::make_word_ref(flags_arg.to_deref(), word_count, word);
            let other_word_field_ref = Self::make_word_ref(other_arg.to_deref(), word_count, word);

            build_op(
                &mut builder,
                word_field_ref.clone(),
                word_field_ref.value(),
                other_word_field_ref.value(),
            );
        }

        let name = format!("operator {} ({}-bit flags)", op, word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            flags_ty.temp_ref(),
            flags_ty.temp_ref(),
        ], ir::Type::Nothing);

        Self::define_func(name, builder.finish(), sig, lib)
    }
    
    fn define_bit_not(word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let flags_ty = flags_repr_type(word_count);

        let mut builder = IRBuilder::new(lib);
        
        let flags_arg = ir::ArgID(0);
        builder.bind_param(flags_arg, flags_ty.temp_ref(), "flags");

        for word in 0..word_count {
            let word_ref = Self::make_word_ref(flags_arg.to_deref(), word_count, word);
            builder.bit_not(word_ref.clone(), word_ref);
        }

        let name = format!("operator ~ ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            flags_ty.temp_ref(),
        ], ir::Type::Nothing);

        Self::define_func(name, builder.finish(), sig, lib)
    }

    fn define_eq(word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let flags_ty = flags_repr_type(word_count);

        let mut builder = IRBuilder::new(lib);
        builder.bind_result(ir::Type::Bool);
        let flags_arg = ir::ArgID(0);
        let other_arg = ir::ArgID(1);

        builder.bind_param(flags_arg, flags_ty.temp_ref(), "flags");
        builder.bind_param(other_arg, flags_ty.temp_ref(), "other");

        builder.mov(ir::RESULT_REF, ir::Value::LiteralBool(true));

        let word_eq_var = builder.local_temp(ir::Type::Bool);

        for word in 0..word_count {
            let word_field_ref = Self::make_word_ref(flags_arg.to_deref(), word_count, word);
            let other_word_field_ref = Self::make_word_ref(other_arg.to_deref(), word_count, word);

            // result := result and (word = other_word)
            builder.eq(word_eq_var,  word_field_ref, other_word_field_ref);
            builder.and(ir::RESULT_REF, ir::RESULT_REF, word_eq_var);
        }
        
        let name = format!("operator = ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            flags_ty.temp_ref(),
            flags_ty.temp_ref(),
        ], ir::Type::Bool);

        Self::define_func(name, builder.finish(), sig, lib)
    }

    fn make_word_ref(self_ref: impl Into<ir::Ref>, word_count: usize, word_index: usize) -> ir::Ref{
        if word_count == 1 {
            self_ref.into()
        } else {
            let index = i32::try_from(word_index).expect("word index out of range");
            self_ref.into().element_ref(flags_repr_type(word_count), index).to_deref()
        }
    }

    // returns (ref to 64-bit word, bit in the 0-63 range) 
    fn find_word_bit(
        builder: &mut IRBuilder,
        self_ref: impl Into<ir::Ref>,
        bit_ref: impl Into<ir::Ref>,
        word_count: usize,
    ) -> (ir::Ref, ir::Value) {
        if word_count == 1 {
            return (self_ref.into(), bit_ref.into().value())
        }

        let self_ref = self_ref.into();
        let bit_ref = bit_ref.into();
        
        let result_ref = builder.local_temp(WORD_TYPE.temp_ref());
        let skip_flag = builder.local_temp(ir::Type::Bool);
        
        let break_label = builder.next_label();
        
        let word_bit = builder.local_temp(ir::Type::U8);

        // this doesn't have to be super smart for now
        for word in 0..word_count {
            let skip_label = if word < word_count - 1 {
                let skip_label = builder.next_label();
                let next_word_start = ir::Value::LiteralU8(((word + 1) * WORD_BITS) as u8);

                builder.gte(skip_flag, bit_ref.clone(), next_word_start);
                builder.jmpif(skip_label, skip_flag);
                Some(skip_label)
            } else {
                None
            };

            builder.make_ref(result_ref, Self::make_word_ref(self_ref.clone(), word_count, word));

            if word > 0 {
                let word_start = ir::Value::LiteralU8((word * WORD_BITS) as u8);
                builder.sub(word_bit, bit_ref.clone(), word_start);
            } else {
                builder.mov(word_bit, bit_ref.clone());
            }

            builder.jmp(break_label);

            if let Some(skip_label) = skip_label {
                builder.label(skip_label);
            }
        }
        
        builder.label(break_label);

        (result_ref.to_deref(), word_bit.value())
    }
}
