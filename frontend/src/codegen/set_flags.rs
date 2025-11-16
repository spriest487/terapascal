use crate::codegen::builder::IRBuilder;
use crate::codegen::library_builder::LibraryBuilder;
use crate::ir;
use crate::Operator;
use terapascal_ir::instruction_builder::InstructionBuilder;

pub const WORD_TYPE: ir::Type = ir::Type::U64;
const WORD_BITS: usize = u64::BITS as usize;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SetFlagsType {
    pub struct_id: ir::TypeDefID,

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

impl SetFlagsType {
    // full-size 256-bit flag struct, the max number of values supported by
    // delphi/FPC sets
    pub fn define_new(lib: &mut LibraryBuilder, bit_count: usize) -> Self {        
        let word_count = set_word_count(bit_count);

        let set_flags_struct = ir::StructDef {
            identity: ir::StructIdentity::SetFlags { bits: bit_count },
            fields: (0..word_count)
                .map(|id| (ir::FieldID(id), ir::StructFieldDef {
                    name: None,
                    ty: WORD_TYPE,
                }))
                .collect(),
        };

        let struct_id = lib.metadata_mut().new_type();
        lib.metadata_mut().define_struct(struct_id, set_flags_struct);
        
        let include_func = Self::define_include(struct_id, word_count, lib);
        let exclude_func = Self::define_exclude(struct_id, word_count, lib);
        let contains_func = Self::define_contains(struct_id, word_count, lib);
        
        let bit_and_func = Self::define_bitwise_bin_op(
            struct_id,
            word_count,
            Operator::BitAnd,
            lib,
            |builder, out, a, b| builder.bit_and(out, a, b)
        );
        let bit_or_func = Self::define_bitwise_bin_op(
            struct_id,
            word_count,
            Operator::BitOr,
            lib,
            |builder, out, a, b| builder.bit_or(out, a, b)
        );
        let bit_xor_func = Self::define_bitwise_bin_op(
            struct_id,
            word_count,
            Operator::Caret,
            lib,
            |builder, out, a, b| builder.bit_xor(out, a, b)
        );
        
        let bit_not_func = Self::define_bit_not(struct_id, word_count, lib);
        
        let eq_func = Self::define_eq(struct_id, word_count, lib);
        
        Self {
            struct_id,
            include_func,
            exclude_func,
            contains_func,

            bit_not_func,

            bit_and_func,
            bit_or_func,
            bit_xor_func,
            
            eq_func,
        }
    }

    fn define_func(
        name: String,
        body: Vec<ir::Instruction>,
        sig: ir::FunctionSig,
        lib: &mut LibraryBuilder
    ) -> ir::FunctionID {
        let name = if lib.opts().debug {
            Some(name)
        } else {
            None
        };

        let func = ir::Function::Local(ir::FunctionDef {
            sig,
            debug_name: name,
            body,
        });

        let func_id = lib.metadata_mut().insert_func(None, false);
        lib.insert_function(func_id, func);

        func_id
    }

    fn define_include(
        struct_id: ir::TypeDefID,
        word_count: usize,
        lib: &mut LibraryBuilder
    ) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = IRBuilder::new(lib);
        builder.bind_param(struct_ty.clone().temp_ref(), "flags");
        builder.bind_param(ir::Type::U8, "bit");

        let (word_ref, word_bit) = Self::find_word_bit(&mut builder, 0, word_count, struct_ty.clone());

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_ref := word_ref | (1 shl (word_bit as u64))  
        builder.cast(word_mask, word_bit, WORD_TYPE);
        builder.shl(word_mask, ir::Value::LiteralU64(1), word_mask);
        builder.bit_or(word_ref.clone(), word_mask, word_ref);
        
        let name = format!("operator include ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            ir::Type::Struct(struct_id).temp_ref(),
            ir::Type::U8,
        ], ir::Type::Nothing);
        
        Self::define_func(name, builder.finish(), sig, lib)
    }
    
    fn define_exclude(struct_id: ir::TypeDefID, word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = IRBuilder::new(lib);
        builder.bind_param(struct_ty.clone().temp_ref(), "flags");
        builder.bind_param(ir::Type::U8, "bit");

        let (word_ref, word_bit) = Self::find_word_bit(&mut builder, 0, word_count, struct_ty.clone());

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_ref := word_ref & ~(1 shl (word_bit as u64))  
        builder.cast(word_mask, word_bit, WORD_TYPE);
        builder.shl(word_mask, ir::Value::LiteralU64(1), word_mask);
        builder.bit_not(word_mask, word_mask);
        builder.bit_and(word_ref.clone(), word_mask, word_ref);
        
        let name = format!("operator exclude ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            ir::Type::Struct(struct_id).temp_ref(),
            ir::Type::U8,
        ], ir::Type::Nothing);
        
        Self::define_func(name, builder.finish(), sig, lib)
    }

    fn define_contains(struct_id: ir::TypeDefID, word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = IRBuilder::new(lib);
        builder.bind_return();
        builder.bind_param(struct_ty.clone().temp_ref(), "flags");
        builder.bind_param(ir::Type::U8, "bit");
        
        let (word_ref, word_bit) = Self::find_word_bit(&mut builder, 1, word_count, struct_ty.clone());

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_mask := 1 shl (word_bit as u64)
        builder.cast(word_mask, word_bit, WORD_TYPE);
        builder.shl(word_mask, ir::Value::LiteralU64(1), word_mask);
        
        // result := (word_mask & word_ref) = word_mask
        let val_and_mask = builder.local_temp(WORD_TYPE);
        builder.bit_and(val_and_mask, word_mask, word_ref);
        builder.eq(ir::RETURN_REF, word_mask, val_and_mask);
        
        let name = format!("operator in ({}-bit flags)", word_count * WORD_BITS);
        
        let sig = ir::FunctionSig::new([
            ir::Type::Struct(struct_id).temp_ref(),
            ir::Type::U8,
        ], ir::Type::Bool);

        Self::define_func(name, builder.finish(), sig, lib)
    }
    
    fn define_bitwise_bin_op(
        struct_id: ir::TypeDefID,
        word_count: usize,
        op: Operator,
        lib: &mut LibraryBuilder,
        build_op: impl Fn(&mut IRBuilder, ir::Ref, ir::Value, ir::Value)
    ) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = IRBuilder::new(lib);
        builder.bind_param(struct_ty.clone().temp_ref(), "flags");
        builder.bind_param(struct_ty.clone().temp_ref(), "other");

        let flags_ref = ir::LocalID(0).to_deref();
        let other_ref = ir::LocalID(1).to_deref();
        
        let word_field_ref = builder.local_temp(WORD_TYPE.temp_ref());
        let other_word_field_ref = builder.local_temp(WORD_TYPE.temp_ref());

        for word in 0..word_count {
            let field_id = ir::FieldID(word);
            builder.field(word_field_ref, flags_ref.clone(), struct_ty.clone(), field_id);
            builder.field(other_word_field_ref, other_ref.clone(), struct_ty.clone(), field_id);
            
            let word_val = word_field_ref.to_deref().value();
            let other_word_val = other_word_field_ref.to_deref().value();

            build_op(&mut builder, word_field_ref.to_deref(), word_val, other_word_val);
        }

        let name = format!("operator {} ({}-bit flags)", op, word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            ir::Type::Struct(struct_id).temp_ref(),
            ir::Type::Struct(struct_id).temp_ref(),
        ], ir::Type::Nothing);

        Self::define_func(name, builder.finish(), sig, lib)
    }
    
    fn define_bit_not(struct_id: ir::TypeDefID, word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = IRBuilder::new(lib);
        builder.bind_param(struct_ty.clone().temp_ref(), "flags");

        let flags_ref = ir::Ref::Local(ir::LocalID(0)).to_deref();
        
        let word_field_ref = builder.local_temp(WORD_TYPE.temp_ref());
        for word in 0..word_count {
            let field_id = ir::FieldID(word);
            builder.field(word_field_ref, flags_ref.clone(), struct_ty.clone(), field_id);
            builder.bit_not(word_field_ref.to_deref(), word_field_ref.to_deref());
        }
        
        let name = format!("operator ~ ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            ir::Type::Struct(struct_id).temp_ref(),
        ], ir::Type::Nothing);
        
        Self::define_func(name, builder.finish(), sig, lib)
    }

    fn define_eq(struct_id: ir::TypeDefID, word_count: usize, lib: &mut LibraryBuilder) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);
        
        let mut builder = IRBuilder::new(lib);
        builder.bind_return();
        builder.bind_param(struct_ty.clone().temp_ref(), "flags");
        builder.bind_param(struct_ty.clone().temp_ref(), "other");

        let flags_arg = ir::LocalID(1);
        let other_arg = ir::LocalID(2);

        let word_field_ref = builder.local_temp(WORD_TYPE.temp_ref());
        let other_word_field_ref = builder.local_temp(WORD_TYPE.temp_ref());

        builder.mov(ir::RETURN_REF, ir::Value::LiteralBool(true));

        let word_eq_var = builder.local_temp(ir::Type::Bool);

        for word in 0..word_count {
            let field_id = ir::FieldID(word);
            builder.field(word_field_ref, flags_arg.to_deref(), struct_ty.clone(), field_id);
            builder.field(other_word_field_ref, other_arg.to_deref(), struct_ty.clone(), field_id);

            // result := result and (word = other_word)
            builder.eq(word_eq_var, word_field_ref.to_deref(), other_word_field_ref.to_deref());
            builder.and(ir::RETURN_REF, ir::RETURN_REF, word_eq_var);
        }
        
        let name = format!("operator = ({}-bit flags)", word_count * WORD_BITS);
        let sig = ir::FunctionSig::new([
            ir::Type::Struct(struct_id).temp_ref(),
            ir::Type::Struct(struct_id).temp_ref(),
        ], ir::Type::Bool);

        Self::define_func(name, builder.finish(), sig, lib)
    }

    // expected locals:
    // %0: pointer to a struct with fields of consecutive 64-bit words
    // %1: U8 bit number in the 0-255 range 
    // returns (ref to 64-bit word, bit in the 0-63 range) 
    fn find_word_bit(
        builder: &mut IRBuilder,
        first_arg: usize,
        word_count: usize,
        struct_ty: ir::Type
    ) -> (ir::Ref, ir::Value) {
        let result = builder.local_temp(WORD_TYPE.temp_ref());
        let skip_flag = builder.local_temp(ir::Type::Bool);
        
        let self_ptr_arg = ir::LocalID(first_arg);
        let bit_arg = ir::LocalID(first_arg + 1);
        
        let break_label = builder.next_label();
        
        let word_bit = builder.local_temp(ir::Type::U8);

        // this doesn't have to be super smart for now
        for word in 0..word_count {
            let skip_label = if word < word_count - 1 {
                let skip_label = builder.next_label();
                let next_word_start = ir::Value::LiteralU8(((word + 1) * WORD_BITS) as u8);

                builder.gte(skip_flag, bit_arg, next_word_start);
                builder.jmpif(skip_label, skip_flag);
                Some(skip_label)
            } else {
                None
            };
            
            let self_ref = self_ptr_arg.to_deref();
            let field_id = ir::FieldID(word);

            builder.field(result, self_ref, struct_ty.clone(), field_id);
            
            if word > 0 {
                let word_start = ir::Value::LiteralU8((word * WORD_BITS) as u8);
                builder.sub(word_bit, bit_arg, word_start);
            } else {
                builder.mov(word_bit, bit_arg);
            }

            builder.jmp(break_label);

            if let Some(skip_label) = skip_label {
                builder.label(skip_label);
            }
        }
        
        builder.label(break_label);

        (result.to_deref(), ir::Value::from(word_bit))
    }
}
