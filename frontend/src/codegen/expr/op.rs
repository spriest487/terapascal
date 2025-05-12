use crate::codegen::builder::Builder;
use crate::codegen::syn;
use crate::codegen::typ;
use crate::codegen::expr;
use crate::IntConstant;
use bigdecimal::BigDecimal;
use ir_lang::*;
use syn::Operator;
use crate::codegen::expr::translate_expr;

pub fn translate_bin_op(
    bin_op: &typ::ast::BinOp,
    out_ty: &typ::Type,
    builder: &mut Builder,
) -> Ref {
    if bin_op.lhs.annotation().is_namespace() {
        // there's nothing to actually translate on the lhs, it's just for name resolution
        return expr::translate_expr(&bin_op.rhs, builder);
    }

    let result_ty = builder.translate_type(out_ty);

    // the functions to translate IR and member operators return pointers to the value
    let (out_val, out_is_ptr) = match bin_op.op {
        Operator::Period | Operator::Index => {
            let out_val = builder.local_new(result_ty.clone().ptr(), None);
            (out_val, true)
        },

        _ => {
            let out_val = builder.local_new(result_ty.clone(), None);
            (out_val, false)
        },
    };

    builder.begin_scope();
    let lhs_val = expr::translate_expr(&bin_op.lhs, builder);

    match &bin_op.op {
        syn::Operator::Period => {
            // auto-deref for rc types
            let of_ty = builder.translate_type(&bin_op.lhs.annotation().ty());

            let struct_id = match &of_ty {
                Type::Struct(id) => *id,
                Type::RcPointer(VirtualTypeID::Class(id)) => *id,
                other => panic!(
                    "lhs ty_def of member binop must be a struct or class, was: {}",
                    other
                ),
            };

            let struct_def = builder
                .get_struct(struct_id)
                .unwrap_or_else(|| panic!("struct {struct_id} referenced in expression {bin_op} must exist"));

            let member_name = bin_op
                .rhs
                .as_ident()
                .map(syn::Ident::to_string)
                .unwrap_or_else(|| panic!("rhs of member expression {bin_op} must be an ident"));

            let field = struct_def
                .find_field(&member_name)
                .expect("referenced field must exist");

            builder.field(out_val.clone(), lhs_val, of_ty, field);
        },

        syn::Operator::Index => {
            let index_val = expr::expr_to_val(&bin_op.rhs, builder);
            let element_val = expr::translate_indexer(
                &result_ty,
                lhs_val,
                index_val,
                &bin_op.lhs.annotation().ty(),
                builder,
            );

            builder.mov(out_val.clone(), element_val);
        },

        syn::Operator::NotEquals => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.eq(out_val.clone(), lhs_val, b);
            builder.not(out_val.clone(), out_val.clone());
        },

        syn::Operator::Equals => {
            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = translate_expr(&bin_op.rhs, builder);
                    builder.set_eq(out_val.clone(), lhs_val, b, set_type.as_ref())
                }
                
                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.eq(out_val.clone(), lhs_val, b);
                }
            }
        },

        syn::Operator::Add => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.add(out_val.clone(), lhs_val, b);
        },

        syn::Operator::Mul => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.mul(out_val.clone(), lhs_val, b);
        }

        syn::Operator::Mod => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.modulo(out_val.clone(), lhs_val, b);
        }

        syn::Operator::FDiv => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.fdiv(out_val.clone(), lhs_val, b);
        }

        syn::Operator::IDiv => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.idiv(out_val.clone(), lhs_val, b);
        }

        syn::Operator::Gt => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.gt(out_val.clone(), lhs_val, b);
        },

        syn::Operator::Gte => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.gte(out_val.clone(), lhs_val, b);
        },

        syn::Operator::Lt => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.lt(out_val.clone(), lhs_val, b);
        },

        syn::Operator::Lte => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.lte(out_val.clone(), lhs_val, b);
        },

        syn::Operator::Sub => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.sub(out_val.clone(), lhs_val, b);
        },

        syn::Operator::Shl => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.shl(out_val.clone(), lhs_val, b);
        },

        syn::Operator::Shr => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.shr(out_val.clone(), lhs_val, b);
        },

        syn::Operator::And => {
            let short_circuit = builder.alloc_label();
            let exit = builder.alloc_label();

            // if lhs is false, short circuit
            let is_short_circuit = builder.not_to_val(lhs_val.clone());
            builder.jmpif(short_circuit, is_short_circuit);

            // eval both sides
            let rhs = expr::expr_to_val(&bin_op.rhs, builder);
            builder.and(out_val.clone(), lhs_val, rhs);
            builder.jmp(exit);
            
            // short circuit: out := false
            builder.label(short_circuit);
            builder.mov(out_val.clone(), false);
            
            builder.label(exit);
        },

        syn::Operator::Or => {
            let short_circuit = builder.alloc_label();
            let exit = builder.alloc_label();
            
            // if lhs is true, short circuit
            builder.jmpif(short_circuit, lhs_val.clone());

            // eval both sides
            let rhs = expr::expr_to_val(&bin_op.rhs, builder);
            builder.or(out_val.clone(), lhs_val, rhs);
            builder.jmp(exit);

            // short circuit: out := true
            builder.label(short_circuit);
            builder.mov(out_val.clone(), true);
            
            builder.label(exit);
        },

        syn::Operator::BitAnd => {
            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = translate_expr(&bin_op.rhs, builder);
                    builder.mov(out_val.clone(), lhs_val);
                    builder.set_bit_and(out_val.clone(), b, set_type.as_ref());
                }

                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.bit_and(out_val.clone(), lhs_val, b);
                }
            }
        },

        syn::Operator::BitOr => {
            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = translate_expr(&bin_op.rhs, builder);
                    builder.mov(out_val.clone(), lhs_val);
                    builder.set_bit_or(out_val.clone(), b, set_type.as_ref());
                }

                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.bit_or(out_val.clone(), lhs_val, b);
                }
            }
        },

        syn::Operator::Caret => {
            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = translate_expr(&bin_op.rhs, builder);
                    builder.mov(out_val.clone(), lhs_val);
                    builder.set_bit_xor(out_val.clone(), b, set_type.as_ref());
                }

                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.bit_xor(out_val.clone(), lhs_val, b);
                }
            }
        },
        
        syn::Operator::In => {
            let break_label = builder.alloc_label();
            let fail_label = builder.alloc_label();
            
            let item = expr::expr_to_val(&bin_op.lhs, builder);
            
            let set_val = translate_expr(&bin_op.rhs, builder);
            let rhs_type = bin_op.rhs.annotation().ty();

            let set_type = rhs_type
                .as_set()
                .expect("rhs of `in` expression must be a set type value");
            
            let value_type = builder.translate_type(&set_type.item_type);
            
            let min_lit = set_bound_to_literal(set_type.min, &value_type);
            let max_lit = set_bound_to_literal(set_type.max, &value_type);

            // if bit is not in range
            let lt_min = builder.lt_to_val(item.clone(), min_lit.clone());
            let gt_max = builder.gt_to_val(item.clone(), max_lit);
            let out_of_range = builder.or_to_value(lt_min, gt_max);
            builder.jmpif(fail_label, out_of_range.clone());

            // calc bit number
            let bit_num = builder.sub_to_val(item, min_lit, &value_type);
            let bit_num_byte = builder.local_temp(Type::U8);
            builder.cast(bit_num_byte.clone(), bit_num, Type::U8);
            
            builder.set_contains(out_val.clone(), set_val, bit_num_byte, set_type.as_ref());
            builder.jmp(break_label);

            builder.label(fail_label);
            builder.mov(out_val.clone(), Value::LiteralBool(false));

            builder.label(break_label);
        }

        _ => unimplemented!("IR for op {}", bin_op.op),
    };

    if !out_is_ptr {
        builder.retain(out_val.clone(), &result_ty);
    }

    builder.end_scope();

    if out_is_ptr {
        out_val.to_deref()
    } else {
        out_val
    }
}

fn set_bound_to_literal(value: IntConstant, value_type: &Type) -> Value {
    let lit_val = BigDecimal::from(value.as_i128());

    Value::from_literal_val(lit_val, &value_type)
        .unwrap_or_else(|| panic!(
            "couldn't create a literal value from set bound value {} of type {}",
            value,
            value_type,
        ))
}

pub fn translate_unary_op(
    unary_op: &typ::ast::UnaryOp,
    out_ty: &typ::Type,
    builder: &mut Builder,
) -> Ref {
    let operand_ref = expr::translate_expr(&unary_op.operand, builder);

    match unary_op.op {
        syn::Operator::AddressOf => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            builder.append(Instruction::AddrOf {
                out: out_val.clone(),
                a: operand_ref,
            });
            builder.retain(out_val.clone(), &out_ty);

            out_val
        },

        syn::Operator::Caret => operand_ref.to_deref(),

        syn::Operator::Sub => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            let op_ty = unary_op.annotation.ty();

            let zero_val = match op_ty.as_ref() {
                typ::Type::Primitive(typ::Primitive::Int8) => Value::LiteralI8(0),
                typ::Type::Primitive(typ::Primitive::UInt8) => Value::LiteralU8(0),
                typ::Type::Primitive(typ::Primitive::Int16) => Value::LiteralI16(0),
                typ::Type::Primitive(typ::Primitive::UInt16) => Value::LiteralI16(0),
                typ::Type::Primitive(typ::Primitive::Int32) => Value::LiteralI32(0),
                typ::Type::Primitive(typ::Primitive::UInt32) => Value::LiteralU32(0),
                typ::Type::Primitive(typ::Primitive::Int64) => Value::LiteralI64(0),
                typ::Type::Primitive(typ::Primitive::UInt64) => Value::LiteralU64(0),
                typ::Type::Primitive(typ::Primitive::NativeInt) => Value::LiteralISize(0),
                typ::Type::Primitive(typ::Primitive::NativeUInt) => Value::LiteralUSize(0),
                typ::Type::Primitive(typ::Primitive::Real32) => Value::LiteralF32(0.0),
                _ => unimplemented!("IR for unary negation of {}", op_ty),
            };

            builder.sub(out_val.clone(), zero_val, operand_ref);

            out_val
        },

        syn::Operator::Add => {
            // just turns its operand into a temporary value
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            builder.mov(out_val.clone(), operand_ref);

            out_val
        },

        syn::Operator::Not => {
            let out_val = builder.local_new(Type::Bool, None);

            builder.not(out_val.clone(), operand_ref);

            out_val
        },
        
        syn::Operator::BitNot => {
            let result_ty = builder.translate_type(out_ty);
            let result_val = builder.local_new(result_ty, None);
            
            match out_ty {
                typ::Type::Set(set_type) => {
                    builder.mov(result_val.clone(), operand_ref.clone());
                    builder.set_bit_not(result_val.clone(), set_type.as_ref());
                }
                
                _ => {
                    builder.bit_not(result_val.clone(), operand_ref);
                }
            }
            
            result_val
        }

        op => unimplemented!("IR translation of unary operator {}", op),
    }
}
