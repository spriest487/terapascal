use crate::ast;
use crate::codegen::builder::IRBuilder;
use crate::codegen::expr;
use crate::codegen::typ;
use crate::ir;
use crate::IntConstant;
use bigdecimal::BigDecimal;
use terapascal_common::span::Spanned;
use terapascal_ir::InstructionBuilder;

pub fn translate_bin_op(
    bin_op: &typ::ast::BinOp,
    out_ty: &typ::Type,
    builder: &mut IRBuilder,
) -> ir::Ref {
    if bin_op.lhs.annotation().is_namespace() {
        // there's nothing to actually translate on the lhs, it's just for name resolution
        return expr::translate_expr(&bin_op.rhs, builder);
    }

    let result_ty = builder.translate_type(out_ty);

    // the functions to translate IR and member operators return direct refs to the value,
    // while other operations return local temp values
    let out_is_ref = matches!(bin_op.op, ast::Operator::Period | ast::Operator::Index);

    builder.local_begin();

    let lhs_val = expr::translate_expr(&bin_op.lhs, builder);

    let out_val = match &bin_op.op {
        ast::Operator::Period => {
            // auto-deref for rc types
            let of_ty = builder.translate_type(&bin_op.lhs.annotation().ty());

            let struct_id = match &of_ty {
                ir::Type::Struct(id) => *id,
                ir::Type::Object(ir::ObjectID::Class(id)) => *id,
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
                .map(ast::Ident::to_string)
                .unwrap_or_else(|| panic!("rhs of member expression {bin_op} must be an ident"));

            let field = struct_def
                .find_field(&member_name)
                .expect("referenced field must exist");

            lhs_val.field_ref(of_ty, field).to_deref()
        },

        ast::Operator::Index => {
            let index_val = expr::expr_to_val(&bin_op.rhs, builder);

            expr::translate_indexer(
                &result_ty,
                lhs_val,
                index_val,
                &bin_op.lhs.annotation().ty(),
                builder,
            ).to_deref()
        },

        ast::Operator::NotEquals => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.eq(out_val, lhs_val, b);
            builder.not(out_val, out_val);

            out_val.to_ref()
        },

        ast::Operator::Equals => {
            let out_val = builder.local_var(result_ty.clone(), None);

            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = expr::translate_expr(&bin_op.rhs, builder);
                    builder.set_eq(out_val, lhs_val, b, set_type.as_ref())
                }
                
                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.eq(out_val, lhs_val, b);
                }
            }

            out_val.to_ref()
        },

        ast::Operator::Add => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.add(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::Mul => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.mul(out_val, lhs_val, b);

            out_val.to_ref()
        }

        ast::Operator::Mod => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.modulo(out_val, lhs_val, b);

            out_val.to_ref()
        }

        ast::Operator::FDiv => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.fdiv(out_val, lhs_val, b);

            out_val.to_ref()
        }

        ast::Operator::IDiv => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.idiv(out_val, lhs_val, b);

            out_val.to_ref()
        }

        ast::Operator::Gt => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.gt(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::Gte => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.gte(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::Lt => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.lt(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::Lte => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.lte(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::Sub => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.sub(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::Shl => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.shl(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::Shr => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.shr(out_val, lhs_val, b);

            out_val.to_ref()
        },

        ast::Operator::And => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let short_circuit = builder.next_label();
            let exit = builder.next_label();

            // if lhs is false, short circuit
            let is_short_circuit = builder.not_to_val(lhs_val.clone());
            builder.jmpif(short_circuit, is_short_circuit);

            // eval both sides
            let rhs = expr::expr_to_val(&bin_op.rhs, builder);
            builder.and(out_val, lhs_val, rhs);
            builder.jmp(exit);
            
            // short circuit: out := false
            builder.label(short_circuit);
            builder.mov(out_val, false);
            
            builder.label(exit);

            out_val.to_ref()
        },

        ast::Operator::Or => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let short_circuit = builder.next_label();
            let exit = builder.next_label();
            
            // if lhs is true, short circuit
            builder.jmpif(short_circuit, lhs_val.clone());

            // eval both sides
            let rhs = expr::expr_to_val(&bin_op.rhs, builder);
            builder.or(out_val, lhs_val, rhs);
            builder.jmp(exit);

            // short circuit: out := true
            builder.label(short_circuit);
            builder.mov(out_val, true);
            
            builder.label(exit);

            out_val.to_ref()
        },

        ast::Operator::BitAnd => {
            let out_val = builder.local_var(result_ty.clone(), None);

            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = expr::translate_expr(&bin_op.rhs, builder);
                    builder.mov(out_val, lhs_val);
                    builder.set_bit_and(out_val, b, set_type.as_ref());
                }

                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.bit_and(out_val, lhs_val, b);
                }
            }

            out_val.to_ref()
        },

        ast::Operator::BitOr => {
            let out_val = builder.local_var(result_ty.clone(), None);

            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = expr::translate_expr(&bin_op.rhs, builder);
                    builder.mov(out_val, lhs_val);
                    builder.set_bit_or(out_val, b, set_type.as_ref());
                }

                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.bit_or(out_val, lhs_val, b);
                }
            }

            out_val.to_ref()
        },

        ast::Operator::Caret => {
            let out_val = builder.local_var(result_ty.clone(), None);

            match out_ty {
                typ::Type::Set(set_type) => {
                    let b = expr::translate_expr(&bin_op.rhs, builder);
                    builder.mov(out_val, lhs_val);
                    builder.set_bit_xor(out_val, b, set_type.as_ref());
                }

                _ => {
                    let b = expr::expr_to_val(&bin_op.rhs, builder);
                    builder.bit_xor(out_val, lhs_val, b);
                }
            }

            out_val.to_ref()
        },
        
        ast::Operator::In => {
            let out_val = builder.local_var(result_ty.clone(), None);

            let break_label = builder.next_label();
            let fail_label = builder.next_label();
            
            let item = expr::expr_to_val(&bin_op.lhs, builder);
            
            let set_val = expr::translate_expr(&bin_op.rhs, builder);
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
            let bit_num_byte = builder.local_temp(ir::Type::U8);
            builder.cast(bit_num_byte.clone(), bit_num, ir::Type::U8);
            
            builder.set_contains(out_val, set_val, bit_num_byte, set_type.as_ref());
            builder.jmp(break_label);

            builder.label(fail_label);
            builder.mov(out_val, ir::Value::LiteralBool(false));

            builder.label(break_label);

            out_val.to_ref()
        }

        _ => unimplemented!("IR for op {}", bin_op.op),
    };

    if !out_is_ref {
        builder.retain_deep(out_val.clone(), &result_ty);
    }

    builder.local_end();

    out_val
}

fn set_bound_to_literal(value: IntConstant, value_type: &ir::Type) -> ir::Value {
    let lit_val = BigDecimal::from(value.as_i128());

    ir::Value::from_literal_val(lit_val, &value_type)
        .unwrap_or_else(|| panic!(
            "couldn't create a literal value from set bound value {} of type {}",
            value,
            value_type,
        ))
}

pub fn translate_unary_op(
    unary_op: &typ::ast::UnaryOp,
    out_ty: &typ::Type,
    builder: &mut IRBuilder,
) -> ir::Ref {
    let operand_ref = expr::translate_expr(&unary_op.operand, builder);

    match unary_op.op {
        ast::Operator::AddressOf => {
            let out_ty = builder.translate_type(out_ty);
            assert!(matches!(out_ty, ir::Type::Pointer(..)), "address-of expression must result in a pointer type");
            
            let out_val = builder.local_var(out_ty.clone(), None);

            builder.addr_of(out_val, operand_ref);
            
            // todo: should never do anything since pointers don't affect RC
            builder.retain_deep(out_val, &out_ty);

            out_val.to_ref()
        },

        ast::Operator::Caret if unary_op.pos == ast::UnaryPosition::Postfix => {
            let operand_type = unary_op.operand.annotation().ty();

            // deref syntax is overloaded for box types
            if let typ::Type::Box(value_type) = operand_type.as_ref() {
                let element_type = builder.translate_type(value_type.as_ref());
                let element_ref = builder.local_temp(element_type.clone().temp_ref());
                
                let box_type = builder.translate_type(operand_type.as_ref());
                builder.element(element_ref, operand_ref, ir::Value::LiteralI32(0), box_type);
                
                element_ref.to_deref()
            } else {
                if !operand_type.is_typed_pointer() {
                    panic!("operand of deref operator must be a pointer, was {operand_type} @ {}", unary_op.span());
                }
                
                // the operand must be a pointer
                operand_ref.to_deref()
            }
        },

        ast::Operator::Sub => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_var(out_ty.clone(), None);

            let op_ty = unary_op.annotation.ty();

            let zero_val = match op_ty.as_ref() {
                typ::Type::Primitive(typ::Primitive::Int8) => ir::Value::LiteralI8(0),
                typ::Type::Primitive(typ::Primitive::UInt8) => ir::Value::LiteralU8(0),
                typ::Type::Primitive(typ::Primitive::Int16) => ir::Value::LiteralI16(0),
                typ::Type::Primitive(typ::Primitive::UInt16) => ir::Value::LiteralI16(0),
                typ::Type::Primitive(typ::Primitive::Int32) => ir::Value::LiteralI32(0),
                typ::Type::Primitive(typ::Primitive::UInt32) => ir::Value::LiteralU32(0),
                typ::Type::Primitive(typ::Primitive::Int64) => ir::Value::LiteralI64(0),
                typ::Type::Primitive(typ::Primitive::UInt64) => ir::Value::LiteralU64(0),
                typ::Type::Primitive(typ::Primitive::NativeInt) => ir::Value::LiteralISize(0),
                typ::Type::Primitive(typ::Primitive::NativeUInt) => ir::Value::LiteralUSize(0),
                typ::Type::Primitive(typ::Primitive::Real32) => ir::Value::LiteralF32(0.0),
                typ::Type::Primitive(typ::Primitive::Real64) => ir::Value::LiteralF64(0.0),
                _ => unimplemented!("IR for unary negation of {}", op_ty),
            };

            builder.sub(out_val.clone(), zero_val, operand_ref);

            out_val.to_ref()
        },

        ast::Operator::Add => {
            // just turns its operand into a temporary value
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_var(out_ty.clone(), None);
            builder.mov(out_val.clone(), operand_ref);

            out_val.to_ref()
        },

        ast::Operator::Not => {
            let out_val = builder.local_var(ir::Type::Bool, None);

            builder.not(out_val.clone(), operand_ref);

            out_val.to_ref()
        },
        
        ast::Operator::BitNot => {
            let result_ty = builder.translate_type(out_ty);
            let result_val = builder.local_var(result_ty, None);
            
            match out_ty {
                typ::Type::Set(set_type) => {
                    builder.mov(result_val.clone(), operand_ref.clone());
                    builder.set_bit_not(result_val.clone(), set_type.as_ref());
                }
                
                _ => {
                    builder.bit_not(result_val.clone(), operand_ref);
                }
            }
            
            result_val.to_ref()
        }

        op => unimplemented!("IR translation of unary operator {}", op),
    }
}
