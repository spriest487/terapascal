pub mod call;
pub mod op;
pub mod ctor;
pub mod cond;

use crate::ast;
use crate::codegen::expr::call::translate_invocation;
use crate::codegen::ir;
use crate::codegen::translate_stmt;
use crate::codegen::typ;
use crate::codegen::Builder;
use crate::typ::TypedValue;
use crate::typ::STRING_TYPE_NAME;
use crate::typ::SYSTEM_UNIT_NAME;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::span::*;

pub fn expr_to_val(expr: &typ::ast::Expr, builder: &mut Builder) -> ir::Value {
    match expr.annotation() {
        typ::Value::Const(const_val) => {
            literal_to_val(&const_val.value, &const_val.ty, builder)
        }

        _ => ir::Value::Ref(translate_expr(expr, builder)),
    }
}

pub fn translate_expr(expr: &typ::ast::Expr, builder: &mut Builder) -> ir::Ref {
    builder.comment(&expr);
    builder.push_debug_context(expr.annotation().span().clone());

    let result_ref = match expr.annotation() {
        typ::Value::Const(const_val) => {
            translate_literal(&const_val.value, &const_val.ty, builder)
        }

        typ::Value::Invocation(invocation) => {
            translate_invocation(invocation, builder)
                .expect("invocation used as an expression must have a value")
        }
        
        typ::Value::Typed(..) => {
            match expr {
                ast::Expr::Literal(lit) => {
                    translate_literal(&lit.literal, &lit.annotation.ty(), builder)
                },

                ast::Expr::BinOp(bin_op) => {
                    op::translate_bin_op(bin_op, &bin_op.annotation.ty(), builder)
                },

                ast::Expr::UnaryOp(unary_op) => {
                    op::translate_unary_op(unary_op, &unary_op.annotation.ty(), builder)
                },

                ast::Expr::Ident(ident, annotation) => translate_ident_expr(ident, annotation, builder),

                ast::Expr::Call(call) => {
                    // eprintln!("translating call @ {}", call.span());
                    call::build_call(call, builder).expect("call used in expr must have a return value")
                },

                ast::Expr::ObjectCtor(ctor) => ctor::translate_object_ctor(ctor, builder),

                ast::Expr::CollectionCtor(ctor) => ctor::translate_collection_ctor(ctor, builder),

                ast::Expr::IfCond(if_cond) => translate_if_cond_expr(if_cond, builder)
                    .expect("conditional used in expr must have a type"),

                ast::Expr::Block(block) => {
                    let out_ty = match &block.output {
                        Some(output_expr) => builder.translate_type(&output_expr.annotation().ty()),
                        None => panic!("block used in expr must have a type"),
                    };
                    let out_ref = builder.local_new(out_ty, None);
                    translate_block(block, out_ref.clone(), builder);

                    out_ref
                },

                ast::Expr::Raise(raise) => translate_raise(raise, builder),
                ast::Expr::Exit(exit) => {
                    translate_exit(exit, builder);
                    ir::Ref::Discard
                },

                ast::Expr::Case(case) => cond::translate_case_expr(case, builder),
                ast::Expr::Match(match_expr) => cond::translate_match_expr(match_expr, builder),

                ast::Expr::Cast(cast) => translate_cast_expr(cast, builder),

                ast::Expr::AnonymousFunction(def) => builder.build_closure_expr(def),

                ast::Expr::ExplicitSpec(..) => unreachable!(),
            }
        }
        
        // todo function pointers
        typ::Value::Function(func_val) => {
            let decl_sig = Arc::new(func_val.decl.sig());
            let func = builder.translate_func(&func_val.name, &decl_sig, None);

            ir::Ref::from(ir::GlobalRef::Function(func.id))
        }
        
        | typ::Value::UfcsFunction(_)
        | typ::Value::Method(_)

        | typ::Value::Untyped(_)
        | typ::Value::Type(_, _)
        | typ::Value::Namespace(_, _)
        | typ::Value::VariantCase(_)
        | typ::Value::Overload(_) => {
            panic!("untranslatable value for expression {}: {}", expr, expr.annotation());
        }
    };

    builder.pop_debug_context();

    result_ref
}

fn translate_indexer(
    val_ty: &ir::Type,
    base_ref: ir::Ref,
    index_val: ir::Value,
    base_ty: &typ::Type,
    builder: &mut Builder,
) -> ir::Ref {
    match base_ty {
        typ::Type::Array(array_ty) => {
            let element_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.begin_scope();

            let element_ty = builder.translate_type(&array_ty.element_ty);
            let len = cast::i32(array_ty.dim).expect("array dim must be within range of i32");
            let len_val = ir::Value::LiteralI32(len);

            builder.bounds_check(&element_ty, len_val, index_val.clone());

            builder.append(ir::Instruction::Element {
                out: element_ptr.clone(),
                a: base_ref,
                index: index_val,
                element: element_ty,
            });

            builder.end_scope();

            element_ptr
        },

        typ::Type::DynArray { element } => {
            let element_type = builder.translate_type(element);
            let element_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.begin_scope();

            let arr_field_ptr = builder.local_temp(val_ty.clone().ptr().ptr());
            let len_field_ptr = builder.local_temp(ir::Type::I32.ptr());

            let array_struct = builder.translate_dyn_array_struct(&element);
            let array_class = ir::VirtualTypeID::Class(array_struct);
            let array_class_ty = ir::Type::RcPointer(array_class);

            builder.field(
                len_field_ptr.clone(),
                base_ref.clone(),
                array_class_ty.clone(),
                ir::DYNARRAY_LEN_FIELD,
            );
            builder.field(
                arr_field_ptr.clone(),
                base_ref.clone(),
                array_class_ty,
                ir::DYNARRAY_PTR_FIELD,
            );

            builder.bounds_check(&element_type, len_field_ptr.to_deref(), index_val.clone());

            // array_ptr := (array_field_ptr)^
            // element_ptr := array_ptr + index
            let array_ptr = arr_field_ptr.to_deref();
            builder.add(element_ptr.clone(), array_ptr, index_val);

            builder.end_scope();

            element_ptr
        },

        typ::Type::Pointer(_) => {
            let result_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.add(result_ptr.clone(), base_ref, index_val);

            result_ptr
        },

        unimpl => unimplemented!("IR for indexing into {}", unimpl),
    }
}

pub fn translate_if_cond_expr(
    if_cond: &typ::ast::IfCondExpression,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    cond::translate_if_cond(if_cond, builder, |branch, out_ref, out_ty, builder| {
        let val = expr_to_val(branch, builder);

        if let Some(out_ref) = out_ref.cloned() {
            builder.append(ir::Instruction::Move {
                out: out_ref.clone(),
                new_val: val.into(),
            });
            builder.retain(out_ref, &out_ty);
        }
    })
}

pub fn translate_if_cond_stmt(
    if_cond: &typ::ast::IfCondStatement,
    builder: &mut Builder,
) -> Option<ir::Ref> {
    cond::translate_if_cond(if_cond, builder, |branch, out_ref, _out_ty, builder| {
        assert!(
            out_ref.is_none(),
            "branch translated as stmt should not have an out location"
        );

        translate_stmt(&branch, builder);
    })
}

fn is_string_class(class: &typ::Symbol) -> bool {
    class.full_path.len() == 2
        && class.full_path.first().name.as_str() == SYSTEM_UNIT_NAME
        && class.ident().name.as_str() == STRING_TYPE_NAME
}

pub fn literal_to_val(
    lit: &ast::Literal<typ::Type>,
    ty: &typ::Type,
    builder: &mut Builder,
) -> ir::Value {
    match lit {
        ast::Literal::Nil => ir::Value::LiteralNull,

        ast::Literal::Boolean(b) => ir::Value::LiteralBool(*b),

        ast::Literal::Integer(i) => match ty {
            typ::Type::Primitive(typ::Primitive::Int8) => {
                let val = i
                    .as_i8()
                    .expect("Int8-typed constant must be within range of i8");
                ir::Value::LiteralI8(val)
            },
            typ::Type::Primitive(typ::Primitive::UInt8) => {
                let val = i
                    .as_u8()
                    .expect("UInt8-typed constant must be within range of u8");
                ir::Value::LiteralU8(val)
            },
            typ::Type::Primitive(typ::Primitive::Int16) => {
                let val = i
                    .as_i16()
                    .expect("Int16-typed constant must be within range of i16");
                ir::Value::LiteralI16(val)
            },
            typ::Type::Primitive(typ::Primitive::UInt16) => {
                let val = i
                    .as_u16()
                    .expect("Int16-typed constant must be within range of i16");
                ir::Value::LiteralU16(val)
            },
            typ::Type::Primitive(typ::Primitive::Int32) => {
                let val = i
                    .as_i32()
                    .expect("Int32-typed constant must be within range of i32");
                ir::Value::LiteralI32(val)
            },
            typ::Type::Primitive(typ::Primitive::UInt32) => {
                let val = i
                    .as_u32()
                    .expect("Int32-typed constant must be within range of u32");
                ir::Value::LiteralU32(val)
            },
            typ::Type::Primitive(typ::Primitive::Int64) => {
                let val = i
                    .as_i64()
                    .expect("Int64-typed constant must be within range of i64");
                ir::Value::LiteralI64(val)
            },
            typ::Type::Primitive(typ::Primitive::UInt64) => {
                let val = i
                    .as_u64()
                    .expect("Int64-typed constant must be within range of u64");
                ir::Value::LiteralU64(val)
            },
            typ::Type::Primitive(typ::Primitive::NativeInt) => {
                let val = i
                    .as_isize()
                    .expect("Int64-typed constant must be within range of isize");
                ir::Value::LiteralISize(val)
            },
            typ::Type::Primitive(typ::Primitive::NativeUInt) => {
                let val = i
                    .as_usize()
                    .expect("Int64-typed constant must be within range of usize");
                ir::Value::LiteralUSize(val)
            },
            typ::Type::Primitive(typ::Primitive::Real32) => {
                let val = i
                    .as_f32()
                    .expect("Real-typed constant must be within range of f32");
                ir::Value::LiteralF32(val)
            },
            typ::Type::Enum(..) => {
                let val = i
                    .as_isize()
                    .expect("Enum-typed constant must be within range of isize");
                ir::Value::LiteralISize(val)
            }

            _ => panic!("bad type for integer literal: {}", ty),
        },

        ast::Literal::Real(r) => match ty {
            typ::Type::Primitive(typ::Primitive::Real32) => {
                let val = r
                    .as_f32()
                    .expect("Real32-typed constant must be within range of f32");
                ir::Value::LiteralF32(val)
            },
            _ => panic!("bad type for real literal: {}", ty),
        },

        ast::Literal::String(s) => {
            match ty {
                typ::Type::Class(class) if is_string_class(class) => {
                    let lit_id = builder.find_or_insert_string(s);
                    let lit_ref = ir::GlobalRef::StringLiteral(lit_id);

                    ir::Value::Ref(ir::Ref::Global(lit_ref))
                },
                _ => panic!("bad type for string literal: {}", ty),
            }
        },
        
        ast::Literal::TypeInfo(ty) => {
            let ty = builder.translate_type(ty);
            let type_info_ref = ir::GlobalRef::StaticTypeInfo(Rc::new(ty));
            
            ir::Value::from(type_info_ref)
        }

        ast::Literal::SizeOf(ty) => {
            let ty = builder.translate_type(ty);
            ir::Value::SizeOf(ty)
        },

        ast::Literal::DefaultValue(ty) => {
            let ir_ty = builder.translate_type(ty);
            match ir_ty {
                ir::Type::Pointer(_)
                | ir::Type::RcPointer(_)
                | ir::Type::RcWeakPointer(_)
                | ir::Type::Function(_) => {
                    ir::Value::LiteralNull
                },

                ir::Type::Bool => ir::Value::LiteralBool(false),
                ir::Type::U8 => ir::Value::LiteralU8(0),
                ir::Type::I8 => ir::Value::LiteralI8(0),
                ir::Type::I16 => ir::Value::LiteralI16(0),
                ir::Type::U16 => ir::Value::LiteralU16(0),
                ir::Type::I32 => ir::Value::LiteralI32(0),
                ir::Type::U32 => ir::Value::LiteralU32(0),
                ir::Type::I64 => ir::Value::LiteralI64(0),
                ir::Type::U64 => ir::Value::LiteralU64(0),
                ir::Type::USize => ir::Value::LiteralUSize(0),
                ir::Type::ISize => ir::Value::LiteralISize(0),
                ir::Type::F32 => ir::Value::LiteralF32(0.0),
                
                ir::Type::Struct(..)
                | ir::Type::Flags(..)
                | ir::Type::Variant(..)
                | ir::Type::Array { .. }
                | ir::Type::Nothing => {
                    let size_expr = ir::Value::SizeOf(ir_ty.clone());
                    let temp_ref = builder.local_temp(ir_ty.clone());
                    
                    gen_fill_byte(temp_ref.clone(), ir_ty, size_expr, ir::Value::LiteralU8(0), builder);
                    
                    ir::Value::Ref(temp_ref)
                }
            }
        }
    }
}

pub fn translate_literal(
    lit: &ast::Literal<typ::Type>,
    ty: &typ::Type,
    builder: &mut Builder,
) -> ir::Ref {
    let out_ty = builder.translate_type(ty);
    let out = builder.local_temp(out_ty);

    let val = literal_to_val(lit, ty, builder);
    builder.mov(out.clone(), val);

    out
}

// inline IR for FillByte-style memory set procedure
fn gen_fill_byte(at: ir::Ref, at_ty: ir::Type, count: ir::Value, byte_val: ir::Value, builder: &mut Builder) {
    builder.comment(&format!("fill_byte: {}, count={}, value {}", at, count, byte_val));
    
    let byte_ptr_ty = ir::Type::U8.ptr();
    
    // dst_ptr := @at as ^UInt8
    let at_addr = builder.local_temp(at_ty.ptr());
    builder.addr_of(at_addr.clone(), at);

    let dst_ptr = builder.local_temp(byte_ptr_ty.clone());
    builder.cast(dst_ptr.clone(), at_addr, byte_ptr_ty.clone());
    
    // end_ptr := dst_ptr + count 
    let end_ptr = builder.local_temp(byte_ptr_ty.clone());
    builder.add(end_ptr.clone(), dst_ptr.clone(), count);
    
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();
    
    builder.label(continue_label);
    
    // at_end := dst_ptr = end_ptr
    let at_end = builder.local_temp(ir::Type::Bool);
    builder.eq(at_end.clone(), dst_ptr.clone(), end_ptr);
    
    // if at_end then break
    builder.jmpif(break_label, at_end);
    
    // else dst_ptr^ := byte_val
    builder.mov(dst_ptr.clone().to_deref(), byte_val.clone());

    // dst_ptr += 1;
    builder.add(dst_ptr.clone(), dst_ptr.clone(), ir::Value::LiteralISize(1));
    
    // continue
    builder.jmp(continue_label);

    builder.label(break_label);
}

fn translate_ident_expr(ident: &ast::Ident, annotation: &typ::Value, builder: &mut Builder) -> ir::Ref {
    match annotation {
        typ::Value::Function(func) => {
            let func = builder.translate_func(&func.name, &func.sig, None);
            
            // references to functions by value are turned into references to the static
            // closure for that function
            builder.build_function_closure(&func)
        },
        
        // standalone no-args invocation a function
        typ::Value::Invocation(invocation) => {
            translate_invocation(invocation, builder)
                .expect("invocation used as an expression must have a result type")
        }

        typ::Value::Typed(val) => {
            let val_ref = find_local_ref(ident, builder)
                .or_else(|| find_global_ref(val, builder))
                .unwrap_or_else(|| {
                    panic!(
                        "identifier not found in local or global scope @ {}: {}",
                        annotation.span(),
                        ident,
                    )
                });

            match val.value_kind {
                // ident lvalues are evaluated as pointers to the original values. they don't need
                // to be refcounted separately, because if they have a name, they must exist
                // in a scope at least as wide as the current one
                typ::ValueKind::Immutable | typ::ValueKind::Mutable | typ::ValueKind::Uninitialized => {
                    let ref_ty = builder.translate_type(&annotation.ty());
                    let ref_temp = builder.local_temp(ref_ty.ptr());

                    builder.append(ir::Instruction::AddrOf {
                        out: ref_temp.clone(),
                        a: val_ref,
                    });
                    ref_temp.to_deref()
                },

                // ident rvalue - just evaluate it
                typ::ValueKind::Temporary => val_ref,
            }
        },

        _ => panic!("wrong kind of node annotation for ident: {:?}", ident),
    }
}

fn find_local_ref(ident: &ast::Ident, builder: &Builder) -> Option<ir::Ref> {
    let local = builder.find_local(ident.name.as_str())?;
    
    let value_ref = ir::Ref::Local(local.id());
    if local.by_ref() {
        Some(value_ref.to_deref())
    } else {
        Some(value_ref)
    }
}

fn find_global_ref(value: &TypedValue, builder: &Builder) -> Option<ir::Ref> {
    // direct references to global variables by their ident will have the qualified name stored
    // in their "decl" property
    let decl_path = value.decl.as_ref()?;

    let var_id = builder.find_global_var(&decl_path)?;
    let global_ref = ir::Ref::from(ir::GlobalRef::Variable(var_id));
    
    Some(global_ref)
}

pub fn translate_block(block: &typ::ast::Block, out_ref: ir::Ref, builder: &mut Builder) {
    let out_ty = match &block.output {
        Some(out_expr) => builder.translate_type(&out_expr.annotation().ty()),
        None => ir::Type::Nothing,
    };

    builder.begin_scope();

    for stmt in &block.stmts {
        translate_stmt(stmt, builder);
    }

    if let Some(out) = &block.output {
        let result_val = translate_expr(out, builder);
        builder.mov(out_ref, result_val.clone());
        builder.retain(result_val, &out_ty);
    }

    builder.end_scope();
}

pub fn translate_exit(exit: &typ::ast::Exit, builder: &mut Builder) {
    if let ast::Exit::WithValue { value_expr, .. } = exit {
        let value_ty = builder.translate_type(&value_expr.annotation().ty());
        let value_val = translate_expr(value_expr, builder);

        // we can assume this function has a return register, otherwise an exit stmt
        // wouldn't pass typechecking
        builder.mov(ir::RETURN_REF, value_val);

        // we are effectively reassigning the return ref, so like a normal assignment, we need
        // retain the new value to make it outlive the scope the exit expr appears in
        builder.retain(ir::RETURN_REF.clone(), &value_ty);
    }

    builder.exit_function();
}

pub fn translate_raise(raise: &typ::ast::Raise, builder: &mut Builder) -> ir::Ref {
    let val = translate_expr(&raise.value, builder);

    builder.append(ir::Instruction::Raise { val: val.clone() });

    ir::Ref::Discard
}

fn translate_cast_expr(cast: &typ::ast::Cast, builder: &mut Builder) -> ir::Ref {
    let val = translate_expr(&cast.expr, builder);
    let ty = builder.translate_type(&cast.as_type);
    let out_ref = builder.local_temp(ty.clone());

    builder.cast(out_ref.clone(), val, ty);

    out_ref
}
