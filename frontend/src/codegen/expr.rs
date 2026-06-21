pub mod call;
pub mod op;
pub mod ctor;
pub mod cond;

use crate::ast;
use crate::codegen::expr::call::translate_invocation;
use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_stmt;
use crate::codegen::typ;
use crate::codegen::IRBuilder;
use crate::typ::TypedValue;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::span::*;
use terapascal_ir::InstructionBuilder as _;

pub fn expr_to_val(expr: &typ::ast::Expr, builder: &mut IRBuilder) -> ir::Value {
    match expr.annotation() {
        typ::Value::Const(const_val) => {
            builder.literal_to_val(&const_val.value, &const_val.ty)
        }

        _ => {
            translate_expr(expr, builder).value()
        },
    }
}

pub fn translate_expr(expr: &typ::ast::Expr, builder: &mut IRBuilder) -> ir::Ref {
    builder.comment(expr.to_string());
    builder.push_source(expr.annotation().span().clone());

    let result_ref = match expr.annotation() {
        typ::Value::Const(const_val) => {
            builder.translate_literal(&const_val.value, &const_val.ty)
        }

        typ::Value::Invocation(invocation) => {
            translate_invocation(invocation, builder)
                .expect("invocation used as an expression must have a value")
        }

        typ::Value::Typed(..) => {
            match expr {
                ast::Expr::Literal(lit) => {
                    builder.translate_literal(&lit.literal, &lit.annotation.ty())
                },

                ast::Expr::BinOp(bin_op) => {
                    op::translate_bin_op(bin_op, &bin_op.annotation.ty(), builder)
                },

                ast::Expr::UnaryOp(unary_op) => {
                    op::translate_unary_op(unary_op, &unary_op.annotation.ty(), builder)
                },

                ast::Expr::Ident(ident, annotation) => {
                    translate_ident_expr(ident, annotation, builder)
                },

                ast::Expr::Call(call) => {
                    // eprintln!("translating call @ {}", call.span());
                    call::build_call(call, builder).expect("call used in expr must have a return value")
                },

                ast::Expr::ObjectCtor(ctor) => {
                    ctor::translate_object_ctor(ctor, builder)
                },

                ast::Expr::CollectionCtor(ctor) => {
                    ctor::translate_collection_ctor(ctor, builder)
                },

                ast::Expr::IfCond(if_cond) => translate_if_cond_expr(if_cond, builder)
                    .expect("conditional used in expr must have a type"),

                ast::Expr::Block(block) => {
                    let out_ty = match &block.output {
                        Some(output_expr) => builder.translate_type(&output_expr.annotation().ty()),
                        None => panic!("block used in expr must have a type"),
                    };
                    let out_ref = builder.local_var(out_ty, None).to_ref();
                    translate_block(block, out_ref.clone(), builder);

                    out_ref
                },

                ast::Expr::Raise(raise) => {
                    translate_raise(raise, builder)
                },

                ast::Expr::Exit(exit) => {
                    translate_exit(exit, builder);
                    ir::Ref::Discard
                },

                ast::Expr::Case(case) => {
                    cond::translate_case_expr(case, builder)
                },

                ast::Expr::Match(match_expr) => {
                    cond::translate_match_expr(match_expr, builder)
                },

                ast::Expr::Cast(cast) => {
                    translate_cast_expr(cast, builder)
                },

                ast::Expr::AnonymousFunction(def) => {
                    builder.build_closure_expr(def)
                },

                ast::Expr::Group(group) => {
                    translate_expr(&group.expr, builder)
                }

                ast::Expr::ExplicitSpec(..) => unreachable!(),
                ast::Expr::Incomplete(..) => unreachable!(),
            }
        }
        
        typ::Value::Function(func_val) => {
            let decl_sig = Arc::new(func_val.decl.sig());
            let func = builder.translate_func(&func_val.name, &decl_sig);
            
            // wrap the function reference in an invokable static closure
            builder.build_function_closure(&func)
        }
        
        // TODO: other types of function pointers
        | typ::Value::UfcsFunction(_)
        | typ::Value::Method(_)

        | typ::Value::Untyped(_)
        | typ::Value::Type(_, _)
        | typ::Value::Namespace(_, _)
        | typ::Value::VariantCase(_)
        | typ::Value::Overload(_) => {
            panic!("{}: untranslatable value for expression {}: {}", expr.span(), expr, expr.annotation());
        }
    };

    builder.pop_source();

    result_ref
}

fn translate_indexer(
    val_ty: &ir::Type,
    base_ref: ir::Ref,
    index_val: ir::Value,
    base_ty: &typ::Type,
    builder: &mut IRBuilder,
) -> ir::Ref {
    match base_ty {
        typ::Type::Array(array_ty) => {
            let base_ty = builder.translate_type(base_ty);
            let len = i32::try_from(array_ty.dim)
                .expect("array dim must be within range of i32");
            let len_val = ir::Value::LiteralI32(len);

            builder.array_bounds_check(len_val, index_val.clone());
            base_ref.element_ref(base_ty, index_val)
        },

        typ::Type::DynArray(..) => {
            let base_ty = builder.translate_type(base_ty);

            base_ref.element_ref(base_ty, index_val)
        },

        typ::Type::Pointer(_) => {
            let result_ref = builder.local_temp(val_ty.temp_ref());

            builder.add(result_ref.clone(), base_ref, index_val);

            result_ref.to_ref()
        },

        unimpl => unimplemented!("IR for indexing into {}", unimpl),
    }
}

pub fn translate_if_cond_expr(
    if_cond: &typ::ast::IfCondExpr,
    builder: &mut IRBuilder,
) -> Option<ir::Ref> {
    cond::translate_if_cond(if_cond, builder, |branch, out_ref, out_ty, builder| {
        let val = expr_to_val(branch, builder);

        if let Some(out_ref) = out_ref.cloned() {
            builder.emit(ir::Instruction::Move {
                out: out_ref.clone(),
                new_val: val.into(),
            });
            builder.retain(out_ref, out_ty.clone());
        }
    })
}

pub fn translate_if_cond_stmt(
    if_cond: &typ::ast::IfCondStmt,
    builder: &mut IRBuilder,
) -> Option<ir::Ref> {
    cond::translate_if_cond(if_cond, builder, |branch, out_ref, _out_ty, builder| {
        assert!(
            out_ref.is_none(),
            "branch translated as stmt should not have an out location"
        );

        translate_stmt(&branch, builder);
    })
}

pub fn literal_to_val(
    lit: &typ::ast::Literal,
    ty: &ir::Type,
    lib: &mut LibraryBuilder,
) -> ir::Value {
    match lit {
        ast::Literal::Nil => {
            ir::Value::LiteralNil
        },

        ast::Literal::Boolean(b) => {
            ir::Value::LiteralBool(*b)
        },

        ast::Literal::Integer(int_val) => match ty {
            ir::Type::U8 => ir::Value::LiteralU8(int_val.as_u8().expect("invalid u8 literal value")),
            ir::Type::I8 => ir::Value::LiteralI8(int_val.as_i8().expect("invalid i8 literal value")),
            ir::Type::I16 => ir::Value::LiteralI16(int_val.as_i16().expect("invalid i16 literal value")),
            ir::Type::U16 => ir::Value::LiteralU16(int_val.as_u16().expect("invalid u16 literal value")),
            ir::Type::I32 => ir::Value::LiteralI32(int_val.as_i32().expect("invalid i32 literal value")),
            ir::Type::U32 => ir::Value::LiteralU32(int_val.as_u32().expect("invalid u32 literal value")),
            ir::Type::I64 => ir::Value::LiteralI64(int_val.as_i64().expect("invalid i64 literal value")),
            ir::Type::U64 => ir::Value::LiteralU64(int_val.as_u64().expect("invalid u64 literal value")),
            ir::Type::USize => ir::Value::LiteralUSize(int_val.as_usize().expect("invalid usize literal value")),
            ir::Type::ISize => ir::Value::LiteralISize(int_val.as_isize().expect("invalid isize literal value")),
            ir::Type::F32 => ir::Value::LiteralF32(int_val.as_f32().expect("invalid f32 literal value")),
            ir::Type::F64 => ir::Value::LiteralF64(int_val.as_f64().expect("invalid f64 literal value")),
            _ => panic!("bad type for integer literal: {}", ty),
        },

        ast::Literal::Real(real_val) => match ty {
            ir::Type::F32 => ir::Value::LiteralF32(real_val.as_f32().expect("invalid f32 literal value")),
            ir::Type::F64 => ir::Value::LiteralF64(real_val.as_f64().expect("invalid f64 literal value")),
            _ => panic!("bad type for real literal: {}", ty),
        }

        ast::Literal::String(s) => {
            if *ty != ir::Type::string() {
                panic!("bad type for string literal: {}", ty)
            }
            
            let lit_id = lib.metadata_mut().find_or_insert_string(s);
            let lit_ref = ir::GlobalRef::StringLiteral(lit_id);

            ir::Value::Ref(ir::Ref::Global(lit_ref))
        },

        ast::Literal::TypeInfo(ty) => {
            let of_ty = lib.translate_type(ty);
            let type_info_ref = ir::GlobalRef::StaticTypeInfo(Rc::new(of_ty));
            
            ir::Value::from(type_info_ref)
        }

        ast::Literal::SizeOf(ty) => {
            let sized_ty = lib.translate_type(ty);
            ir::Value::SizeOf(sized_ty)
        },

        ast::Literal::DefaultValue(ty) => {
            let value_ty = lib.translate_type(ty);
            ir::Value::Default(value_ty)
        }
    }
}

fn translate_ident_expr(ident: &ast::Ident, annotation: &typ::Value, builder: &mut IRBuilder) -> ir::Ref {
    match annotation {
        typ::Value::Function(func) => {
            let func = builder.translate_func(&func.name, &func.sig);
            
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
            let val_ref = find_named_binding(ident, builder)
                .or_else(|| find_global_ref(val, builder))
                .unwrap_or_else(|| {
                    panic!(
                        "identifier not found in local or global scope @ {}: {}",
                        annotation.span(),
                        ident,
                    )
                });

            val_ref
        },

        _ => panic!("wrong kind of node annotation for ident: {:?}", ident),
    }
}

fn find_named_binding(ident: &ast::Ident, builder: &IRBuilder) -> Option<ir::Ref> {
    let binding = builder.find_named(ident.name.as_str())?;
    Some(binding.to_ref())
}

fn find_global_ref(value: &TypedValue, builder: &IRBuilder) -> Option<ir::Ref> {
    // direct references to global variables by their ident will have the qualified name stored
    // in their "decl" property
    let decl_path = value.decl.as_ref()?;

    let var_id = builder.find_global_var(&decl_path)?;
    let global_ref = ir::Ref::from(ir::GlobalRef::Variable(var_id));
    
    Some(global_ref)
}

pub fn translate_block(block: &typ::ast::Block, out_ref: ir::Ref, builder: &mut IRBuilder) {
    let out_ty = match &block.output {
        Some(out_expr) => builder.translate_type(&out_expr.annotation().ty()),
        None => ir::Type::Nothing,
    };

    builder.local_begin();

    for stmt in &block.stmts {
        translate_stmt(stmt, builder);
    }

    if let Some(out_expr) = &block.output {
        let result_val = translate_expr(out_expr, builder);
        builder.mov(out_ref.clone(), result_val.clone());
        builder.retain(out_ref, out_ty);
    }

    builder.local_end();
}

pub fn translate_exit(exit: &typ::ast::Exit, builder: &mut IRBuilder) {
    if let ast::Exit::WithValue { value_expr, .. } = exit {
        let value_ty = builder.translate_type(&value_expr.annotation().ty());
        let value_val = translate_expr(value_expr, builder);

        // we can assume this function has a return register, otherwise an exit stmt
        // wouldn't pass typechecking
        builder.mov(ir::RESULT_REF, value_val);

        // we are effectively reassigning the return ref, so like a normal assignment, we need
        // retain the new value to make it outlive the scope the exit expr appears in
        builder.retain(ir::RESULT_REF, value_ty);
    }

    builder.exit_function();
}

pub fn translate_raise(raise: &typ::ast::Raise, builder: &mut IRBuilder) -> ir::Ref {
    let val = translate_expr(&raise.value, builder);

    builder.emit(ir::Instruction::Raise { val: val.clone() });

    ir::Ref::Discard
}

fn translate_cast_expr(cast: &typ::ast::Cast, builder: &mut IRBuilder) -> ir::Ref {
    let val = translate_expr(&cast.expr, builder);
    let ty = builder.translate_type(&cast.as_type);
    let out_ref = builder.local_temp(ty.clone());

    builder.cast(out_ref.clone(), val, ty);

    out_ref.to_ref()
}
