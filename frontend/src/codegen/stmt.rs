use crate::ast;
use crate::codegen::builder::jmp_exists;
use crate::codegen::expr::call;
use crate::codegen::expr::call::translate_invocation;
use crate::codegen::expr::expr_to_val;
use crate::codegen::expr::translate_raise;
use crate::codegen::ir;
use crate::codegen::pattern::translate_pattern_match;
use crate::codegen::translate_block;
use crate::codegen::translate_exit;
use crate::codegen::translate_expr;
use crate::codegen::translate_if_cond_stmt;
use crate::codegen::typ;
use crate::codegen::Builder;
use crate::typ::system_option_type_of;
use crate::typ::OPTION_NONE_CASE;
use crate::typ::OPTION_SOME_CASE;
use terapascal_common::span::Spanned;

pub fn translate_stmt(stmt: &typ::ast::Stmt, builder: &mut Builder) {
    builder.push_debug_context(stmt.annotation().span().clone());
    builder.comment(stmt);
    
    match stmt {
        ast::Stmt::Ident(..) | ast::Stmt::Member(..) => {
            let typ::Value::Invocation(invocation) = stmt.annotation() else {
                unreachable!("this statement should only pass typechecking as an invocation");
            };
            
            translate_invocation(invocation, builder);
        }

        ast::Stmt::LocalBinding(binding) => {
            build_binding(binding, builder);
        },

        ast::Stmt::Call(call) => {
            call::build_call(call, builder);
        },

        ast::Stmt::Block(block) => {
            translate_block(block, ir::Ref::Discard, builder);
        },

        ast::Stmt::Exit(exit) => {
            translate_exit(exit, builder);
        },

        ast::Stmt::ForLoop(for_loop) => {
            build_for_loop(for_loop, builder);
        },

        ast::Stmt::WhileLoop(while_loop) => {
            translate_while_loop(while_loop, builder);
        },

        ast::Stmt::Assignment(assignment) => {
            translate_assignment(assignment, builder);
        },

        ast::Stmt::CompoundAssignment(assignment) => {
            translate_compound_assignment(assignment, builder);
        },

        ast::Stmt::If(if_stmt) => {
            translate_if_cond_stmt(if_stmt, builder);
        },

        ast::Stmt::Raise(raise) => {
            translate_raise(raise, builder);
        },

        ast::Stmt::Break(_) => {
            builder.break_loop();
        },

        ast::Stmt::Continue(_) => {
            builder.continue_loop();
        },

        ast::Stmt::Case(case) => {
            translate_case_stmt(case, builder);
        },

        ast::Stmt::Match(match_stmt) => {
            translate_match_stmt(match_stmt, builder);
        },

        ast::Stmt::IncompleteExpr(expr) => {
            panic!("illegal state: {expr} is not a complete expression")
        }
    }

    builder.pop_debug_context()
}

fn build_binding(binding: &typ::ast::LocalBinding, builder: &mut Builder) {
    let bound_ty = builder.translate_type(&binding.ty);

    let binding_ref = builder.local_new(bound_ty.clone(), Some(binding.name.to_string()));

    if let Some(init_expr) = &binding.val {
        builder.scope(|builder| {
            let val = expr_to_val(init_expr, builder);

            builder.mov(binding_ref.clone(), val);
            builder.retain(binding_ref, &bound_ty);
        });
    };
}

pub fn build_for_loop(for_loop: &typ::ast::ForLoop, builder: &mut Builder) {
    match &for_loop.range {
        ast::ForLoopRange::UpTo(range) => {
            build_for_loop_up_to(range, &for_loop.body, builder);
        }

        ast::ForLoopRange::InSequence(range) => {
            build_for_loop_sequence(range, &for_loop.body, builder);
        }
    }
}

fn build_for_loop_up_to(
    range: &ast::ForLoopCounterRange<typ::Value>,
    body: &typ::ast::Stmt,
    builder: &mut Builder,
) {
    builder.scope(|builder| {
        let (counter_val, counter_init_val, counter_ty) = match &range.init {
            ast::ForLoopCounterInit::Assignment { counter, value, .. } => {
                let counter_ty = builder.translate_type(counter.annotation().ty().as_ref());

                let counter_ref = translate_expr(counter, builder);
                let init_val = expr_to_val(&value, builder);

                (counter_ref, init_val, counter_ty)
            }

            ast::ForLoopCounterInit::Binding { name, init, ty, .. } => {
                let counter_binding_name = name.to_string();
                let counter_ty = builder.translate_type(ty.ty());

                let counter_val = builder.local_new(counter_ty.clone(), Some(counter_binding_name));
                let init_val = expr_to_val(&init, builder);

                (counter_val, init_val, counter_ty)
            }
        };

        let to_val = expr_to_val(&range.to_expr, builder);

        let inc_val = match counter_ty {
            ir::Type::I8 => ir::Value::LiteralI8(1),
            ir::Type::U8 => ir::Value::LiteralU8(1),
            ir::Type::I16 => ir::Value::LiteralI16(1),
            ir::Type::U16 => ir::Value::LiteralU16(1),
            ir::Type::I32 => ir::Value::LiteralI32(1),
            ir::Type::U32 => ir::Value::LiteralU32(1),
            ir::Type::I64 => ir::Value::LiteralI64(1),
            ir::Type::U64 => ir::Value::LiteralU64(1),
            ir::Type::ISize => ir::Value::LiteralISize(1),
            ir::Type::USize => ir::Value::LiteralUSize(1),
            _ => ir::Value::LiteralI32(1),
        };

        build_for_loop_with_counter(
            counter_val,
            counter_init_val,
            inc_val,
            to_val,
            builder,
            |builder| translate_stmt(body, builder),
        );
    });
}

fn build_for_loop_with_counter<BodyFn>(
    counter_val: ir::Ref,
    counter_init_val: ir::Value,
    inc_val: ir::Value,
    high_val: ir::Value,
    builder: &mut Builder,
    body_fn: BodyFn,
) 
where
    BodyFn: FnOnce(&mut Builder)
{
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();
    
    let loop_instructions = builder.scope(|builder| {
        // temp value to store the result of evaluating the break condition
        let break_cond_val = builder.local_temp(ir::Type::Bool);

        builder.mov(counter_val.clone(), counter_init_val);

        builder.label(top_label);

        // break check: break_cond_val := counter_val > high_val
        builder.gt(break_cond_val.clone(), counter_val.clone(), high_val);

        // jump to break label if loop_val
        builder.jmpif(break_label, break_cond_val);

        let body_instructions = builder.loop_body_scope(continue_label, break_label, body_fn);

        if jmp_exists(body_instructions, continue_label) {
            builder.label(continue_label);
        }

        // counter_val := counter_val + inc_val
        builder.add(counter_val.clone(), counter_val.clone(), inc_val);

        // return to top of loop
        builder.append(ir::Instruction::Jump { dest: top_label });
    });

    if jmp_exists(&loop_instructions, break_label) {
        builder.label(break_label);
    }
}

fn build_for_loop_sequence(
    range: &ast::ForLoopSequenceRange<typ::Value>,
    body: &typ::ast::Stmt,
    builder: &mut Builder
) {
    builder.scope(|builder| {
        let src_ref = translate_expr(&range.src_expr, builder);

        let binding_ty = builder.translate_type(range.binding_ty.ty());
        let binding_name = range.binding_name.to_string();
        let binding_ref = builder.local_new(binding_ty.clone(), Some(binding_name));

        let counter_ref = builder.local_temp(ir::Type::I32);

        match &range.src_expr.annotation().ty().as_ref() {
            typ::Type::Array(array_ty) => {
                let high_index = i32::try_from(array_ty.dim)
                    .expect("array dimension is out of range for i32");

                if high_index == 0 {
                    return
                }
                
                let high_val = ir::Value::LiteralI32(high_index - 1);

                let element_ty = builder.translate_type(&array_ty.element_ty);

                build_array_sequence_loop(
                    counter_ref.clone(),
                    high_val,
                    binding_ref,
                    binding_ty,
                    body,
                    builder,
                    |builder| {
                        builder.element_to_val(src_ref, counter_ref, element_ty).value()
                    }
                );
            },
            
            typ::Type::DynArray { element } => {
                let element_ty = builder.translate_type(element);
                let dynarray_ty = builder.translate_type(&range.src_expr.annotation().ty());
                
                // high := seq_val.length
                let high_index_ref = builder.local_temp(ir::Type::I32);
                builder.field_val(
                    high_index_ref.clone(),
                    src_ref.clone(),
                    dynarray_ty.clone(),
                    ir::DYNARRAY_LEN_FIELD,
                    ir::Type::I32
                );
                
                // high -= 1;
                builder.sub(high_index_ref.clone(), high_index_ref.clone(), ir::Value::LiteralI32(1));

                let array_ty = element_ty.clone().ptr();
                let array_ptr = builder.field_to_val(src_ref, dynarray_ty, ir::DYNARRAY_PTR_FIELD, array_ty.clone());

                build_array_sequence_loop(
                    counter_ref.clone(),
                    high_index_ref,
                    binding_ref,
                    binding_ty,
                    body,
                    builder,
                    |builder| {
                        let element_ptr = builder.local_temp(array_ty);
                        builder.add(element_ptr.clone(), array_ptr, counter_ref.clone());

                        element_ptr.to_deref().value()
                    }
                );
            },
            
            src_ty => {
                // typechecker must have verified that this type is sequence compatible
                let seq_support = builder
                    .find_type_seq_support(src_ty)
                    .unwrap_or_else(|| {
                        panic!("type {} is not sequence compatible", src_ty);
                    });

                let seq_method = builder.translate_method(
                    (**src_ty).clone(),
                    seq_support.sequence_method_index,
                    None
                );

                let next_method = builder.translate_method(
                    seq_support.sequence_type.clone(),
                    seq_support.item_next_method_index,
                    None,
                );

                let src_ty = builder.translate_type(src_ty);

                let seq_ty = builder.translate_type(&seq_support.sequence_type);
                let seq_ref = builder.local_new(seq_ty.clone(), None);

                // call Sequence with a pointer to the source if it's a value type
                let src_self_arg_ref = if src_ty.is_rc() {
                    src_ref.clone()
                } else {
                    let src_ref_ptr = builder.local_temp(src_ty.ptr());
                    builder.addr_of(src_ref_ptr.clone(), src_ref);
                    src_ref_ptr
                };

                // call Next with a pointer to the sequence if it's a value type
                let seq_self_arg_ref = if seq_ty.is_rc() {
                    seq_ref.clone()
                } else {
                    let seq_ref_ptr = builder.local_temp(seq_ty.ptr());
                    builder.addr_of(seq_ref_ptr.clone(), seq_ref.clone());
                    seq_ref_ptr
                };
                
                let next_result_ty = system_option_type_of(seq_support.item_type.clone());
                let item_option_ty = builder.translate_type(&typ::Type::variant(next_result_ty));
                
                // vardata gives us a pointer so we can't copy the value straight into the binding   
                let item_option_data_ref = builder.local_temp(binding_ty.clone().ptr());

                // seq_ref := src_ref.Sequence();
                builder.call(seq_method.id, [src_self_arg_ref.value()], Some(seq_ref.clone()));

                // stores the option resulting from calling Next. don't need to RC this,
                // it'll either return an item and be stored in the binding local and retained there,
                // or return None and will never need retaining in that case
                let next_item_option_ref = builder.local_temp(item_option_ty.clone());
                let next_item_tag_ptr_ref = builder.local_temp(ir::Type::I32.ptr());
                
                let continue_label = builder.alloc_label();
                let break_label = builder.alloc_label();
                let top_label = builder.alloc_label();

                let loop_instructions = builder.scope(|builder| {
                    builder.label(top_label);

                    // next item in the sequence
                    // next_item_option_ref := sequence.Next();
                    builder.call(next_method.id, [seq_self_arg_ref.value()], Some(next_item_option_ref.clone()));

                    // if the case is None, break
                    // next_item_tag_ref := next_item_option_ref.tag
                    builder.vartag(next_item_tag_ptr_ref.clone(), next_item_option_ref.clone(), item_option_ty.clone());

                    let is_end_val = builder.eq_to_val(next_item_tag_ptr_ref.to_deref(), ir::Value::LiteralI32(OPTION_NONE_CASE as i32));
                    builder.jmpif(break_label, is_end_val);

                    // binding_ref := next_item_option_ref.Get()
                    builder.release(binding_ref.clone(), &binding_ty);
                    builder.vardata(item_option_data_ref.clone(), next_item_option_ref, item_option_ty.clone(), OPTION_SOME_CASE);
                    builder.mov(binding_ref.clone(), item_option_data_ref.to_deref());

                    let body_instructions = builder.loop_body_scope(continue_label, break_label, |builder| {
                        translate_stmt(&body, builder);
                    });

                    if jmp_exists(body_instructions, continue_label) {
                        builder.label(continue_label);
                    }

                    // return to top of loop
                    builder.jmp(top_label);
                });

                if jmp_exists(&loop_instructions, break_label) {
                    builder.label(break_label);
                }
            }
        };
    });
}

fn build_array_sequence_loop<ElementFn>(
    counter_ref: ir::Ref,
    high_val: impl Into<ir::Value>,
    binding_ref: ir::Ref,
    binding_ty: ir::Type,
    body: &typ::ast::Stmt,
    builder: &mut Builder,
    element_fn: ElementFn,
) 
    where ElementFn: FnOnce(&mut Builder) -> ir::Value
{
    build_for_loop_with_counter(
        counter_ref.clone(),
        ir::Value::LiteralI32(0),
        ir::Value::LiteralI32(1),
        high_val.into(),
        builder,
        |builder| {
            let skip_release_label = builder.alloc_label();
            builder.scope(|builder| {
                let first_iter = builder.eq_to_val(counter_ref.clone(), ir::Value::LiteralI32(0));
                builder.jmpif(skip_release_label, first_iter);

                builder.release(binding_ref.clone(), &binding_ty);    
            });
            
            builder.label(skip_release_label);
            
            builder.scope(|builder| {
                let element = element_fn(builder);
                builder.cast(binding_ref.clone(), element, binding_ty.clone());
                builder.retain(binding_ref, &binding_ty);

                translate_stmt(body, builder);    
            });
        }
    );
}

pub fn translate_while_loop(while_loop: &typ::ast::WhileLoop, builder: &mut Builder) {
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    let loop_instructions = builder.scope(|builder| {
        let not_cond = builder.local_temp(ir::Type::Bool);

        builder.comment("while-loop top");
        builder.label(top_label);

        // evaluate condition
        builder.scope(|builder| {
            let cond_val = expr_to_val(&while_loop.condition, builder);
            builder.not(not_cond.clone(), cond_val);
        });

        // break now if condition is false
        builder.jmpif(break_label, not_cond);

        // run loop body
        let body_instructions = builder.loop_body_scope(continue_label, break_label, |builder| {
            translate_stmt(&while_loop.body, builder);
        });

        if jmp_exists(body_instructions, continue_label) {
            builder.comment("while-loop continue");
            builder.label(continue_label);
        }

        // start next iteration
        builder.jmp(top_label);
    });

    if jmp_exists(loop_instructions, break_label) {
        builder.comment("while-loop break");
        builder.label(break_label);
    }
}

pub fn translate_assignment(assignment: &typ::ast::Assignment, builder: &mut Builder) {
    let lhs = translate_expr(&assignment.lhs, builder);
    let rhs = translate_expr(&assignment.rhs, builder);

    // the new value is being stored in a new location, retain it
    let rhs_ty = builder.translate_type(&assignment.rhs.annotation().ty());
    builder.retain(rhs.clone(), &rhs_ty);

    // the old value is being replaced, release it. local variables can be uninitialized,
    // or ambiguously initialized (initialized in one branch and not another), so we can't check
    // if the pointer is initialized here.
    //
    // if it's uninitialized it'll be NULL and we need to
    // handle that in the backend's release mechanism.
    //
    // the alternative would be to store an initialization flag alongside each rc variable
    let lhs_ty = builder.translate_type(&assignment.lhs.annotation().ty());
    builder.release(lhs.clone(), &lhs_ty);

    builder.append(ir::Instruction::Move {
        out: lhs,
        new_val: rhs.into(),
    });
}

pub fn translate_compound_assignment(
    assignment: &typ::ast::CompoundAssignment,
    builder: &mut Builder,
) {
    builder.scope(|builder| {
        let lhs = translate_expr(&assignment.lhs, builder);
        let rhs = expr_to_val(&assignment.rhs, builder);

        // result type must be the same as the lhs ty_def, that's where we're storing it
        let lhs_ty = builder.translate_type(&assignment.lhs.annotation().ty());

        let op_result = builder.local_temp(lhs_ty.clone());
        match assignment.op {
            ast::CompoundAssignmentOperator::AddAssign => {
                builder.add(op_result.clone(), lhs.clone(), rhs.clone())
            },
            ast::CompoundAssignmentOperator::SubAssign => {
                builder.sub(op_result.clone(), lhs.clone(), rhs.clone())
            }
            ast::CompoundAssignmentOperator::MulAssign => {
                builder.mul(op_result.clone(), lhs.clone(), rhs.clone())
            }
            ast::CompoundAssignmentOperator::FDivAssign => {
                builder.fdiv(op_result.clone(), lhs.clone(), rhs.clone())
            }
        };

        // the new value is being stored in a new location, release the old value and retain it
        builder.retain(op_result.clone(), &lhs_ty);
        builder.release(lhs.clone(), &lhs_ty);

        builder.append(ir::Instruction::Move {
            out: lhs,
            new_val: op_result.into(),
        });
    });
}

fn translate_case_stmt(case: &typ::ast::CaseStmt, builder: &mut Builder) {
    build_case_block(case, builder, |item, builder| translate_stmt(item, builder))
}

pub fn build_case_block<Item, ItemFn>(
    case: &typ::ast::CaseBlock<Item>,
    builder: &mut Builder,
    mut translate_item: ItemFn,
) where
    ItemFn: FnMut(&Item, &mut Builder),
{
    builder.scope(|builder| {
        let cond_expr_val = expr_to_val(&case.cond_expr, builder);

        let break_label = builder.alloc_label();

        let mut branch_labels = Vec::new();
        for _ in 0..case.branches.len() {
            branch_labels.push(builder.alloc_label());
        }

        let else_label = match &case.else_branch {
            Some(_) => Some(builder.alloc_label()),
            _ => None,
        };

        // jump to the branch stmt where the actual value is equal to any of the branch values
        for (branch, branch_label) in case.branches.iter().zip(branch_labels.iter()) {
            builder.scope(|builder| {
                for case_expr in &branch.case_values {
                    let branch_val = expr_to_val(case_expr, builder);
                    let matches_cond = builder.eq_to_val(branch_val, cond_expr_val.clone());

                    builder.jmpif(*branch_label, matches_cond);
                }
            });
        }

        if let Some(else_label) = else_label {
            builder.jmp(else_label);
        } else {
            builder.jmp(break_label);
        }

        // write the branch statements after their respective labels
        for (branch, branch_label) in case.branches.iter().zip(branch_labels.iter()) {
            builder.label(*branch_label);

            builder.scope(|builder| {
                translate_item(&branch.item, builder);
            });

            builder.jmp(break_label);
        }
        if let (Some(else_item), Some(else_label)) = (&case.else_branch, else_label) {
            builder.label(else_label);

            builder.scope(|builder| {
                translate_item(&else_item.item, builder);
            });

            builder.jmp(break_label);
        }

        builder.label(break_label);
    });
}

fn translate_match_stmt(match_stmt: &typ::ast::MatchStmt, builder: &mut Builder) {
    builder.scope(|builder| {
        let cond_expr = translate_expr(&match_stmt.cond_expr, builder);
        let cond_ty = builder.translate_type(&match_stmt.cond_expr.annotation().ty());

        let break_label = builder.alloc_label();

        let else_label = if match_stmt.else_branch.is_some() {
            Some(builder.alloc_label())
        } else {
            None
        };

        let is_skip = builder.local_temp(ir::Type::Bool);

        for branch in &match_stmt.branches {
            builder.scope(|builder| {
                // label to skip this branch if it isn't a match
                let skip_label = builder.alloc_label();

                let pattern_match = translate_pattern_match(
                    &branch.pattern,
                    branch.binding.as_ref(),
                    &cond_expr,
                    &cond_ty,
                    builder
                );

                // jump to skip label if pattern match return false
                builder.not(is_skip.clone(), pattern_match.is_match.clone());
                builder.jmpif(skip_label, is_skip.clone());

                // code to run if we didn't skip - the actual branch
                builder.scope(|builder| {
                    for binding in pattern_match.bindings {
                        binding.bind_local(builder);
                    }

                    translate_stmt(&branch.item, builder);
                });

                // only one branch must run so break out of the block now
                builder.jmp(break_label);

                builder.label(skip_label);
            });
        }

        // write the else branch - will fall through to here if we didn't run any branches
        if let Some(else_branch) = &match_stmt.else_branch {
            builder.scope(|builder| {
                builder.label(else_label.unwrap());

                translate_stmt(&else_branch.item, builder);
            });
        }

        builder.label(break_label);
    });
}
