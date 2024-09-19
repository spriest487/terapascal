use crate::emit::builder::jmp_exists;
use crate::emit::expr::build_call;
use crate::emit::expr::expr_to_val;
use crate::emit::expr::translate_raise;
use crate::emit::ir;
use crate::emit::pattern::translate_pattern_match;
use crate::emit::syn;
use crate::emit::translate_block;
use crate::emit::translate_exit;
use crate::emit::translate_expr;
use crate::emit::translate_if_cond_stmt;
use crate::emit::typ;
use crate::emit::Builder;
use common::span::Spanned;
use std::borrow::Cow;

pub fn translate_stmt(stmt: &typ::ast::Stmt, builder: &mut Builder) {
    builder.push_debug_context(stmt.annotation().span().clone());
    builder.comment(stmt);
    
    match stmt {
        syn::Stmt::Ident(..) => {
            unreachable!("should be turned into no-args calls during typechecking")
        }

        syn::Stmt::LocalBinding(binding) => {
            build_binding(binding, builder);
        },

        syn::Stmt::Call(call) => {
            build_call(call, builder);
        },

        syn::Stmt::Block(block) => {
            translate_block(block, ir::Ref::Discard, builder);
        },

        syn::Stmt::Exit(exit) => {
            translate_exit(exit, builder);
        },

        syn::Stmt::ForLoop(for_loop) => {
            build_for_loop(for_loop, builder);
        },

        syn::Stmt::WhileLoop(while_loop) => {
            translate_while_loop(while_loop, builder);
        },

        syn::Stmt::Assignment(assignment) => {
            translate_assignment(assignment, builder);
        },

        syn::Stmt::CompoundAssignment(assignment) => {
            translate_compound_assignment(assignment, builder);
        },

        syn::Stmt::If(if_stmt) => {
            translate_if_cond_stmt(if_stmt, builder);
        },

        syn::Stmt::Raise(raise) => {
            translate_raise(raise, builder);
        },

        syn::Stmt::Break(_) => {
            builder.break_loop();
        },

        syn::Stmt::Continue(_) => {
            builder.continue_loop();
        },

        syn::Stmt::Case(case) => {
            translate_case_stmt(case, builder);
        },

        syn::Stmt::Match(match_stmt) => {
            translate_match_stmt(match_stmt, builder);
        },
    }

    builder.pop_debug_context()
}

fn build_binding(binding: &typ::ast::VarBinding, builder: &mut Builder) {
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
    let top_label = builder.alloc_label();
    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    let counter_ty = match &for_loop.init {
        syn::ForLoopInit::Assignment { counter, .. } => counter.annotation().ty(),
        syn::ForLoopInit::Binding(binding) => Cow::Borrowed(&binding.ty),
    };

    let loop_instructions = builder.scope(|builder| {
        // counter
        let counter_ty = builder.translate_type(&counter_ty);

        let (counter_val, counter_init_val) = match &for_loop.init {
            syn::ForLoopInit::Assignment { counter, value } => {
                let counter_ref = translate_expr(counter, builder);
                let init_val = expr_to_val(&value, builder);

                (counter_ref, init_val)
            }

            syn::ForLoopInit::Binding(counter_binding) => {
                let counter_init_val = counter_binding
                    .val
                    .as_ref()
                    .expect("for loop counter binding must have an init expr");

                let counter_binding_name = counter_binding.name.to_string();
                let counter_val = builder.local_new(counter_ty.clone(), Some(counter_binding_name));
                let init_val = expr_to_val(&counter_init_val, builder);

                (counter_val, init_val)
            }
        };

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

        // temp value to store the result of evaluating the break condition
        let break_cond_val = builder.local_temp(ir::Type::Bool);
        let to_val = expr_to_val(&for_loop.to_expr, builder);

        builder.mov(counter_val.clone(), counter_init_val);

        builder.label(top_label);

        // break check: break_cond_val := counter_val > to_val
        builder.gt(break_cond_val.clone(), counter_val.clone(), to_val);

        // jump to break label if loop_val
        builder.jmp_if(break_label, break_cond_val);

        let body_instructions = builder.loop_body_scope(continue_label, break_label, |builder| {
            translate_stmt(&for_loop.body, builder);
        });

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
        builder.jmp_if(break_label, not_cond);

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
            syn::CompoundAssignmentOperator::AddAssign => {
                builder.add(op_result.clone(), lhs.clone(), rhs.clone())
            },
            syn::CompoundAssignmentOperator::SubAssign => {
                builder.sub(op_result.clone(), lhs.clone(), rhs.clone())
            }
            syn::CompoundAssignmentOperator::MulAssign => {
                builder.mul(op_result.clone(), lhs.clone(), rhs.clone())
            }
            syn::CompoundAssignmentOperator::FDivAssign => {
                builder.idiv(op_result.clone(), lhs.clone(), rhs.clone())
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

        // jump to the branch stmt where the actual value is equal to the branch value
        for (branch, branch_label) in case.branches.iter().zip(branch_labels.iter()) {
            builder.scope(|builder| {
                let branch_val = expr_to_val(&branch.value, builder);

                let cond_eq = builder.local_temp(ir::Type::Bool);

                builder.eq(cond_eq.clone(), branch_val, cond_expr_val.clone());
                builder.jmp_if(*branch_label, cond_eq);
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
                translate_item(&else_item, builder);
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

                let pattern_match =
                    translate_pattern_match(&branch.pattern, &cond_expr, &cond_ty, builder);

                // jump to skip label if pattern match return false
                builder.not(is_skip.clone(), pattern_match.is_match.clone());
                builder.jmp_if(skip_label, is_skip.clone());

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

                translate_stmt(else_branch, builder);
            });
        }

        builder.label(break_label);
    });
}
