use crate::codegen::builder::Builder;
use crate::codegen::expr;
use crate::codegen::pattern::translate_pattern_match;
use crate::codegen::pattern::PatternMatchOutput;
use crate::codegen::stmt::build_case_block;
use crate::codegen::typ;
use crate::codegen::ir;

pub fn translate_if_cond<B, BranchTranslateFn>(
    if_cond: &typ::ast::IfCond<B>,
    builder: &mut Builder,
    branch_translate: BranchTranslateFn,
) -> Option<ir::Ref>
where
    BranchTranslateFn: Fn(&B, Option<&ir::Ref>, &ir::Type, &mut Builder),
{
    let (out_val, out_ty) = match if_cond.annotation.ty().as_ref() {
        typ::Type::Nothing => (None, ir::Type::Nothing),
        out_ty => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            (Some(out_val), out_ty)
        },
    };

    builder.scope(|builder| {
        let then_label = builder.alloc_label();
        let end_label = builder.alloc_label();
        let else_label = if_cond.else_branch.as_ref().map(|_| builder.alloc_label());

        let cond_ty = builder.translate_type(&if_cond.cond.annotation().ty());

        let pattern_match = match &if_cond.is_pattern {
            // match the cond val against the type pattern that follows it
            Some(is_pattern) => {
                let cond_ref = expr::translate_expr(&if_cond.cond, builder);
                translate_pattern_match(is_pattern, &cond_ref, &cond_ty, builder)
            }

            // no pattern, the cond val must be a boolean and we're just testing that
            None => {
                let cond_val = expr::expr_to_val(&if_cond.cond, builder);
                PatternMatchOutput {
                    is_match: cond_val,
                    bindings: Vec::new(),
                }
            }
        };

        builder.jmpif(then_label, pattern_match.is_match.clone());

        if let Some(else_label) = else_label {
            builder.jmp(else_label);
        } else {
            builder.jmp(end_label);
        }

        builder.label(then_label);

        builder.scope(|builder| {
            // bind pattern locals to names and retain them
            for pattern_binding in &pattern_match.bindings {
                pattern_binding.bind_local(builder);
            }

            branch_translate(&if_cond.then_branch, out_val.as_ref(), &out_ty, builder);
        });

        builder.jmp(end_label);

        if let Some(else_branch) = &if_cond.else_branch {
            builder.label(else_label.unwrap());

            builder.begin_scope();
            branch_translate(&else_branch.item, out_val.as_ref(), &out_ty, builder);
            builder.end_scope();
        }

        builder.label(end_label);
    });

    out_val
}

pub fn translate_case_expr(case: &typ::ast::CaseExpr, builder: &mut Builder) -> ir::Ref {
    let out_ty = builder.translate_type(&case.annotation.ty());
    let out_ref = builder.local_temp(out_ty);

    build_case_block(case, builder, |item, builder| {
        let branch_result = expr::translate_expr(item, builder);
        builder.mov(out_ref.clone(), branch_result);
    });

    out_ref
}

pub fn translate_match_expr(match_expr: &typ::ast::MatchExpr, builder: &mut Builder) -> ir::Ref {
    let out_ty = builder.translate_type(&match_expr.annotation.ty());
    let out_ref = builder.local_new(out_ty.clone(), None);

    builder.scope(|builder| {
        let cond_expr = expr::translate_expr(&match_expr.cond_expr, builder);
        let cond_ty = builder.translate_type(&match_expr.cond_expr.annotation().ty());

        let break_label = builder.alloc_label();

        let else_label = if match_expr.else_branch.is_some() {
            Some(builder.alloc_label())
        } else {
            None
        };

        let is_skip = builder.local_temp(ir::Type::Bool);

        for branch in &match_expr.branches {
            // label to skip this branch if it isn't a match
            let skip_label = builder.alloc_label();

            builder.scope(|builder| {
                let pattern_match =
                    translate_pattern_match(&branch.pattern, &cond_expr, &cond_ty, builder);

                // jump to skip label if pattern match return false
                builder.not(is_skip.clone(), pattern_match.is_match.clone());
                builder.jmpif(skip_label, is_skip.clone());

                builder.scope(|builder| {
                    // code to run if we didn't skip - the actual branch
                    for binding in pattern_match.bindings {
                        binding.bind_local(builder);
                    }

                    let branch_val = expr::expr_to_val(&branch.item, builder);

                    builder.mov(out_ref.clone(), branch_val);
                    builder.retain(out_ref.clone(), &out_ty);
                });

                // only one branch must run so break out of the block now
                builder.jmp(break_label);

                builder.label(skip_label);
            });
        }

        // write the else branch - will fall through to here if we didn't run any branches
        if let Some(else_branch) = &match_expr.else_branch {
            builder.scope(|builder| {
                builder.label(else_label.unwrap());

                let else_val = expr::expr_to_val(else_branch, builder);
                builder.mov(out_ref.clone(), else_val);
                builder.retain(out_ref.clone(), &out_ty);
            });

            builder.jmp(break_label);
        }

        // we MUST have executed a branch!
        let err = "unhandled pattern in match expr";
        let err_str = builder.find_or_insert_string(err);
        builder.append(ir::Instruction::Raise {
            val: ir::Ref::Global(ir::GlobalRef::StringLiteral(err_str)),
        });

        builder.label(break_label);
    });

    out_ref
}
