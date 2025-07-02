use crate::ast;
use crate::result::ErrorContinue;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::evaluate_expr;
use crate::typ::ast::expr::expect_expr_initialized;
use crate::typ::ast::expr::expect_stmt_initialized;
use crate::typ::ast::typecheck_stmt;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypedValue;
use crate::typ::Value;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use crate::typ::completion::CompletionHintKind;

pub type Block = ast::Block<Value>;

pub fn typecheck_block(
    block: &ast::Block<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> Block {
    let block_env = Environment::Block {
        allow_unsafe: block.unsafe_kw.is_some(),
    };

    ctx.scope(block_env, |ctx| {
        let mut statements = Vec::new();

        let begin_end = block.begin_end.as_ref();
        let mut last_item_span = begin_end.map(|(begin, _end)| begin.clone());
        
        let mut output = None;

        let expect_output = *expect_ty != Type::Nothing;
        
        for (i, stmt) in block.stmts.iter().enumerate() {
            let stmt_span = stmt.span().clone();
            if let Some(last_item_span) = &last_item_span {
                ctx.hint_completion_range(last_item_span, &stmt_span, CompletionHintKind::Block);
            }
            last_item_span = Some(stmt_span);
            
            let is_last_stmt = i == block.stmts.len() - 1;

            if is_last_stmt && expect_output && block.output.is_none() {
                // this is the final stmt in the block, and during parsing this block didn't
                // get an output expr. we expect this block to have an output, so try to convert
                // the final stmt here into an expr
                if let Some(src_output) = stmt.to_expr() {
                    if let Some(expr) = evaluate_expr(&src_output, expect_ty, ctx)
                        .ok_or_continue(ctx)
                    {
                        if *expect_ty != Type::Nothing {
                            output = implicit_conversion(expr, expect_ty, ctx).ok_or_continue(ctx);
                        } else {
                            output = Some(expr);
                        }
                    }
                } else {
                    // typ the actual stmt which isn't a valid expr so we can use it
                    // for a better error message
                    if let Some(bad_stmt) =  typecheck_stmt(&stmt, expect_ty, ctx)
                        .ok_or_continue(ctx) 
                    {
                        ctx.error(TypeError::BlockOutputIsNotExpression {
                            stmt: Box::new(bad_stmt),
                            expected_expr_ty: expect_ty.clone(),
                        });
                    }
                }

                continue;
            }

            // even if we *weren't* expecting output, the block might still output a value,
            // and that value might be an expression that's syntactically valid as a statement,
            // but not semantically. in that case, we'll get InvalidStatement, and can set the
            // output now
            let allow_output_expr = block.output.is_none() && output.is_none() && is_last_stmt;

            match typecheck_stmt(stmt, &Type::Nothing, ctx) {
                Ok(stmt) => {
                    expect_stmt_initialized(&stmt, ctx).or_continue(ctx, ());
                    statements.push(stmt);
                },

                Err(TypeError::InvalidStatement(invalid)) if allow_output_expr => {
                    let expr = *invalid.0;
                    output = Some(expr);
                },

                Err(err) => { 
                    ctx.error(err);
                }
            }
        }

        // if the parser already identified the output expression, process that now
        // (this is mutually exclusive with converting the final stmt into an output)
        // the block's body statements can alter the context by declaring vars, initializing decls,
        // etc, so this has to be checked *after* we've processed the rest of the statements
        if let Some(src_output_expr) = &block.output {
            // we should not have tried to interpret any statements as output expressions
            assert_eq!(None, output);

            let expr_span = src_output_expr.span().clone();
            if let Some(last_item_span) = &last_item_span {
                ctx.hint_completion_range(last_item_span, &expr_span, CompletionHintKind::Block);
            }
            last_item_span = Some(expr_span);

            if let Some(out_expr) = evaluate_expr(src_output_expr, expect_ty, ctx)
                .ok_or_continue(ctx) 
            {
                if *expect_ty != Type::Nothing && !expect_ty.contains_unresolved_params(ctx) {
                    output = implicit_conversion(out_expr, expect_ty, ctx).ok_or_continue(ctx);
                } else {
                    out_expr.annotation().expect_any_value()
                        .or_continue(ctx, ());

                    output = Some(out_expr);
                }
            }
        }
        
        if let (Some((_, end_span)), Some(last_span)) = (begin_end, last_item_span) {
            ctx.hint_completion_range(&last_span, end_span, CompletionHintKind::Block);
        }

        if let Some(output_expr) = &output {
            expect_expr_initialized(output_expr, ctx).or_continue(ctx, ());
        }

        let span = block.annotation.span().clone();
        let annotation = match &output {
            Some(out_expr) => {
                if *out_expr.annotation().ty() == Type::Nothing {
                    Value::Untyped(span)
                } else {
                    let out_ty = out_expr.annotation().ty().into_owned();
                    Value::from(TypedValue::temp(out_ty, span))
                }
            },
            None => Value::Untyped(span),
        };

        let block = Block {
            annotation,
            output,
            stmts: statements,

            unsafe_kw: block.unsafe_kw.clone(),
            begin_end: block.begin_end.clone(),
        };

        assert_eq!(*block.annotation.ty(), {
            let out_ty = block.output.as_ref().map(|o| o.annotation().ty().into_owned());
            out_ty.unwrap_or(Type::Nothing)
        });

        block
    })
}
