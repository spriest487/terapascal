mod assign;

pub use self::assign::Assignment;
pub use self::assign::CompoundAssignment;
use crate::ast;
use crate::ast::Ident;
use crate::ast::Operator;
use crate::parse::IllegalStatement;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::expr::typecheck_call;
use crate::typ::ast::expr::Invocation;
use crate::typ::ast::stmt::assign::typecheck_assignment;
use crate::typ::ast::stmt::assign::typecheck_compound_assignment;
use crate::typ::ast::typecheck_block;
use crate::typ::ast::typecheck_case_stmt;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_for_loop;
use crate::typ::ast::typecheck_if_cond_stmt;
use crate::typ::ast::typecheck_match_stmt;
use crate::typ::ast::typecheck_raise;
use crate::typ::ast::typecheck_while_loop;
use crate::typ::ast::Expr;
use crate::typ::typecheck_type;
use crate::typ::Binding;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type VarBinding = ast::LocalBinding<Value>;
pub type Stmt = ast::Stmt<Value>;
pub type Exit = ast::Exit<Value>;

pub fn typecheck_local_binding(
    binding: &ast::LocalBinding<Span>,
    ctx: &mut Context,
) -> TypeResult<VarBinding> {
    let (val, binding_ty) = match &binding.ty {
        ast::TypeName::Unspecified(_) => match &binding.val {
            None => {
                return Err(TypeError::UninitBindingWithNoType {
                    binding: Box::new(binding.clone()),
                });
            },

            Some(val) => {
                let val = typecheck_expr(val, &Type::Nothing, ctx)?;
                let val_ty = val.annotation().ty().into_owned();
                (Some(val), val_ty)
            },
        },

        val_ty => {
            let explicit_ty = typecheck_type(val_ty, ctx)?;
            if explicit_ty.is_unspecialized_generic() {
                return Err(TypeError::from_generic_err(
                    GenericError::IllegalUnspecialized { ty: explicit_ty },
                    binding.span().clone(),
                ));
            }

            let val = match &binding.val {
                Some(val) => {
                    let val = typecheck_expr(val, &explicit_ty, ctx)?;

                    let val = implicit_conversion(val, &explicit_ty, ctx)
                        .map_err(|err| match err {
                            TypeError::TypeMismatch {
                                expected,
                                actual,
                                span,
                                ..
                            } => TypeError::InvalidBinOp {
                                lhs: expected,
                                rhs: actual,
                                op: Operator::Assignment,
                                span,
                            },

                            err => err,
                        })?;

                    Some(val)
                },
                None => None,
            };

            (val, explicit_ty)
        },
    };

    if binding_ty == Type::Nothing {
        return Err(TypeError::BindingWithNoType {
            binding_name: binding.name.clone(),
            span: binding.span().clone(),
        });
    }

    if binding_ty.is_unspecialized_generic() {
        return Err(TypeError::from_generic_err(
            GenericError::IllegalUnspecialized { ty: binding_ty },
            binding.span().clone(),
        ));
    }

    let name = binding.name.clone();
    let span = binding.annotation.span().clone();

    let binding = Binding {
        kind: match &val {
            Some(..) => ValueKind::Mutable,
            None => ValueKind::Uninitialized,
        },
        ty: binding_ty.clone(),
        def: Some(name.clone()),
    };

    ctx.declare_local_var(name.clone(), binding)?;

    let annotation = Value::Untyped(span);

    let local_binding = VarBinding {
        name,
        ty: binding_ty,
        val,
        annotation,
    };

    Ok(local_binding)
}

pub fn typecheck_stmt(
    stmt: &ast::Stmt<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Stmt> {
    match stmt {
        ast::Stmt::Ident(ident, span) => {
            // a statement consisting of a single ident MUST be a call to a function without args
            typecheck_ident_stmt(ident, span, ctx)
        }

        ast::Stmt::LocalBinding(binding) => {
            typecheck_local_binding(binding, ctx).map(|s| ast::Stmt::LocalBinding(Box::new(s)))
        },

        ast::Stmt::Call(call) => match typecheck_call(call, expect_ty, ctx)? {
            Invocation::Call(call) => Ok(ast::Stmt::Call(call)),
            Invocation::Ctor(ctor) => {
                let ctor_expr = Expr::from(*ctor);
                let invalid_stmt = IllegalStatement::from(ctor_expr);
                Err(TypeError::InvalidStatement(invalid_stmt))
            },
        },

        ast::Stmt::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;

            Ok(ast::Stmt::Block(Box::new(block)))
        },

        ast::Stmt::ForLoop(for_loop) => typecheck_for_loop(for_loop, ctx)
            .map(Box::new)
            .map(ast::Stmt::ForLoop),

        ast::Stmt::WhileLoop(while_loop) => typecheck_while_loop(while_loop, ctx)
            .map(Box::new)
            .map(ast::Stmt::WhileLoop),

        ast::Stmt::Assignment(assignment) => typecheck_assignment(assignment, ctx)
            .map(Box::new)
            .map(ast::Stmt::Assignment),

        ast::Stmt::CompoundAssignment(assignment) => typecheck_compound_assignment(assignment, ctx)
            .map(Box::new)
            .map(ast::Stmt::CompoundAssignment),

        ast::Stmt::Exit(exit) => {
            let exit = typecheck_exit(exit, &expect_ty, ctx)?;
            Ok(ast::Stmt::Exit(Box::new(exit)))
        },

        ast::Stmt::Break(span) => {
            expect_in_loop(stmt, ctx)?;
            let annotation = Value::Untyped(span.clone());
            Ok(ast::Stmt::Break(annotation))
        },

        ast::Stmt::Continue(span) => {
            expect_in_loop(stmt, ctx)?;
            let annotation = Value::Untyped(span.clone());
            Ok(ast::Stmt::Continue(annotation))
        },

        ast::Stmt::If(if_cond) => {
            let if_cond = typecheck_if_cond_stmt(if_cond, expect_ty, ctx)?;
            Ok(ast::Stmt::If(Box::new(if_cond)))
        },

        ast::Stmt::Raise(raise) => {
            let raise = typecheck_raise(raise, expect_ty, ctx)?;
            Ok(ast::Stmt::Raise(Box::new(raise)))
        },

        ast::Stmt::Case(case) => {
            let case = typecheck_case_stmt(case, expect_ty, ctx)?;
            Ok(ast::Stmt::Case(Box::new(case)))
        },

        ast::Stmt::Match(match_stmt) => {
            let match_stmt = typecheck_match_stmt(match_stmt, expect_ty, ctx)?;
            Ok(ast::Stmt::Match(Box::new(match_stmt)))
        },
    }
}

fn typecheck_ident_stmt(ident: &Ident, span: &Span, ctx: &mut Context) -> TypeResult<Stmt> {
    let target = ast::Expr::Ident(ident.clone(), span.clone());

    // an expression which appears on its own as a statement MUST be a function we call call with
    // no args, so if the statement expression here typechecks to anything else (even another valid
    // stmt kind), raise an error.
    // we handle other kind of expr->stmt conversions during parsing, this is just the only
    // case where we need type information to decide
    match typecheck_expr(&target, &Type::Nothing, ctx)? {
        ast::Expr::Call(call) => {
            let call_stmt = Stmt::Call(call);
            Ok(call_stmt)
        },

        invalid => {
            // this would only be valid as an expression
            let invalid_stmt = IllegalStatement(Box::new(invalid));
            return Err(TypeError::InvalidStatement(invalid_stmt));
        }
    }
}

fn expect_in_loop(stmt: &ast::Stmt<Span>, ctx: &Context) -> TypeResult<()> {
    match ctx.in_loop() {
        Some(..) => Ok(()),

        None => Err(TypeError::NoLoopContext {
            stmt: Box::new(stmt.clone()),
        }),
    }
}

pub fn typecheck_exit(
    exit: &ast::Exit<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Exit> {
    // exit expressions always count as the expected type, so you can write e.g.
    // `let x := if true then 1 else exit;`
    // since no value will ever be assigned to `x` if the exit expr is reached
    let make_annotation = |span: &Span| match expect_ty {
        Type::Nothing => Value::Untyped(span.clone()),
        _ => TypedValue {
            span: span.clone(),
            value_kind: ValueKind::Temporary,
            ty: expect_ty.clone(),
            decl: None,
        }
        .into(),
    };

    let exit = match exit {
        ast::Exit::WithoutValue(span) => Exit::WithoutValue(make_annotation(span).into()),

        ast::Exit::WithValue(value, span) => {
            let mut ret_ty = ctx
                .current_func_return_ty()
                .ok_or_else(|| TypeError::NoFunctionContext {
                    stmt: Box::new(ast::Stmt::Exit(Box::new(exit.clone()))),
                })?
                .clone();

            let value = typecheck_expr(value, &ret_ty, ctx)?;
            let val_ty = value.annotation().ty();

            if ctx.set_inferred_result_ty(val_ty.as_ref()) {
                ret_ty = val_ty.into_owned();
            };

            if ret_ty == Type::Nothing {
                return Err(TypeError::InvalidExitWithValue {
                    span: exit.span().clone(),
                });
            }
            
            let value = implicit_conversion(value, &ret_ty, ctx)?;

            Exit::WithValue(value, make_annotation(span).into())
        },
    };

    Ok(exit)
}
