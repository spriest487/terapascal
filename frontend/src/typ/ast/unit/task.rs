use crate::ast;
use crate::ast::FunctionName;
use crate::result::ErrorContinue;
use crate::typ::Context;
use crate::typ::TypeResult;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDef;
use crate::typ::ast::UnitDecl;
use crate::typ::ast::typecheck_func_def;
use rayon::prelude::*;
use std::sync::Arc;

pub enum UnitDeclTask {
    Done(UnitDecl),
    DeferredFuncDef {
        src_def: ast::FunctionDef,
        func_decl: FunctionDecl,
        body_ctx: Context,
    },
}

enum UnitDeclOutput {
    Done(UnitDecl),
    DeferredFuncDef {
        def_result: TypeResult<FunctionDef>,
        body_ctx: Context,
    },
}

impl UnitDeclTask {
    fn complete(self) -> UnitDeclOutput {
        match self {
            UnitDeclTask::Done(item) => UnitDeclOutput::Done(item),

            UnitDeclTask::DeferredFuncDef {
                func_decl,
                src_def,
                mut body_ctx,
            } => {
                let def_result = typecheck_func_def(func_decl.clone(), &src_def, &mut body_ctx);

                UnitDeclOutput::DeferredFuncDef {
                    def_result,
                    body_ctx,
                }
            },
        }
    }

    pub fn run_parallel(tasks: Vec<UnitDeclTask>, ctx: &mut Context) -> Vec<UnitDecl> {
        let tasks: Vec<_> = tasks.into_par_iter().map(|task| task.complete()).collect();

        let mut decls = Vec::with_capacity(tasks.len());
        for complete_task in tasks {
            if let Some(decl) = complete_task.finish(ctx).ok_or_continue(ctx) {
                decls.push(decl);
            }
        }

        decls
    }
}

impl UnitDeclOutput {
    fn finish(self, ctx: &mut Context) -> TypeResult<UnitDecl> {
        match self {
            UnitDeclOutput::Done(item) => Ok(item),

            UnitDeclOutput::DeferredFuncDef {
                def_result,
                mut body_ctx,
            } => {
                let func_def = Arc::new(def_result?);

                match func_def.decl.method_declaring_type() {
                    Some(ty) => {
                        ctx.define_method(ty.clone(), func_def.clone())?;
                    },

                    None => {
                        let ident = func_def.decl.name.ident().clone();
                        ctx.define_function(ident, func_def.clone())?;
                    },
                }

                ctx.end_branch(&mut body_ctx);

                Ok(UnitDecl::FunctionDef { def: func_def })
            },
        }
    }
}
