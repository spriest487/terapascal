use crate::ast::Visibility;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::{specialize_call_args, Expr};
use crate::typ::function::FunctionValue;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::InvocationValue;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeResult;
use derivative::Derivative;
use std::sync::Arc;
use terapascal_common::span::Span;

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct UfcsValue {
    pub function_name: Symbol,
    pub visibility: Visibility,

    pub self_arg: Box<Expr>,

    pub decl: Arc<FunctionDecl>,
    pub sig: Arc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl UfcsValue {
    pub fn new(
        function_name: Symbol,
        visibility: Visibility,
        self_arg: Expr,
        decl: Arc<FunctionDecl>,
        span: Span,
    ) -> Self {
        Self {
            self_arg: Box::new(self_arg),
            function_name,
            sig: Arc::new(decl.sig()),
            decl,
            visibility,
            span,
        }
    }

    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        let self_arg_ty = self.self_arg.annotation().ty();

        self.decl.sig().should_call_noargs_in_expr(expect_ty, self_arg_ty.as_ref())
    }

    pub fn create_zero_args_invocation(
        &self,
        expect_ty: &Type,
        span: &Span,
        ctx: &mut Context,
    ) -> TypeResult<InvocationValue> {
        let args = specialize_call_args(
            &self.decl,
            &[],
            Some(self.self_arg.as_ref()),
            None,
            &self.span,
            ctx
        )?;

        let func_value = FunctionValue {
            name: self.function_name.clone(),
            span: self.span.clone(),
            sig: Arc::new(args.sig.clone()),
            decl: self.decl.clone(),
            visibility: self.visibility.clone(),
        };

        func_value.create_invocation(&args.actual_args, None, args.type_args.as_ref(), expect_ty, span, ctx)
    }
}
