use crate::ast::Visibility;
use crate::typ::annotation::invoke::InvocationValue;
use crate::typ::ast::specialize_func_decl;
use crate::typ::ast::validate_args;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::infer_type_args;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use derivative::Derivative;
use std::borrow::Cow;
use std::sync::Arc;
use terapascal_common::span::Span;

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct FunctionValue {
    pub name: Symbol,
    pub visibility: Visibility,

    /// original decl
    pub decl: Arc<FunctionDecl>,

    /// sig of this reference ot the function, with any type args applied
    pub sig: Arc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl FunctionValue {
    pub fn new(
        name: Symbol,
        visibility: Visibility,
        decl: impl Into<Arc<FunctionDecl>>,
        sig: impl Into<Arc<FunctionSig>>,
        span: Span
    ) -> Self {
        Self {
            name,
            visibility,
            sig: sig.into(),
            decl: decl.into(),
            span,
        }
    }

    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        self.sig.should_call_noargs_in_expr(expect_ty, &Type::Nothing)
    }

    pub fn check_visible(&self, at: &Span, ctx: &Context) -> TypeResult<()> {
        if self.visibility < Visibility::Interface
            && !ctx.is_current_namespace_child(&self.name.full_path)
        {
            return Err(TypeError::NameNotVisible {
                name: self.name.full_path.clone(),
                span: at.clone(),
            });
        }

        Ok(())
    }

    pub fn create_invocation(
        &self,
        args: &[Expr],
        args_span: Option<&Span>,
        type_args: Option<&TypeArgList>,
        expect_ty: &Type,
        span: &Span,
        ctx: &mut Context,
    ) -> TypeResult<InvocationValue> {
        self.check_visible(span, ctx)?;
        
        // eprintln!("creating invocation of {}", self.name);
        // if let Some(args) = type_args {
        //     eprintln!("\t type args: {}", args);
        // }
        
        let type_args = match (&self.decl.name.type_params, type_args) {
            (Some(params), None) => {
                let args = infer_type_args(&self.sig.result_ty, expect_ty, params, &self.span, ctx)
                    .ok_or_else(|| {
                        let err = GenericError::CannotInferArgs {
                            target: GenericTarget::FunctionSig((*self.sig).clone()),
                            hint: GenericTypeHint::ExpectedReturnType(expect_ty.clone()),
                        };
                        TypeError::from_generic_err(err, self.span.clone())
                    })?;

                Some(Cow::Owned(args))
            },

            (Some(..), Some(args)) => Some(Cow::Borrowed(args)),

            (None, Some(args)) => {
                let err = GenericError::ArgsLenMismatch {
                    target: GenericTarget::FunctionSig((*self.sig).clone()),
                    expected: 0,
                    actual: args.items.len(),
                };
                return Err(TypeError::from_generic_err(err, self.span.clone()));
            },

            (None, None) => None,
        };

        let mut args = args.to_vec();
        validate_args(&mut args, &self.sig.params, &span, ctx)?;

        let func_val = if let Some(ty_args) = type_args.as_ref() {
            let call_name = self
                .name
                .specialize(ty_args, ctx)
                .map_err(|e| TypeError::from_generic_err(e, span.clone()))?
                .into_owned();

            // let spec_decl = specialize_func_decl(&self.decl, ty_args, ctx)
            //     .map_err(|err| TypeError::from_generic_err(err, span.clone()))?;
            let sig = specialize_func_decl(&self.decl, ty_args, ctx)
                .map_err(|err| TypeError::from_generic_err(err, span.clone()))?
                .sig();

            let specialized_func_type = FunctionValue::new(
                call_name,
                self.visibility,
                self.decl.clone(),
                sig,
                self.span.clone(),
            );

            Arc::new(specialized_func_type)
        } else {
            Arc::new(self.clone())
        };

        // eprintln!("{func_call} -> {}", func_val.sig);

        Ok(InvocationValue::Function {
            args,
            args_span: args_span.cloned(),
            type_args: type_args.map(Cow::into_owned),
            function: func_val,
            span: span.clone(),
        })
    }
}

impl From<FunctionValue> for Value {
    fn from(a: FunctionValue) -> Self {
        Value::Function(Arc::new(a))
    }
}
