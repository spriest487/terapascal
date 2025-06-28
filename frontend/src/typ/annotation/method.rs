use crate::ast;
use crate::typ::ast::build_args_for_params;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::ast::MethodDecl;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::Invocation;
use crate::typ::NameError;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use derivative::Derivative;
use std::borrow::Cow;
use std::sync::Arc;
pub use terapascal_common::span::Span;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MethodValue {
    /// the type via which this method is being referred to. we don't distinguish here between
    /// an interface method (implemented on a type other than the self type) and a direct method
    /// call (known to be implemented on the self type used here)
    pub self_ty: Type,
    pub index: usize,

    /// None for class methods
    pub self_arg: Option<Box<Expr>>,

    // members below this point are just cached for convenience, all of these can be
    // fetched from the type by the index
    /// span of this reference to the method (not the method's own decl)
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub span: Span,

    /// original decl
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub decl: MethodDecl,

    /// sig of this usage, with any type args/self types applied
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub sig: Arc<FunctionSig>,
}

impl MethodValue {
    pub fn new(
        self_ty: Type,
        self_arg: Option<Expr>,
        index: usize,
        decl: MethodDecl,
        span: Span,
    ) -> Self {
        Self {
            self_ty,
            self_arg: self_arg.map(|arg| Box::new(arg)),
            index,
            span,
            sig: Arc::new(decl.func_decl.sig()),
            decl,
        }
    }

    pub fn func_ty(&self) -> Type {
        Type::Function(Arc::new(self.decl.func_decl.sig()))
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type, self_arg: &Type) -> bool {
        self.decl.func_decl.sig().should_call_noargs_in_expr(expect_ty, self_arg)
    }

    pub fn create_invocation(
        &self,
        rest_args: &[ast::Expr],
        args_span: Option<&Span>,
        span: &Span,
        ctx: &mut Context,
    ) -> TypeResult<Invocation> {
        if self.self_ty.get_current_access(ctx) < self.decl.access {
            return Err(TypeError::TypeMemberInaccessible {
                ty: self.self_ty.clone(),
                access: self.decl.access,
                member: self.decl.func_decl.ident().clone(),
                span: span.clone(),
            });
        }

        // branch the context to check the self-arg, because we're about to re-check it in a second
        let self_type = match &self.self_arg {
            Some(self_arg) => self_arg.annotation().ty(),

            None => {
                ctx.with_temp_branch(|temp_ctx| {
                    let first_self_pos = self
                        .decl
                        .func_decl
                        .params()
                        .position(|(group, _param)| group.ty == Type::MethodSelf);

                    let self_ty: Cow<Type> = match first_self_pos {
                        Some(index) => {
                            // note that this isn't the "self arg" used for typecheck_args, because we're not passing
                            // it implicitly as a separate arg (like the self-arg `x` of `x.Y()` in a UFCS call).
                            // it's just the first arg from which we can infer the self-type
                            let first_self_arg =
                                typecheck_expr(&rest_args[index], &Type::Nothing, temp_ctx)?;

                            Cow::Owned(first_self_arg.annotation().ty().into_owned())
                        },

                        None => Cow::Owned(Type::Nothing),
                    };
                    
                    Ok(self_ty)
                })?
            },
        };

        let sig = self.decl.func_decl.sig().with_self(&self_type);

        let typechecked_args = build_args_for_params(
            &sig.params,
            &rest_args,
            self.self_arg.as_ref().map(Box::as_ref),
            span,
            ctx,
        )?;

        if self.self_ty.as_iface().is_some() {
            let is_impl = ctx
                .is_implementation(self_type.as_ref(), &self.self_ty)
                .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

            if !is_impl {
                return Err(TypeError::from_name_err(
                    NameError::NoImplementationFound {
                        owning_ty: self.self_ty.clone(),
                        impl_ty: self_type.into_owned(),
                    },
                    span.clone(),
                ));
            }
        } else if let Some(..) = &self.self_arg {
            if self.self_ty != *self_type {
                return Err(TypeError::type_mismatch(
                    self.self_ty.clone(),
                    self_type.into_owned(),
                    span.clone(),
                ));
            }
        }

        // eprintln!("{func_call}: method call with {self_type}");

        let invocation = Invocation::Method {
            self_ty: self_type.into_owned(),
            method: Arc::new(self.clone()),
            type_args: None,
            args: typechecked_args,
            args_span: args_span.cloned(),
            span: span.clone(),
        };

        Ok(invocation)
    }
}

impl From<MethodValue> for Value {
    fn from(a: MethodValue) -> Self {
        Value::Method(Arc::new(a))
    }
}
