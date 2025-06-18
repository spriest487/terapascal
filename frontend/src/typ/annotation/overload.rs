use std::sync::Arc;
use derivative::Derivative;
use terapascal_common::span::Span;
use crate::typ::annotation::function::FunctionValue;
use crate::ast;
use crate::ast::SemanticHint;
use crate::typ::ast::{resolve_overload, Expr, MethodDecl, OverloadCandidate};
use crate::typ::{Context, FunctionSig, Invocation, Type, TypeArgList, TypeName, TypeResult, Value};
use crate::typ::method::MethodValue;

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct OverloadValue {
    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,

    pub candidates: Vec<OverloadCandidate>,
    pub sig: Option<Arc<FunctionSig>>,

    pub self_arg: Option<Box<Expr>>,
}

impl OverloadValue {
    pub fn method(
        self_ty: Type,
        iface_ty: Type,
        index: usize,
        self_arg: Expr,
        method: MethodDecl,
        span: Span,
    ) -> Self {
        let sig = Arc::new(method.func_decl.sig());

        Self {
            span,
            self_arg: Some(Box::new(self_arg)),
            sig: Some(sig.clone()),
            candidates: vec![OverloadCandidate::Method {
                iface_ty,
                self_ty,
                index,
                decl: method.clone(),
            }],
        }
    }

    pub fn new(
        candidates: Vec<OverloadCandidate>,
        self_arg: Option<Box<Expr>>,
        span: Span,
    ) -> Self {
        let sig = if candidates.len() == 1 {
            Some(Arc::new(candidates[0].decl().sig()))
        } else {
            // undecided
            None
        };

        Self {
            candidates,
            sig,
            span,
            self_arg,
        }
    }

    pub fn func_ty(&self) -> Type {
        match &self.sig {
            Some(sig) => Type::Function(sig.clone()),
            None => Type::Nothing,
        }
    }

    pub fn create_invocation(
        &self,
        args: &[ast::Expr],
        args_span: Option<&Span>,
        type_args: Option<&TypeArgList>,
        span: &Span,
        ctx: &mut Context,
    ) -> TypeResult<Invocation> {
        let self_arg = self.self_arg.as_ref().map(Box::as_ref);
        let resolved = resolve_overload(&self.candidates, args, type_args, self_arg, span, ctx)?;

        match &self.candidates[resolved.selected_sig] {
            OverloadCandidate::Function {
                decl_name,
                decl,
                visibility,
            } => {
                let sig = resolved.func_sig(&Type::Nothing, &self.candidates);

                let func_val = FunctionValue {
                    name: decl_name.clone(),
                    visibility: *visibility,
                    decl: decl.clone(),
                    sig: Arc::new(sig),
                    span: self.span.clone(),
                };
                
                // eprintln!("overloaded invocation of `{}` with {} args (self arg? {})", decl, resolved.args.len(), self.self_arg.is_some());

                Ok(Invocation::Function {
                    function: Arc::new(func_val),
                    type_args: resolved.type_args,
                    span: span.clone(),
                    args: resolved.args,
                    args_span: args_span.cloned(),
                })
            },

            OverloadCandidate::Method {
                iface_ty,
                self_ty,
                index,
                decl,
            } => {
                let iface_ty = TypeName::inferred(iface_ty.clone());

                let sig = resolved.func_sig(self_ty, &self.candidates);
                let args = resolved.args;
                let type_args = resolved.type_args;
                
                assert_eq!(args.len(), sig.params.len());

                let method_val = MethodValue {
                    self_ty: iface_ty,
                    self_arg: self_arg.cloned().map(Box::new),
                    index: *index,
                    decl: decl.clone(),
                    span: self.span.clone(),
                    sig: Arc::new(sig),
                };

                Ok(Invocation::Method {
                    method: Arc::new(method_val),
                    self_ty: self_ty.clone(),
                    args,
                    args_span: args_span.cloned(),
                    span: span.clone(),
                    type_args,
                })
            },
        }
    }
    
    pub fn semantic_hint(&self) -> SemanticHint {
        if self.candidates.iter().all(|c| matches!(c, OverloadCandidate::Method {..})) {
            SemanticHint::Method
        } else {
            SemanticHint::Function
        }
    }
}

impl From<OverloadValue> for Value {
    fn from(a: OverloadValue) -> Self {
        Value::Overload(Arc::new(a))
    }
}
