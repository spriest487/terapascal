use crate::ast::IncompleteExpr;
use crate::typ::Context;
use crate::typ::Value;
use std::sync::Arc;
use derivative::Derivative;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct CompletionContext {
    pub span: Span,

    #[derivative(Debug = "ignore")]
    pub context: Arc<Context>,
}

#[derive(Clone, Debug)]
pub enum CompletionHint {
    Expr(IncompleteExpr<Value>),
    Range(CompletionContext, CompletionHintKind),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CompletionHintKind {
    Block,
    Type,
}

impl CompletionContext {
    pub fn new(span: Span, ctx: &mut Context) -> Self {
        Self {
            span,
            context: Arc::new(ctx.branch()),
        }
    }
}

impl Spanned for CompletionContext {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Spanned for CompletionHint {
    fn span(&self) -> &Span {
        match self {
            CompletionHint::Expr(expr) => &expr.context.span,
            CompletionHint::Range(context, _kind) => &context.span,
        }
    }
}

impl From<IncompleteExpr<Value>> for CompletionHint {
    fn from(expr: IncompleteExpr<Value>) -> Self {
        CompletionHint::Expr(expr)
    }
}
