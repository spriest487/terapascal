use crate::ast::Annotation;
use crate::ast::Expr;
use crate::parse::Matcher;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub enum Exit<A: Annotation> {
    WithValue {
        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        exit_kw: Span,
        value_expr: Expr<A>,

        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        annotation: A, 
    },

    WithoutValue(
        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        A
    ),
}

impl Exit<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let exit_tt = tokens.match_one(Keyword::Exit)?;

        let exit = match tokens.look_ahead().match_one(Matcher::ExprOperandStart) {
            None => Exit::WithoutValue(exit_tt.span().clone().into()),

            Some(..) => {
                let value_expr = Expr::parse(tokens)?;
                let span = exit_tt.span().to(value_expr.annotation().span());

                Exit::WithValue {
                    value_expr,
                    exit_kw: exit_tt.into_span().into(),
                    annotation: span.into(),
                }
            }
        };

        Ok(exit)
    }
}

impl<A: Annotation> Exit<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Exit::WithValue { annotation, .. } => annotation,
            Exit::WithoutValue(a) => a,
        }
    }

    pub fn annotation_mut(&mut self) -> &mut A {
        match self {
            Exit::WithValue { annotation, .. } => annotation,
            Exit::WithoutValue(a) => a,
        }
    }
}

impl<A: Annotation> fmt::Display for Exit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exit::WithoutValue(_) => write!(f, "exit"),
            Exit::WithValue { value_expr, .. } => write!(f, "exit {}", value_expr),
        }
    }
}

impl<A: Annotation> Spanned for Exit<A> {
    fn span(&self) -> &Span {
        match self {
            Exit::WithValue { annotation, .. } => annotation.span(),
            Exit::WithoutValue(a) => a.span(),
        }
    }
}
