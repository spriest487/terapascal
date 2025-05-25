use std::fmt;
use crate::{
    ast::{Annotation, Expr},
    parse::{ParseResult, TokenStream},
    Keyword,
};
use terapascal_common::span::{Span, Spanned};
use crate::parse::Matcher;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exit<A: Annotation> {
    WithValue(Expr<A>, A),
    WithoutValue(A),
}

impl Exit<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let exit_tt = tokens.match_one(Keyword::Exit)?;

        let exit = match tokens.look_ahead().match_one(Matcher::ExprOperandStart) {
            None => Exit::WithoutValue(exit_tt.span().clone()),

            Some(..) => {
                let value_expr = Expr::parse(tokens)?;
                let span = exit_tt.span().to(value_expr.annotation().span());
                Exit::WithValue(value_expr, span)
            }
        };

        Ok(exit)
    }
}

impl<A: Annotation> Exit<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Exit::WithValue(_, a) => a,
            Exit::WithoutValue(a) => a,
        }
    }

    pub fn annotation_mut(&mut self) -> &mut A {
        match self {
            Exit::WithValue(_, a) => a,
            Exit::WithoutValue(a) => a,
        }
    }
}

impl<A: Annotation> fmt::Display for Exit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exit::WithoutValue(_) => write!(f, "exit"),
            Exit::WithValue(value, _) => write!(f, "exit {}", value),
        }
    }
}

impl<A: Annotation> Spanned for Exit<A> {
    fn span(&self) -> &Span {
        match self {
            Exit::WithValue(_, a) => a.span(),
            Exit::WithoutValue(a) => a.span(),
        }
    }
}
