#[cfg(test)]
mod test;

use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::TypeNamePattern;
use crate::parse::InvalidStatement;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use common::span::Span;
use common::span::Spanned;
use common::TracedError;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct IfCond<A, B>
where
    A: Annotation,
{
    pub cond: Expr<A>,

    pub is_pattern: Option<A::Pattern>,

    pub then_branch: B,
    pub else_branch: Option<B>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl IfCond<Span, Expr> {
    pub fn parse_expr(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expr::parse(tokens)?;

        let is_pattern = match tokens.match_one_maybe(Keyword::Is) {
            Some(_is_kw) => {
                let pattern = TypeNamePattern::parse(tokens)?;
                Some(pattern)
            },

            None => None,
        };

        tokens.match_one(Keyword::Then)?;
        let then_branch = Expr::parse(tokens)?;
        let (else_branch, span) = Self::parse_else_branch(tokens, if_token.span(), &then_branch)?;

        Ok(IfCond {
            cond,
            is_pattern,
            then_branch,
            else_branch,
            annotation: span,
        })
    }
    
    fn parse_else_branch(
        tokens: &mut TokenStream,
        kw_span: &Span,
        then_branch: &Expr
    ) -> ParseResult<(Option<Expr>, Span)> {
        let (else_branch, span) = match tokens.match_one_maybe(Keyword::Else) {
            Some(_else_token) => {
                let else_branch = Expr::parse(tokens)?;
                let span = kw_span.to(else_branch.span());

                (Some(else_branch), span)
            },

            None => (None, kw_span.to(then_branch.span())),
        };
        
        Ok((else_branch, span))
    }
}

impl IfCond<Span, Stmt> {
    pub fn parse_stmt(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expr::parse(tokens)?;

        let is_pattern = match tokens.match_one_maybe(Keyword::Is) {
            Some(_is_kw) => {
                let pattern = TypeNamePattern::parse(tokens)?;
                Some(pattern)
            },

            None => None,
        };

        tokens.match_one(Keyword::Then)?;

        let then_branch = match Stmt::parse(tokens) {
            Ok(stmt) => stmt,
            
            Err(TracedError { err: ParseError::InvalidStatement(InvalidStatement(then_expr)), .. } ) => {
                // if the `then` branch is only valid as an expression, parse the else branch
                // as one too and return the whole thing as an invalid expression
                let (else_expr, span) = IfCond::parse_else_branch(tokens, &if_token.span(), &then_expr)?;
                let invalid_expr = Expr::IfCond(Box::new(IfCond {
                    cond,
                    is_pattern,
                    then_branch: *then_expr,
                    else_branch: else_expr,
                    annotation: span,
                }));
                
                return Err(TracedError::trace(ParseError::InvalidStatement(InvalidStatement(
                    Box::new(invalid_expr)
                ))));
            },
            
            Err(other) => return Err(other),
        };

        let (else_branch, span) = match tokens.match_one_maybe(Keyword::Else) {
            Some(_else_token) => {
                let else_branch = match Stmt::parse(tokens) {
                    Ok(stmt) => stmt,

                    // if the `else` branch is only valid as an expression, then the `then` branch
                    // should have been one too
                    Err(TracedError { err: ParseError::InvalidStatement(InvalidStatement(..)), .. } ) => {
                        return Err(TracedError::trace(ParseError::IsStatement(Box::new(then_branch))));
                    },

                    Err(other) => return Err(other),
                };

                let span = if_token.span().to(else_branch.span());

                (Some(else_branch), span)
            },

            None => {
                let span = if_token.span().to(then_branch.span());
                (None, span)
            },
        };

        Ok(IfCond {
            cond,
            is_pattern,
            then_branch,
            else_branch,
            annotation: span,
        })
    }
}

impl<A, B> fmt::Display for IfCond<A, B>
where
    A: Annotation,
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} ", self.cond)?;

        if let Some(is_pattern) = &self.is_pattern {
            write!(f, "is {}", is_pattern)?;
        }

        write!(f, " then {}", self.then_branch)?;

        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)
        } else {
            Ok(())
        }
    }
}

impl<A, B> Spanned for IfCond<A, B>
where
    A: Annotation,
    B: Parse,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl IfCond<Span, Stmt<Span>> {
    pub fn to_expr(&self) -> Option<IfCond<Span, Expr<Span>>> {
        let then_branch = self.then_branch.to_expr()?;
        let else_branch = match &self.else_branch {
            Some(else_stmt) => Some(else_stmt.to_expr()?),
            None => None,
        };

        Some(IfCond {
            cond: self.cond.clone(),
            annotation: self.annotation.clone(),
            is_pattern: self.is_pattern.clone(),
            then_branch,
            else_branch,
        })
    }
}
