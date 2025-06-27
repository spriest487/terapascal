#[cfg(test)]
mod test;

use crate::ast::{Annotation, Ident, MatchPattern};
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::parse::{IllegalStatement, TryParse};
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ElseBranch<B> {
    pub item: Box<B>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub else_kw_span: Span,
}

impl<B> ElseBranch<B> {
    pub fn new(else_kw_span: Span, item: impl Into<Box<B>>) -> Self {
        Self {
            else_kw_span,
            item: item.into(),
        }
    } 
}

impl<B: Parse> ElseBranch<B> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let else_token = tokens.match_one(Keyword::Else)?;
        let branch = B::parse(tokens)?;

        Ok(ElseBranch {
            else_kw_span: else_token.into_span(),
            item: Box::new(branch),
        })
    }
    
    pub fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        match tokens.match_one_maybe(Keyword::Else) {
            Some(else_token) => {
                let branch = B::parse(tokens)?;

                Ok(Some(ElseBranch {
                    else_kw_span: else_token.into_span(),
                    item: Box::new(branch),
                }))
            },

            None => Ok(None),
        }
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct IsPatternMatch<A: Annotation = Span> {
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub is_kw: Span,
    
    pub pattern: MatchPattern<A>,

    pub binding: Option<Ident>,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct IfCond<B, A = Span>
where
    A: Annotation,
{
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub if_kw_span: Span,

    pub cond: Expr<A>,

    pub is_pattern: Option<IsPatternMatch<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub then_kw_span: Span,
    pub then_branch: B,

    pub else_branch: Option<ElseBranch<B>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

fn try_parse_is_pattern(tokens: &mut TokenStream) -> ParseResult<Option<IsPatternMatch>> {
    match tokens.match_one_maybe(Keyword::Is) {
        Some(tt) => {
            let pattern = MatchPattern::parse(tokens)?;
            
            let binding = Ident::try_parse(tokens)?;

            Ok(Some(IsPatternMatch {
                pattern,
                is_kw: tt.into_span(),
                binding,
            }))
        },

        None => Ok(None),
    }
}

impl IfCond<Expr, Span> {
    pub fn parse_expr(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expr::parse(tokens)?;

        let is_pattern = try_parse_is_pattern(tokens)?;

        let then_tt = tokens.match_one(Keyword::Then)?;
        let then_branch = Expr::parse(tokens)?;
        
        let (span, else_branch) = match ElseBranch::<Expr>::try_parse(tokens)? {
            Some(else_branch) => (if_token.span().to(else_branch.item.span()), Some(else_branch)),
            None => (if_token.span().to(then_branch.span()), None),
        };

        Ok(IfCond {
            if_kw_span: if_token.into_span(),
            cond,

            is_pattern,
            
            then_kw_span: then_tt.into_span(),
            then_branch,
            
            else_branch,
            
            annotation: span,
        })
    }
}

impl IfCond<Stmt, Span> {
    pub fn parse_stmt(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expr::parse(tokens)?;

        let is_pattern = try_parse_is_pattern(tokens)?;

        let then_tt = tokens.match_one(Keyword::Then)?;

        let then_branch = match Stmt::parse(tokens) {
            Ok(stmt) => stmt,
            
            Err(TracedError { err: ParseError::IsExpr(IllegalStatement(then_expr)), .. } ) => {
                // if the `then` branch is only valid as an expression, parse the else branch
                // as one too and return the whole thing as an invalid expression
                let (span, else_branch) = match ElseBranch::<Expr>::try_parse(tokens)? {
                    Some(else_branch) => (if_token.span().to(else_branch.item.span()), Some(else_branch)),
                    None => (if_token.span().to(then_expr.span()), None),
                };
                
                let invalid_expr = Expr::IfCond(Box::new(IfCond {
                    if_kw_span: if_token.into_span(),
                    cond,
                    is_pattern,
                    then_kw_span: then_tt.into_span(),
                    then_branch: *then_expr,
                    else_branch,
                    annotation: span,
                }));
                
                return Err(ParseError::is_expr(invalid_expr).into());
            },
            
            Err(other) => return Err(other),
        };

        // hack: we need this tt if parsing fails with an invalid stmt
        let else_tt = tokens.look_ahead().match_one(Keyword::Else).cloned();
        
        let (span, else_branch) = match ElseBranch::<Stmt>::try_parse(tokens) {
            Ok(Some(else_branch)) => {
                (if_token.span().to(else_branch.item.span()), Some(else_branch))
            }

            Ok(None) => {
                (if_token.span().to(then_branch.span()), None)
            },

            // if the `else` branch is only valid as an expression, then the `then` branch
            // should have been one too
            Err(TracedError { err: ParseError::IsExpr(else_expr), .. } ) => {
                match then_branch.to_expr() {
                    Some(then_expr) => {
                        let span = if_token.span().to(else_expr.0.span());

                        let if_expr = Expr::IfCond(Box::new(IfCond {
                            if_kw_span: if_token.into_span(),
                            cond,
                            is_pattern,
                            then_kw_span: then_tt.into_span(),
                            then_branch: then_expr,
                            else_branch: Some(ElseBranch {
                                item: else_expr.0,
                                else_kw_span: else_tt.unwrap().into_span(),
                            }),
                            annotation: span,
                        }));

                        return Err(ParseError::is_expr(if_expr).into());
                    }

                    None => {
                        return Err(ParseError::StatementIsIllegal(Box::new(then_branch)).into());
                    }
                }
            },
            
            Err(err) => return Err(err),
        };

        Ok(IfCond {
            if_kw_span: if_token.into_span(),
            cond,
            is_pattern,
            then_kw_span: then_tt.into_span(),
            then_branch,
            else_branch,
            annotation: span,
        })
    }
}

impl<B, A> fmt::Display for IfCond<B, A>
where
    A: Annotation,
    B: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} ", self.cond)?;

        if let Some(is_pattern) = &self.is_pattern {
            write!(f, "is {}", is_pattern.pattern)?;
            if let Some(binding) = &is_pattern.binding {
                write!(f, " {}", binding)?;
            }
        }

        write!(f, " then {}", self.then_branch)?;

        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch.item)
        } else {
            Ok(())
        }
    }
}

impl<B, A> Spanned for IfCond<B, A>
where
    A: Annotation,
    B: Parse,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl IfCond<Stmt> {
    pub fn to_expr(&self) -> Option<IfCond<Expr>> {
        let then_branch = self.then_branch.to_expr()?;
        let else_branch = match &self.else_branch {
            Some(else_branch) => Some(ElseBranch {
                else_kw_span: else_branch.else_kw_span.clone(),
                item: Box::new(else_branch.item.to_expr()?),
            }),
            None => None,
        };

        Some(IfCond {
            if_kw_span: self.if_kw_span.clone(),
            cond: self.cond.clone(),
            annotation: self.annotation.clone(),
            is_pattern: self.is_pattern.clone(),
            then_kw_span: self.then_kw_span.clone(),
            then_branch,
            else_branch,
        })
    }
}
