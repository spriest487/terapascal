use crate::ast::type_name::TypeName;
use crate::ast::{Annotation, TypeAnnotation};
use crate::ast::BindingDeclKind;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::parse::MatchOneOf;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;
use derivative::*;
use std::fmt;

/// var or const binding (depending on the keyword)
#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct UnitBinding<A: Annotation> {
    pub kind: BindingDeclKind,

    pub items: Vec<UnitBindingItem<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for UnitBinding<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Parse for UnitBinding<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Keyword::Const.or(Keyword::Var))?;

        let kind = match kw_token.as_keyword() {
            Some(Keyword::Const) => BindingDeclKind::Const,
            Some(Keyword::Var) => BindingDeclKind::Var,
            _ => unreachable!(),
        };
        
        let mut items = Vec::new();
        
        loop {
            let mut idents = Vec::new();
            idents.push(Ident::parse(tokens)?);
            
            let mut idents_span = idents[0].span().clone();

            while tokens.match_one_maybe(Separator::Comma).is_some() {
                let next_ident = Ident::parse(tokens)?;
                
                idents_span = idents_span.to(next_ident.span());
                idents.push(next_ident);
            }

            let ty = match tokens.match_one_maybe(Separator::Colon) {
                Some(..) => TypeName::parse(tokens)?,
                None => TypeName::Unspecified(idents_span),
            };

            let val = if tokens.match_one_maybe(Operator::Equals).is_some() {
                let val = Expr::parse(tokens)?;
                Some(Box::new(val))
            } else {
                None
            };

            if let (Some(val_expr), 2..) = (&val, idents.len()) { 
                return Err(TracedError::trace(ParseError::MultiVarDeclHasInitExpr {
                    span: idents[0].span().to(val_expr.span()),
                }));
            }
            
            for ident in idents {
                items.push(UnitBindingItem {
                    ty: ty.clone(),
                    val: val.clone(),
                    span: ident.span.clone(),
                    ident,
                })
            }

            let match_more = Separator::Semicolon + Matcher::AnyIdent;
            if tokens.look_ahead().match_sequence(match_more).is_none() {
                break;
            }

            tokens.match_one(Separator::Semicolon)?;
        }

        let last_item = items.last().ok_or_else(|| {
            TracedError::trace(ParseError::EmptyConstOrVarDecl {
                span: kw_token.clone().into_span(),
            })
        })?;

        let span = kw_token.span().to(last_item.span());

        Ok(UnitBinding {
            kind,
            span,
            items,
        })
    }
}

impl<A: Annotation> fmt::Display for UnitBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "const")?;
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f, ";")?;
            }
            write!(f, "\t{}", item)?;
        }

        Ok(())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct UnitBindingItem<A: Annotation> {
    pub ident: Ident,
    pub ty: A::Type,

    pub val: Option<Box<Expr<A>>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for UnitBindingItem<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for UnitBindingItem<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if self.ty.is_known() {
            write!(f, ": {}", self.ty)?;
        }

        if let Some(val) = &self.val {
            write!(f, " = {}", val)?;
        }

        Ok(())
    }
}
