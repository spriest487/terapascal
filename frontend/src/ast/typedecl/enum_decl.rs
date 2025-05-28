use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::ast::TypeDeclName;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::DelimiterPair;
use crate::Operator;
use crate::Separator;
use derivative::*;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct EnumDecl<A: Annotation> {
    pub name: A::Name,

    pub items: Vec<EnumDeclItem<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for EnumDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for EnumDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;

        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }

        write!(f, ")")
    }
}

impl EnumDecl<Span> {
    pub fn parse(name: TypeDeclName, tokens: &mut TokenStream) -> ParseResult<Self> {
        let group = tokens
            .match_one(DelimiterPair::Bracket)?
            .into_delimited_group()
            .unwrap();
        let mut items_tokens = group.to_inner_tokens();

        let first_item = EnumDeclItem::parse(&mut items_tokens)?;

        let mut span = first_item.span().clone();
        let mut items = vec![first_item];
        while items_tokens.match_one_maybe(Separator::Comma).is_some() {
            let item = EnumDeclItem::parse(&mut items_tokens)?;
            span = span.to(item.span());

            items.push(item);
        }

        let enum_decl = EnumDecl { name, items, span };

        Ok(enum_decl)
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct EnumDeclItem<A: Annotation> {
    pub ident: Ident,
    pub value: Option<A::ConstIntegerExpr>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> Spanned for EnumDeclItem<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for EnumDeclItem<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(val_expr) = &self.value {
            write!(f, " = {}", val_expr)?;
        }
        Ok(())
    }
}

impl Parse for EnumDeclItem<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let ident = Ident::parse(tokens)?;
        let mut span = ident.span().clone();

        let value = if tokens.match_one_maybe(Operator::Equals).is_some() {
            let val_expr = Expr::parse(tokens)?;
            span = span.to(val_expr.span());

            Some(Box::new(val_expr))
        } else {
            None
        };

        let item = EnumDeclItem {
            ident,
            value,
            annotation: span,
        };
        Ok(item)
    }
}
