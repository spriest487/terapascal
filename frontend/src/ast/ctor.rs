#[cfg(test)]
mod test;

use crate::ast::Expr;
use crate::ast::Ident;
use crate::ast::{Annotation, TypeArgList};
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::DelimiterPair;
use crate::Separator;
use crate::TokenTree;
use common::span::Span;
use common::span::Spanned;
use derivative::*;
use std::fmt;

#[derive(Eq, Clone, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ObjectCtorMember<A: Annotation = Span> {
    pub ident: Ident,
    pub value: Expr<A>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> fmt::Display for ObjectCtorMember<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.ident, self.value)
    }
}

impl<A: Annotation> Spanned for ObjectCtorMember<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl ParseSeq for ObjectCtorMember<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;
        tokens.match_one(Separator::Colon)?;

        let value = Expr::parse(tokens)?;

        Ok(ObjectCtorMember {
            span: ident.span().to(value.span()),
            ident,
            value,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ObjectCtorArgs<A: Annotation = Span> {
    pub members: Vec<ObjectCtorMember<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl ObjectCtorArgs<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let args_group = match tokens.match_one(DelimiterPair::Bracket)? {
            TokenTree::Delimited(group) => group,
            _ => unreachable!(),
        };

        let open = args_group.open.clone();
        let close = args_group.close.clone();

        let mut members_tokens = args_group.to_inner_tokens();
        let members = ObjectCtorMember::parse_seq(&mut members_tokens)?;

        if members.len() > 0 {
            members_tokens.match_one_maybe(Separator::Semicolon);
        }

        members_tokens.finish()?;

        Ok(ObjectCtorArgs {
            members,
            span: open.to(&close),
        })
    }
}

impl<A: Annotation> ObjectCtorArgs<A> {
    pub fn iter(&self) -> impl Iterator<Item = &ObjectCtorMember<A>> {
        self.members.iter()
    }
}

impl<A: Annotation> fmt::Display for ObjectCtorArgs<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (i, member) in self.members.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", member)?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CollectionCtorElement<A: Annotation> {
    pub value: Expr<A>,
}

impl<A: Annotation> fmt::Display for CollectionCtorElement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<A: Annotation> Spanned for CollectionCtorElement<A> {
    fn span(&self) -> &Span {
        self.value.span()
    }
}

impl ParseSeq for CollectionCtorElement<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Comma)?;
        }

        let value = Expr::parse(tokens)?;
        Ok(CollectionCtorElement { value })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Comma).is_none() {
            return false;
        }

        tokens.match_one(Matcher::ExprOperandStart).is_some()
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ObjectCtor<A: Annotation = Span> {
    pub type_expr: Option<Expr<A>>,
    
    pub args: ObjectCtorArgs<A>,
    pub type_args: Option<TypeArgList<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for ObjectCtor<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(type_expr) = &self.type_expr {
            write!(f, "{}", type_expr)?;
        }
        write!(f, "{}", self.args)
    }
}

impl<A: Annotation> Spanned for ObjectCtor<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct CollectionCtor<A: Annotation> {
    pub elements: Vec<CollectionCtorElement<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for CollectionCtor<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", element)?;
        }
        write!(f, "]")
    }
}

impl CollectionCtor<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let (span, mut elems_tokens) =
            match tokens.match_one(Matcher::Delimited(DelimiterPair::SquareBracket))? {
                TokenTree::Delimited(group) => (group.span.clone(), group.to_inner_tokens()),

                _ => unreachable!(),
            };

        let elements = CollectionCtorElement::parse_seq(&mut elems_tokens)?;
        if elements.len() > 0 {
            elems_tokens.match_one_maybe(Separator::Comma);
        }

        elems_tokens.finish()?;

        Ok(Self {
            elements,
            annotation: span,
        })
    }
}
