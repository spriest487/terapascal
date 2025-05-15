use crate::ast::IdentPath;
use crate::ast::IdentTypeName;
use crate::ast::TypeName;
use crate::ast::Annotation;
use crate::ast::ObjectCtorArgs;
use crate::parse::LookAheadTokenStream;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::token_tree::DelimitedGroup;
use crate::TokenStream;
use crate::{DelimiterPair, Separator};
use common::span::Span;
use common::span::Spanned;
use derivative::Derivative;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct Tag<A: Annotation = Span> {
    pub items: Vec<TagItem<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl Spanned for Tag {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl ParseSeq for Tag<Span> {
    fn parse_group(_prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        Tag::parse(tokens)
    }

    fn has_more(_prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        tokens.match_one(DelimiterPair::SquareBracket).is_some()
    }
}

impl Parse for Tag<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let group = DelimitedGroup::parse(tokens, DelimiterPair::SquareBracket)?;
        let span = group.span.clone();
        let mut tag_tokens = group.to_inner_tokens();

        let mut items = Vec::new();

        loop {
            let item = TagItem::parse(&mut tag_tokens)?;

            items.push(item);

            if tag_tokens.match_one_maybe(Separator::Semicolon).is_none() {
                break;
            }
        }

        tag_tokens.finish()?;

        Ok(Tag { items, span })
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct TagItem<A: Annotation = Span> {
    pub tag_type: A::Type,
    pub args: ObjectCtorArgs<A>,

    pub span: Span,
}

impl TagItem {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let tag_type = IdentPath::parse(tokens)?;
        
        let args = ObjectCtorArgs::parse(tokens)?;
        let span = tag_type.span().to(&args.span);

        Ok(Self {
            tag_type: TypeName::Ident(IdentTypeName {
                type_args: None,
                span: tag_type.path_span().clone(),
                ident: tag_type,
                indirection: 0,
            }),
            args,
            span,
        })
    }
}
