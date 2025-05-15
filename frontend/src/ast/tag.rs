use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::IdentPath;
use crate::ast::ObjectCtor;
use crate::ast::ObjectCtorArgs;
use crate::ast::ObjectCtorMember;
use crate::parse::LookAheadTokenStream;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::TokenStream;
use common::span::Span;
use common::span::Spanned;
use derivative::Derivative;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct Tag<A: Annotation = Span> {
    pub items: Vec<ObjectCtor<A>>,

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
            let tag_type_path = IdentPath::parse(&mut tag_tokens)?;

            let ctor_group = DelimitedGroup::parse(&mut tag_tokens, DelimiterPair::Bracket)?;

            let ctor_span = ctor_group.span.clone();
            let mut ctor_tokens = ctor_group.to_inner_tokens();

            let members = ObjectCtorMember::parse_seq(&mut ctor_tokens)?;
            ctor_tokens.finish()?;

            items.push(ObjectCtor {
                annotation: tag_type_path.first().span.to(&ctor_span),
                type_expr: Some(Expr::from(tag_type_path)),
                type_args: None,
                args: ObjectCtorArgs {
                    span: ctor_span,
                    members,
                },
            });

            if tag_tokens.look_ahead().next().is_none() {
                break;
            }
        }

        tag_tokens.finish()?;

        Ok(Tag { items, span })
    }
}
