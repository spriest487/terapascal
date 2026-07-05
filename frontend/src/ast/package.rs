use crate::ast::UseDeclItem;
use crate::parse::Matcher;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::Parser;
use crate::parse::{AggregateParseError, Parse};
use crate::result::ErrorContinue;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use crate::TokenTree;
use derivative::Derivative;
use terapascal_common::ident::Ident;
use terapascal_common::ident::IdentPath;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct PackageUnit {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub kw: Span,

    pub name: IdentPath,

    pub requires: Option<PackageRequires>,
    pub contains: Option<PackageContains>,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct PackageRequires {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub kw: Span,

    pub package_names: Vec<IdentPath>,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct PackageContains {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub kw: Span,

    pub units: Vec<UseDeclItem>,
}

impl PackageUnit {
    pub fn is_package(tokens: &[TokenTree]) -> bool {
        let Some(tt) = tokens.get(0) else {
            return false;
        };

        tt.is_ident("package")
    }

    pub fn parse(mut parser: Parser) -> ParseResult<Self> {
        let kw = parser.match_one("package")?.into_span();

        let name = match IdentPath::parse(&mut parser) {
            Ok(path) => path,
            Err(err) => {
                parser.error(err);
                parser.advance_until(Matcher::from("requires") | "contains" | Keyword::End);

                IdentPath::new(Ident::new("", kw.span().clone()), [])
            },
        };

        Self::expect_semicolon(&mut parser);

        let mut package = PackageUnit {
            kw,
            name,
            requires: None,
            contains: None,
        };

        if let Some(requires_kw) = parser.match_one_maybe("requires") {
            let package_names = Self::parse_requires(&mut parser);

            package.requires = Some(PackageRequires {
                kw: requires_kw.into_span(),
                package_names,
            });

            Self::expect_semicolon(&mut parser);
        }

        if let Some(contains_kw) = parser.match_one_maybe("contains") {
            let units =
                UseDeclItem::parse_seq(&mut parser).or_continue_with(parser.errors(), Vec::new);

            package.contains = Some(PackageContains {
                kw: contains_kw.into_span(),
                units,
            });

            Self::expect_semicolon(&mut parser);
        }

        if let Err(err) = parser.match_sequence(Keyword::End + Operator::Period) {
            parser.error(err);
        }

        AggregateParseError::result(package, parser.finish())
            .map_err(ParseError::PackageWithErrors)
            .map_err(TracedError::trace)
    }

    fn parse_requires(parser: &mut Parser) -> Vec<IdentPath> {
        let mut package_names = Vec::new();

        loop {
            if !package_names.is_empty() && parser.match_one_maybe(Separator::Comma).is_none() {
                break;
            }

            if parser.look_ahead().match_one(Matcher::AnyIdent).is_none() {
                break;
            }

            let package_name = match IdentPath::parse(parser) {
                Ok(path) => path,
                Err(err) => {
                    parser.error(err);
                    parser.advance_until(Separator::Comma);
                    continue;
                },
            };

            package_names.push(package_name);
        }

        package_names
    }

    fn expect_semicolon(parser: &mut Parser) {
        parser
            .match_one(Separator::Semicolon)
            .map(|_| ())
            .or_continue(parser.errors(), ());
    }
}
