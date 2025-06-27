use crate::ast::parse_separated_members;
use crate::ast::tag::Tag;
use crate::ast::type_method_start;
use crate::ast::type_name::TypeName;
use crate::ast::typedecl::TypeDeclHeader;
use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::DeclIdent;
use crate::ast::FunctionDecl;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::MethodDecl;
use crate::ast::MethodDeclSection;
use crate::ast::MethodOwner;
use crate::ast::SupersClause;
use crate::ast::WhereClause;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::Parser;
use crate::result::ErrorContinue;
use crate::Separator;
use crate::TokenTree;
use derivative::*;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantDecl<A: Annotation = Span> {
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub kw_span: Span,

    pub name: Arc<A::DeclName>,
    pub where_clause: Option<WhereClause<A>>,

    pub forward: bool,

    pub tags: Vec<Tag<A>>,

    pub cases: Vec<VariantCase<A>>,

    pub implements: Option<SupersClause<A>>,

    pub sections: Vec<MethodDeclSection<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub end_kw_span: Option<Span>,
}

impl<A: Annotation> MethodOwner<A> for VariantDecl<A> {
    fn methods<'a>(&'a self) -> impl Iterator<Item = &'a MethodDecl<A>>
    where
        A: 'a,
    {
        self.sections
            .iter()
            .flat_map(|section| section.methods.iter())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCaseData<TypeName> {
    pub ty: TypeName,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCase<A: Annotation = Span> {
    pub ident: Ident,
    pub data: Option<VariantCaseData<TypeName<A>>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> VariantDecl<A> {
    pub fn case_position(&self, case_ident: &Ident) -> Option<usize> {
        self.cases.iter().position(|c| c.ident == *case_ident)
    }

    pub fn find_case(&self, case: &str) -> Option<&VariantCase<A>> {
        self.cases.iter().find(|c| c.ident.as_str() == case)
    }

    pub fn implements_types(&self) -> &[TypeName<A>] {
        match &self.implements {
            Some(implements) => &implements.types,
            None => &[],
        }
    }
}

impl VariantDecl {
    pub fn parse(
        parser: &mut Parser,
        name: DeclIdent,
        tags: Vec<Tag>,
        keyword_token: TokenTree,
    ) -> Self {
        let header = TypeDeclHeader::parse_or_empty(
            parser,
            Keyword::Variant,
            keyword_token,
            &tags,
            &name.span,
        );

        let kw_span = header.keyword.into_span();

        if header.forward {
            VariantDecl {
                kw_span: kw_span.into(),

                name: Arc::new(name),
                where_clause: header.where_clause,

                forward: true,

                tags: Vec::new(),

                cases: Vec::new(),

                implements: header.supers,
                sections: Vec::new(),

                span: header.span.into(),

                end_kw_span: None,
            }
        } else {
            let mut cases = Vec::new();

            let last_sep = parse_separated_members(parser, &mut cases, |parser| {
                VariantCase::try_parse(parser)
                    .ok_or_continue(parser.errors())
                    .flatten()
            });

            let sections = if last_sep.is_some() {
                parse_method_sections(parser, Access::Public)
            } else {
                Vec::new()
            };

            let end_kw_span = parser.advance_to(Keyword::End).map(TokenTree::into_span);

            let decl_span = end_kw_span
                .as_ref()
                .map(|end_span| header.span.to(end_span))
                .unwrap_or_else(|| {
                    sections
                        .last()
                        .and_then(|last_section| last_section.methods.last())
                        .map(|last_decl| last_decl.func_decl.span())
                        .cloned()
                        .unwrap_or_else(|| header.span.clone())
                });

            VariantDecl {
                kw_span: kw_span.into(),

                name: Arc::new(name),
                where_clause: header.where_clause,

                tags,

                forward: false,
                cases,
                span: decl_span,

                implements: header.supers,
                sections,

                end_kw_span,
            }
        }
    }
}

impl VariantCase {
    fn try_parse(parser: &mut Parser) -> ParseResult<Option<Self>> {
        let Some(tt) = parser.tokens().match_one_maybe(Matcher::AnyIdent) else {
            return Ok(None);
        };

        let ident = tt.into_ident().unwrap();

        let case = match parser.tokens().match_one_maybe(Separator::Colon) {
            Some(..) => {
                let ty = TypeName::parse(parser.tokens())?;

                let mut case_span = ident.span.clone();
                case_span.maybe_extend(&ty);

                VariantCase {
                    ident,
                    data: Some(VariantCaseData { ty }),
                    span: case_span,
                }
            },

            None => VariantCase {
                span: ident.span.clone(),
                ident,
                data: None,
            },
        };

        Ok(Some(case))
    }
}

fn parse_method_sections(parser: &mut Parser, default_access: Access) -> Vec<MethodDeclSection> {
    let mut sections = Vec::new();

    let mut current_access = default_access;
    let mut current_access_kw = None;

    let mut methods = Vec::new();
    loop {
        if let Some((new_access, new_access_span)) = Access::try_parse(parser.tokens()) {
            if current_access_kw.is_some() || !methods.is_empty() {
                let mut section_methods = Vec::with_capacity(methods.len());
                section_methods.append(&mut methods);

                sections.push(MethodDeclSection {
                    access: current_access,
                    access_kw_span: current_access_kw,
                    methods: section_methods,
                });
            }

            current_access = new_access;
            current_access_kw = Some(new_access_span);
        }

        let func_ahead = parser
            .tokens()
            .look_ahead()
            .match_one(type_method_start())
            .is_some();

        if !func_ahead {
            break;
        }

        let tags = Tag::parse_seq(parser.tokens()).or_continue_with(parser.errors(), Vec::new);

        match FunctionDecl::parse(parser, true, tags) {
            Ok(method_decl) => {
                methods.push(MethodDecl {
                    func_decl: method_decl.into(),
                    access: current_access,
                });

                if parser
                    .tokens()
                    .match_one_maybe(Separator::Semicolon)
                    .is_none()
                {
                    break;
                }
            },

            Err(err) => {
                parser.error(err);
                if !parser.advance_to(Separator::Semicolon).is_some() {
                    break;
                }
            },
        }
    }

    if current_access_kw.is_some() || !methods.is_empty() {
        sections.push(MethodDeclSection {
            access: current_access,
            access_kw_span: current_access_kw,
            methods,
        });
    }

    sections
}

impl<A: Annotation> fmt::Display for VariantDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "variant {}", self.name)?;
        for case in &self.cases {
            write!(f, "  {}", case.ident)?;
            if let Some(data) = &case.data {
                write!(f, ": {}", data.ty)?;
            }
            writeln!(f, ";")?;
        }
        write!(f, "end")
    }
}

impl<A: Annotation> Spanned for VariantDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}
