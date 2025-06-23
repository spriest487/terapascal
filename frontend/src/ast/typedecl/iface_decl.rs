use crate::ast::{iface_method_start, parse_separated_members, SupersClause};
use crate::ast::tag::Tag;
use crate::ast::typedecl::TypeDeclHeader;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::DeclIdent;
use crate::ast::WhereClause;
use crate::parse::{ContinueParse, LookAheadTokenStream, Parser};
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::{Keyword, TokenTree};
use crate::Separator;
use derivative::*;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceMethodDecl<A: Annotation> {
    pub decl: Arc<FunctionDecl<A>>,
}

impl<A: Annotation> InterfaceMethodDecl<A> {
    pub fn ident(&self) -> &Ident {
        self.decl.name.ident()
    }
}

impl<A: Annotation> Spanned for InterfaceMethodDecl<A> {
    fn span(&self) -> &Span {
        self.decl.span()
    }
}

impl<A: Annotation> fmt::Display for InterfaceMethodDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.decl)
    }
}

impl ParseSeq for InterfaceMethodDecl<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let tags = Tag::parse_seq(tokens)?;

        let decl = FunctionDecl::parse(tokens, false, tags)?;
        Ok(InterfaceMethodDecl { 
            decl: decl.into(),
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens
            .match_one(iface_method_start())
            .is_some()
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct InterfaceDecl<A: Annotation = Span> {
    pub name: A::DeclName,
    pub where_clause: Option<WhereClause<A>>,

    pub tags: Vec<Tag<A>>,
    
    pub methods: Vec<InterfaceMethodDecl<A>>,

    pub supers: Option<SupersClause<A>>,
    
    pub forward: bool,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub kw_span: Span,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub end_kw_span: Option<Span>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> InterfaceDecl<A> {
    pub fn get_method(&self, method: &Ident) -> Option<&InterfaceMethodDecl<A>> {
        self.methods.iter().find(|m| *m.ident() == *method)
    }
    
    pub fn super_types(&self) -> &[A::TypeName] {
        match &self.supers {
            Some(supers) => &supers.types,
            None => &[],
        }
    }
}

impl InterfaceDecl<Span> {
    pub fn parse(
        parser: &mut Parser,
        name: DeclIdent,
        tags: Vec<Tag>,
        keyword_token: TokenTree,
    ) -> ParseResult<Self> {
        let header = TypeDeclHeader::parse(parser.tokens(), Keyword::Interface, &tags, &name.span)
            .or_continue_with(parser.errors(), || TypeDeclHeader::new(keyword_token));

        if header.forward {
            Ok(InterfaceDecl {
                kw_span: header.keyword.into_span().into(),
                name,
                where_clause: header.where_clause,
                tags: Vec::new(),

                supers: header.supers,
                span: header.span.into(),
                forward: true,
                methods: Vec::new(),
                end_kw_span: None,
            })
        } else {
            let mut methods = Vec::new();
            parse_separated_members(parser, &mut methods, |parser| {
                if parser.tokens().look_ahead().match_one(iface_method_start()).is_none() {
                    return None;
                }
                
                let tags = Tag::parse_seq(parser.tokens())
                    .or_continue_with(parser.errors(), Vec::new);

                let decl = FunctionDecl::parse(parser.tokens(), false, tags)
                    .ok_or_continue(parser.errors())?;
                
                Some(InterfaceMethodDecl {
                    decl: decl.into(),
                })
            });

            // no more methods found, must be "end" next, but if there's an invalid token, the error
            // should indicate that it could've been a method too
            let end_span = if let Err(mut err) = parser.tokens().match_one(Keyword::End) {
                if let ParseError::UnexpectedToken(_, Some(expected)) = &mut err.err {
                    *expected |= iface_method_start()
                }

                parser.error(err);
                parser.advance_to(Keyword::End).map(TokenTree::into_span)
            } else {
                None
            };
            
            let decl_span = match &end_span {
                Some(end_span) => {
                    header.keyword.span().to(end_span)
                }  
                
                None => match methods.last().map(|m| &m.decl.span) {
                    Some(method_span) => header.keyword.span().to(method_span),
                    None => header.keyword.span().clone(),
                }
            };

            Ok(InterfaceDecl {
                name,
                where_clause: header.where_clause,
                tags,
                supers: header.supers,
                forward: false,
                methods,
                span: decl_span,
                kw_span: header.keyword.into_span().into(),
                end_kw_span: end_span,
            })
        }
    }
}

impl<A: Annotation> Spanned for InterfaceDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for InterfaceDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "interface")?;
        for method in &self.methods {
            writeln!(f, "{};", method)?;
        }
        write!(f, "end")
    }
}
