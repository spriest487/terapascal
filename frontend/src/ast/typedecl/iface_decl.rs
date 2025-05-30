use crate::ast::iface_method_start;
use crate::ast::tag::Tag;
use crate::ast::typedecl::TypeDeclStart;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::DeclIdent;
use crate::ast::WhereClause;
use crate::parse::LookAheadTokenStream;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::Keyword;
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

    pub supers: Vec<A::Type>,
    
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
}

impl InterfaceDecl<Span> {
    pub fn parse(
        tokens: &mut TokenStream,
        name: DeclIdent,
        tags: Vec<Tag>
    ) -> ParseResult<Self> {
        let decl_start = TypeDeclStart::parse(tokens, Keyword::Interface, &tags, &name.span)?;

        if decl_start.forward {
            Ok(InterfaceDecl {
                kw_span: decl_start.keyword.into_span().into(),
                name,
                where_clause: decl_start.where_clause,
                tags: Vec::new(),

                supers: decl_start.supers,
                span: decl_start.span.into(),
                forward: true,
                methods: Vec::new(),
                end_kw_span: None,
            })
        } else {
            let methods = InterfaceMethodDecl::parse_seq(tokens)?;
            tokens.match_one_maybe(Separator::Semicolon);

            // no more methods found, must be "end" next, but if there's an invalid token, the error
            // should indicate that it could've been a method too
            let end = tokens
                .match_one(Keyword::End)
                .map_err(|mut err| {
                    if let ParseError::UnexpectedToken(_, Some(expected)) = &mut err.err {
                        *expected |= iface_method_start()
                    }

                    err
                })?;

            Ok(InterfaceDecl {
                name,
                where_clause: decl_start.where_clause,
                tags,
                supers: decl_start.supers,
                forward: false,
                methods,
                span: decl_start.keyword.span().to(end.span()).into(),
                kw_span: decl_start.keyword.into_span().into(),
                end_kw_span: Some(end.into_span().into()),
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
