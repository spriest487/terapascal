use crate::ast::tag::Tag;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::TypeDeclName;
use crate::ast::{iface_method_start, parse_implements_clause};
use crate::parse::LookAheadTokenStream;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Separator;
use common::span::Span;
use common::span::Spanned;
use derivative::*;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceMethodDecl<A: Annotation> {
    pub decl: Rc<FunctionDecl<A>>,
    pub tags: Vec<Tag<A>>,
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

        let decl = FunctionDecl::parse(tokens, false)?;
        Ok(InterfaceMethodDecl { 
            decl: Rc::new(decl),
            tags,
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
pub struct InterfaceDecl<A: Annotation> {
    pub name: A::Name,
    pub tags: Vec<Tag<A>>,
    
    pub methods: Vec<InterfaceMethodDecl<A>>,

    pub supers: Vec<A::Type>,
    
    pub forward: bool,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
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
        name: TypeDeclName,
        tags: Vec<Tag>
    ) -> ParseResult<Self> {
        let iface_kw = tokens.match_one(Keyword::Interface)?;
        
        // the last type in a section can never be forward, so every legal forward declaration
        // will end with a semicolon
        if tokens.look_ahead().match_one(Separator::Semicolon).is_some() {
            if !tags.is_empty() {
                return Err(ParseError::forward_decl_tags(name.span, &tags).into())
            }

            Ok(InterfaceDecl {
                name,
                tags: Vec::new(),

                supers: Vec::new(),
                span: iface_kw.into_span(),
                forward: true,
                methods: Vec::new()
            })
        } else {
            let supers = parse_implements_clause(tokens)?;
            
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
                tags,
                supers,
                span: iface_kw.span().to(end.span()),
                forward: false,
                methods,
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
