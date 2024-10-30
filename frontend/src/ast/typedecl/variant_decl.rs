use crate::ast::parse_implements_clause;
use crate::ast::type_name::TypeName;
use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::MethodDecl;
use crate::ast::TypeDeclName;
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::parse::TryParse;
use crate::Separator;
use common::span::Span;
use common::span::Spanned;
use derivative::*;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantDecl<A: Annotation> {
    pub name: Rc<A::Name>,
    pub forward: bool,
    
    pub cases: Vec<VariantCase<A>>,

    pub implements: Vec<A::Type>,
    
    pub methods: Vec<MethodDecl<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCase<A: Annotation> {
    pub ident: Ident,
    pub data_ty: Option<A::Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl ParseSeq for VariantCase<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;

        let case = match tokens.match_one_maybe(Separator::Colon) {
            Some(..) => {
                let ty = TypeName::parse(tokens)?;
                let span = ident.span().to(ty.span());

                VariantCase {
                    span,
                    ident,
                    data_ty: Some(ty),
                }
            },

            None => VariantCase {
                span: ident.span.clone(),
                ident,
                data_ty: None,
            },
        };

        Ok(case)
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

impl<A: Annotation> VariantDecl<A> {
    pub fn case_position(&self, case_ident: &Ident) -> Option<usize> {
        self.cases.iter().position(|c| c.ident == *case_ident)
    }
}

impl<A: Annotation> VariantDecl<A> {
    pub fn find_methods<'a>(&'a self, ident: &'a Ident) -> impl Iterator<Item=&'a MethodDecl<A>> {
        self.methods
            .iter()
            .filter(move |m| m.decl.ident() == ident)
    }
}

impl<A: Annotation> VariantDecl<A> {
    pub fn find_case(&self, case: &Ident) -> Option<&VariantCase<A>> {
        self.cases.iter().find(|c| c.ident == *case)
    }
}

impl VariantDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Variant)?;

        // the last type in a section can never be forward, so every legal forward declaration
        // will end with a semicolon
        if tokens.look_ahead().match_one(Separator::Semicolon).is_some() {
            Ok(VariantDecl {
                name: Rc::new(name),
                forward: true,
                
                cases: Vec::new(),
                
                implements: Vec::new(),
                methods: Vec::new(),
                
                span: kw.into_span(),
            })
        } else {
            let implements = parse_implements_clause(tokens)?;

            let cases = VariantCase::parse_seq(tokens)?;
            tokens.match_one_maybe(Separator::Semicolon);
            
            let mut access = Access::Public;
            
            let mut methods = Vec::new();

            loop {
                if let Some(new_access) = Access::try_parse(tokens)? {
                    access = new_access;
                }

                let func_ahead = tokens
                    .look_ahead()
                    .match_one(Keyword::Function | Keyword::Procedure);

                if func_ahead.is_none() {
                    break;
                }

                let method_decl= FunctionDecl::parse(tokens)?;
                methods.push(MethodDecl { 
                    decl: Rc::new(method_decl),
                    access,
                });

                if tokens.match_one_maybe(Separator::Semicolon).is_none() {
                    break;
                }
            }

            let end_kw = tokens.match_one(Keyword::End)?;

            Ok(VariantDecl {
                name: Rc::new(name),
                forward: false,
                cases,
                span: kw.span().to(end_kw.span()),

                implements,
                methods,
            })
        }
    }
}

impl<A: Annotation> fmt::Display for VariantDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "variant {}", self.name)?;
        for case in &self.cases {
            write!(f, "  {}", case.ident)?;
            if let Some(data_ty) = &case.data_ty {
                write!(f, ": {}", data_ty)?;
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
