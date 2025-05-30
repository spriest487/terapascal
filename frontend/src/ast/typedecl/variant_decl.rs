use crate::ast::tag::Tag;
use crate::ast::type_method_start;
use crate::ast::type_name::TypeName;
use crate::ast::typedecl::TypeDeclStart;
use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::MethodDecl;
use crate::ast::MethodOwner;
use crate::ast::TypeDeclName;
use crate::ast::WhereClause;
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::parse::TryParse;
use crate::Separator;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use derivative::*;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantDecl<A: Annotation> {
    pub name: Rc<A::Name>,
    pub where_clause: Option<WhereClause<A::Type>>,
    
    pub forward: bool,

    pub tags: Vec<Tag<A>>,
    
    pub cases: Vec<VariantCase<A>>,

    pub implements: Vec<A::Type>,
    
    pub methods: Vec<MethodDecl<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> MethodOwner<A> for VariantDecl<A> {
    fn methods(&self) -> &[MethodDecl<A>] {
        self.methods.as_slice()
    }
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
    pub fn find_case(&self, case: &Ident) -> Option<&VariantCase<A>> {
        self.cases.iter().find(|c| c.ident == *case)
    }
}

impl VariantDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName, tags: Vec<Tag>) -> ParseResult<Self> {
        let decl_start = TypeDeclStart::parse(tokens, Keyword::Variant, &tags, &name.span)?;

        if decl_start.forward {
            Ok(VariantDecl {
                name: Rc::new(name),
                where_clause: decl_start.where_clause,
                
                forward: true,

                tags: Vec::new(),
                
                cases: Vec::new(),
                
                implements: decl_start.supers,
                methods: Vec::new(),
                
                span: decl_start.span,
            })
        } else {
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
                    .match_one(type_method_start())
                    .is_some();

                if !func_ahead {
                    break;
                }
                
                let tags = Tag::parse_seq(tokens)?;

                let method_decl= FunctionDecl::parse(tokens, true, tags)?;
                methods.push(MethodDecl { 
                    func_decl: Rc::new(method_decl),
                    access,
                });

                if tokens.match_one_maybe(Separator::Semicolon).is_none() {
                    break;
                }
            }

            let end_kw = tokens.match_one(Keyword::End)?;

            Ok(VariantDecl {
                name: Rc::new(name),
                where_clause: decl_start.where_clause,

                tags,

                forward: false,
                cases,
                span: decl_start.span.to(end_kw.span()),

                implements: decl_start.supers,
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
