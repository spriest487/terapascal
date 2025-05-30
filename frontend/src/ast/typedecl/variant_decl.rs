use crate::ast::tag::Tag;
use crate::ast::type_name::TypeName;
use crate::ast::typedecl::TypeDeclStart;
use crate::ast::{Access, SupersClause};
use crate::ast::Annotation;
use crate::ast::DeclIdent;
use crate::ast::FunctionDecl;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::MethodDecl;
use crate::ast::MethodOwner;
use crate::ast::WhereClause;
use crate::ast::{type_method_start, MethodDeclSection};
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::Separator;
use derivative::*;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantDecl<A: Annotation> {
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
    fn methods<'a>(&'a self) -> impl Iterator<Item=&'a MethodDecl<A>>
    where A: 'a
    {
        self.sections.iter()
            .flat_map(|section| section.methods.iter())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCaseData<T> {
    pub ty: T,
    pub span: Span,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCase<A: Annotation> {
    pub ident: Ident,
    pub data: Option<VariantCaseData<A::Type>>,

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
                    data: Some(VariantCaseData {
                        span: ty.span().clone(),
                        ty
                    }),
                }
            },

            None => VariantCase {
                span: ident.span.clone(),
                ident,
                data: None,
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
    
    pub fn find_case(&self, case: &Ident) -> Option<&VariantCase<A>> {
        self.cases.iter().find(|c| c.ident == *case)
    }

    pub fn implements_types(&self) -> &[A::TypeName] {
        match &self.implements {
            Some(implements) => &implements.types,
            None => &[],
        }
    }
}

impl VariantDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: DeclIdent, tags: Vec<Tag>) -> ParseResult<Self> {
        let decl_start = TypeDeclStart::parse(tokens, Keyword::Variant, &tags, &name.span)?;

        let kw_span = decl_start.keyword.into_span();

        if decl_start.forward {
            Ok(VariantDecl {
                kw_span: kw_span.into(),

                name: Arc::new(name),
                where_clause: decl_start.where_clause,
                
                forward: true,

                tags: Vec::new(),
                
                cases: Vec::new(),
                
                implements: decl_start.supers,
                sections: Vec::new(),
                
                span: decl_start.span.into(),
                
                end_kw_span: None,
            })
        } else {
            let cases = VariantCase::parse_seq(tokens)?;

            let sections = if tokens.match_one_maybe(Separator::Semicolon).is_some() {
                parse_method_sections(tokens, Access::Public)?
            } else {
                Vec::new()
            };

            let end_kw = tokens.match_one(Keyword::End)?;

            Ok(VariantDecl {
                kw_span: kw_span.into(),

                name: Arc::new(name),
                where_clause: decl_start.where_clause,

                tags,

                forward: false,
                cases,
                span: decl_start.span.to(end_kw.span()).into(),

                implements: decl_start.supers,
                sections,
                
                end_kw_span: Some(end_kw.into_span().into()),
            })
        }
    }
}

fn parse_method_sections(tokens: &mut TokenStream, default_access: Access) -> ParseResult<Vec<MethodDeclSection>> {
    let mut sections = Vec::new();
    
    let mut current_access = default_access;
    let mut current_access_kw = None;

    let mut methods = Vec::new();
    loop {
        if let Some((new_access, new_access_span)) = Access::try_parse(tokens)? {
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
            func_decl: method_decl.into(),
            access: current_access,
        });

        if tokens.match_one_maybe(Separator::Semicolon).is_none() {
            break;
        }
    }

    if current_access_kw.is_some() || !methods.is_empty() {
        sections.push(MethodDeclSection {
            access: current_access,
            access_kw_span: current_access_kw,
            methods,
        });
    }
    
    Ok(sections)
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
