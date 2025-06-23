use crate::ast::tag::Tag;
use crate::ast::type_method_start;
use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::Ident;
use crate::ast::StructDeclSection;
use crate::ast::TypeName;
use crate::parse::{ContinueParse, ParseError};
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::Parser;
use crate::Keyword;
use crate::Separator;
use crate::TokenStream;
use crate::TokenTree;
use derivative::Derivative;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum TypeMemberDecl<A: Annotation = Span> {
    Field(FieldDecl<A>),
    Method(MethodDecl<A>),
}

impl<A: Annotation> TypeMemberDecl<A> {
    pub fn as_field(&self) -> Option<&FieldDecl<A>> {
        match self {
            TypeMemberDecl::Field(field) => Some(field),
            _ => None,
        }
    }

    pub fn as_method(&self) -> Option<&MethodDecl<A>> {
        match self {
            TypeMemberDecl::Method(method) => Some(method),
            _ => None,
        }
    }
}

impl<A: Annotation> From<FieldDecl<A>> for TypeMemberDecl<A> {
    fn from(value: FieldDecl<A>) -> Self {
        TypeMemberDecl::Field(value)
    }
}

impl<A: Annotation> From<MethodDecl<A>> for TypeMemberDecl<A> {
    fn from(value: MethodDecl<A>) -> Self {
        TypeMemberDecl::Method(value)
    }
}

impl<A: Annotation> Spanned for TypeMemberDecl<A> {
    fn span(&self) -> &Span {
        match self {
            TypeMemberDecl::Field(field) => field.span(),
            TypeMemberDecl::Method(method) => method.func_decl.span(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TypeMemberDeclRef<'a, A: Annotation = Span> {
    Field(&'a FieldDecl<A>),
    Method(&'a MethodDecl<A>),
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct FieldDecl<A: Annotation = Span> {
    pub idents: Vec<Ident>,

    pub ty: A::TypeName,

    pub access: Access,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for FieldDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for FieldDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in 0..self.idents.len() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", self.idents[i])?;
        }

        write!(f, ": {}", self.ty)
    }
}

pub fn struct_member_start() -> Matcher {
    type_method_start() | Matcher::AnyIdent
}

pub fn parse_struct_sections(
    parser: &mut Parser,
    default_access: Access,
) -> Vec<StructDeclSection> {
    let mut current_access = default_access;
    let mut current_access_span = None;

    let mut sections: Vec<StructDeclSection> = Vec::new();

    let mut members = Vec::new();
    loop {
        if let Some((new_access, access_span)) = Access::try_parse(parser.tokens()) {
            if !members.is_empty() || current_access_span.is_some() {
                let mut section_members = Vec::new();
                section_members.append(&mut members);

                sections.push(StructDeclSection {
                    access: current_access,
                    access_kw_span: current_access_span,
                    members: section_members,
                });
            }

            current_access = new_access;
            current_access_span = Some(access_span);
        }
        
        if parser.tokens().look_ahead().match_one(Keyword::End).is_some() {
            break;
        }

        if let Some(member) = try_parse_section_member(parser, current_access) {
            members.push(member);
        }

        match parser.advance_until(Separator::Semicolon | Keyword::End) {
            Some(tt) if tt.is_keyword(Keyword::End) => {
                break;
            },

            Some(..) => {
                // semicolon
                parser.tokens().advance(1);
            },

            None => {
                break;
            },
        };
    }

    if !members.is_empty() || current_access_span.is_some() {
        sections.push(StructDeclSection {
            access: current_access,
            access_kw_span: current_access_span,
            members,
        });
    }

    sections
}

fn try_parse_section_member(parser: &mut Parser, access: Access) -> Option<TypeMemberDecl> {
    parser.advance_until(struct_member_start());
    
    let mut ahead = parser.tokens().look_ahead();
    let next_start = ahead.next()?;
    
    if type_method_start().is_match(&next_start) {
        let method = parse_method_decl(parser.tokens(), access)
            .ok_or_continue(parser.errors())?;

        Some(TypeMemberDecl::Method(method))
    } else if next_start.as_ident().is_some() {
        let field = parse_field(parser.tokens(), access)
            .ok_or_continue(parser.errors())?;

        Some(TypeMemberDecl::Field(field))
    } else {
        let unexpected = Box::new(next_start.clone());
        let expected = Some(type_method_start() | Matcher::AnyIdent);
        parser.error(TracedError::from(ParseError::UnexpectedToken(unexpected, expected)));
        
        parser.tokens().advance(1);
        
        None
    }
}

fn parse_field(tokens: &mut TokenStream, access: Access) -> ParseResult<FieldDecl> {
    let first_ident = Ident::parse(tokens)?;

    let mut rest_idents = Vec::new();
    while tokens.match_one_maybe(Separator::Comma).is_some() {
        let ident = Ident::parse(tokens)?;
        rest_idents.push(ident);
    }

    tokens.match_one(Separator::Colon)?;
    let ty = TypeName::parse(tokens)?;

    let span = first_ident.span.to(ty.span());

    rest_idents.insert(0, first_ident);

    Ok(FieldDecl {
        idents: rest_idents,
        access,
        span,
        ty,
    })
}

fn parse_method_decl(tokens: &mut TokenStream, access: Access) -> ParseResult<MethodDecl> {
    let tags = Tag::parse_seq(tokens)?;

    // these get parsed one at a time
    let decl = FunctionDecl::parse(tokens, true, tags)?;

    Ok(MethodDecl {
        func_decl: Arc::new(decl),
        access,
    })
}

pub fn parse_separated_members<F, T>(
    parser: &mut Parser,
    items: &mut Vec<T>,
    f: F,
) -> Option<TokenTree>
where
    F: Fn(&mut Parser) -> Option<T>,
{
    loop {
        let sep = if !items.is_empty() {
            let next_sep = parser.advance_to(Separator::Semicolon);
            if next_sep.is_none() {
                break None;
            } else {
                next_sep
            }
        } else {
            None
        };

        if let Some(item) = f(parser) {
            items.push(item);
        } else {
            break sep;
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDecl<A: Annotation = Span> {
    pub access: Access,
    pub func_decl: Arc<FunctionDecl<A>>,
}

pub trait MemberDeclSection<A: Annotation = Span> {
    type Source: MemberDeclSection<Span>;

    fn clone_empty(other: &Self::Source) -> Self;

    fn access(&self) -> Access;

    fn add_field(&mut self, field: FieldDecl<A>) -> bool;
    fn add_method(&mut self, method: MethodDecl<A>) -> bool;

    fn access_kw_span(&self) -> Option<&Span>;

    fn members<'a>(&'a self) -> impl Iterator<Item = TypeMemberDeclRef<'a, A>>
    where
        A: 'a;

    fn fields<'a>(&'a self) -> impl Iterator<Item = &'a FieldDecl<A>>
    where
        A: 'a,
    {
        self.members().filter_map(|member| match member {
            TypeMemberDeclRef::Field(field) => Some(field),
            TypeMemberDeclRef::Method(_) => None,
        })
    }

    fn methods<'a>(&'a self) -> impl Iterator<Item = &'a MethodDecl<A>>
    where
        A: 'a,
    {
        self.members().filter_map(|member| match member {
            TypeMemberDeclRef::Method(method) => Some(method),
            TypeMemberDeclRef::Field(_) => None,
        })
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MethodDeclSection<A: Annotation = Span> {
    pub access: Access,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub access_kw_span: Option<Span>,

    pub methods: Vec<MethodDecl<A>>,
}

impl<A: Annotation> MemberDeclSection<A> for MethodDeclSection<A> {
    type Source = MethodDeclSection;

    fn clone_empty(other: &MethodDeclSection) -> Self {
        Self {
            access: other.access,
            access_kw_span: other.access_kw_span.clone(),
            methods: Vec::with_capacity(other.methods.len()),
        }
    }

    fn access(&self) -> Access {
        self.access
    }

    fn add_field(&mut self, _: FieldDecl<A>) -> bool {
        false
    }

    fn add_method(&mut self, method: MethodDecl<A>) -> bool {
        self.methods.push(method);
        true
    }

    fn access_kw_span(&self) -> Option<&Span> {
        self.access_kw_span.as_ref()
    }

    fn members<'a>(&'a self) -> impl Iterator<Item = TypeMemberDeclRef<'a, A>>
    where
        A: 'a,
    {
        self.methods.iter().map(TypeMemberDeclRef::Method)
    }
}
