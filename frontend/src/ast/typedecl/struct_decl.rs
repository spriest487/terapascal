mod member;

use crate::ast::tag::Tag;
use crate::ast::typedecl::TypeDeclStart;
use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::DeclIdent;
use crate::ast::Ident;
use crate::ast::MethodOwner;
use crate::ast::WhereClause;
use crate::parse::Matcher;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use derivative::*;
pub use member::*;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[cfg(test)]
mod test;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum StructKind {
    /// heap-allocated, reference-counted type, passed by pointer. declared
    /// with the `class` keyword.
    Class,

    /// locally-allocated value type. declared with the `record` keyword.
    Record,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct StructDecl<A: Annotation = Span> {
    pub kind: StructKind,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub kw_span: Span,

    pub name: A::DeclName,
    pub where_clause: Option<WhereClause<A::Type>>,

    pub tags: Vec<Tag<A>>,
    
    pub packed: bool,
    
    pub forward: bool,

    pub sections: Vec<StructDeclSection<A>>,
    
    pub implements: Vec<A::Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> StructDecl<A> {    
    pub fn members(&self) -> impl Iterator<Item=&TypeMemberDecl<A>> {
        self.sections
            .iter()
            .flat_map(|section| section.members.iter())
    }
    
    pub fn fields(&self) -> impl Iterator<Item=&FieldDecl<A>> {
        self.members()
            .filter_map(|member| match member {
                TypeMemberDecl::Field(field) => Some(field),
                TypeMemberDecl::Method(..) => None,
            })
    }

    pub fn methods(&self) -> impl Iterator<Item=&MethodDecl<A>> {
        self.members()
            .filter_map(|member| match member {
                TypeMemberDecl::Method(method) => Some(method),
                TypeMemberDecl::Field(..) => None,
            })
    }

    pub fn find_field_decl(&self, by_ident: &Ident) -> Option<&FieldDecl<A>> {
        self.fields().find(|field| {
            field.idents.contains(by_ident)
        })
    }
}

impl<A: Annotation> MethodOwner<A> for StructDecl<A> {
    fn methods<'a>(&'a self) -> impl Iterator<Item=&'a MethodDecl<A>>
    where A: 'a
    {
        StructDecl::methods(self)
    }
}

impl StructDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: DeclIdent, tags: Vec<Tag>) -> ParseResult<Self> {
        let packed_kw = tokens.match_one_maybe(Keyword::Packed);
        let packed = packed_kw.is_some();
        
        let match_kw = if packed {
            Matcher::from(Keyword::Record)
        } else {
            Keyword::Record | Keyword::Class
        };

        let decl_start = TypeDeclStart::parse(tokens, match_kw, &tags, &name.span)?;

        let kind = match &decl_start.keyword {
            tt if tt.is_keyword(Keyword::Class) => StructKind::Class,
            tt if tt.is_keyword(Keyword::Record) => StructKind::Record,
            _ => unreachable!(),
        };
        
        let kw_span = decl_start.keyword.into_span();

        let span = match packed_kw {
            Some(tt) => tt.span().to(&decl_start.span),
            None => decl_start.span,
        };
        
        // the last type in a section can never be forward, so every legal forward declaration
        // will end with a semicolon
        if decl_start.forward {
            Ok(StructDecl {
                kw_span,
                kind,
                name,
                where_clause: decl_start.where_clause,
                packed,
                tags: Vec::new(),

                forward: true,
                implements: decl_start.supers,
                
                sections: Vec::new(),
                span,
            })
        } else {
            let default_access = match kind {
                StructKind::Class => Access::Private,
                StructKind::Record => Access::Public,
            };

            let sections = parse_struct_sections(tokens, default_access)?;

            let end_token = tokens.match_one(Keyword::End)?;

            Ok(StructDecl {
                kw_span,
                kind,
                name,
                where_clause: decl_start.where_clause,
                packed,
                tags,
                forward: false,
                implements: decl_start.supers,
                sections,
                span: span.to(end_token.span()),
            })
        }
    }
}

impl<A: Annotation> Spanned for StructDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for StructDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.packed {
            write!(f, "packed ")?;
        }

        let kind = match self.kind {
            StructKind::Record => "record",
            StructKind::Class => "class",
        };
        writeln!(f, "{}", kind)?;
        
        for section in &self.sections {
            writeln!(f, "{}", section.access)?;
            for member in &section.members {
                match member {
                    TypeMemberDecl::Field(field) => writeln!(f, "  {};", field)?,
                    TypeMemberDecl::Method(method) => writeln!(f, "  {};", method.func_decl)?,
                }
            }
        }

        write!(f, "end")
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct StructDeclSection<A: Annotation = Span> {
    pub access: Access,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub access_kw_span: Option<Span>,
    
    pub members: Vec<TypeMemberDecl<A>>,
}

impl<A: Annotation> MemberDeclSection<A> for StructDeclSection<A> {
    type Source = StructDeclSection;

    fn clone_empty(other: &Self::Source) -> Self {
        Self {
            access: other.access,
            access_kw_span: other.access_kw_span.clone(),
            members: Vec::with_capacity(other.members.len()),
        }
    }

    fn access(&self) -> Access {
        self.access
    }

    fn add_field(&mut self, field: FieldDecl<A>) -> bool {
        self.members.push(TypeMemberDecl::Field(field));
        true
    }

    fn add_method(&mut self, method: MethodDecl<A>) -> bool {
        self.members.push(TypeMemberDecl::Method(method));
        true
    }

    fn access_kw_span(&self) -> Option<&Span> {
        self.access_kw_span.as_ref()
    }

    fn members<'a>(&'a self) -> impl Iterator<Item=TypeMemberDeclRef<'a, A>>
    where A: 'a
    {
        self.members
            .iter()
            .map(|member| match member {
                TypeMemberDecl::Field(field) => TypeMemberDeclRef::Field(field),
                TypeMemberDecl::Method(method) => TypeMemberDeclRef::Method(method),
            })
    }
} 
