mod member;

use crate::ast::tag::Tag;
use crate::ast::typedecl::TypeDeclHeader;
use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::DeclIdent;
use crate::ast::MethodOwner;
use crate::ast::SupersClause;
use crate::ast::WhereClause;
use crate::parse::Matcher;
use crate::parse::Parser;
use crate::{Keyword, TokenTree};
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
    pub where_clause: Option<WhereClause<A>>,

    pub tags: Vec<Tag<A>>,
    
    pub packed: bool,
    
    pub forward: bool,

    pub sections: Vec<StructDeclSection<A>>,
    
    pub implements: Option<SupersClause<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub end_kw_span: Option<Span>,
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

    pub fn find_field_decl(&self, str: &str) -> Option<(&FieldDecl<A>, usize)> {
        self.fields().find_map(|field_decl| {
            let pos = field_decl.idents.iter().position(|f| f.as_str() == str)?;
            
            Some((field_decl, pos))
        })
    }

    pub fn implements_types(&self) -> &[A::TypeName] {
        match &self.implements {
            Some(implements) => &implements.types,
            None => &[],
        }
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
    pub fn parse(
        parser: &mut Parser,
        name: DeclIdent,
        tags: Vec<Tag>,
        keyword_token: TokenTree,
    ) -> Self {
        let packed_kw = parser.tokens().match_one_maybe(Keyword::Packed);
        let packed = packed_kw.is_some();
        
        let match_kw = if packed {
            Matcher::from(Keyword::Record)
        } else {
            Keyword::Record | Keyword::Class
        };

        let header = TypeDeclHeader::parse_or_empty(
            parser,
            match_kw,
            keyword_token,
            &tags,
            &name.span,
        );

        let kind = match &header.keyword {
            tt if tt.is_keyword(Keyword::Class) => StructKind::Class,
            tt if tt.is_keyword(Keyword::Record) => StructKind::Record,
            _ => unreachable!(),
        };
        
        let kw_span = header.keyword.into_span();

        let span = match packed_kw {
            Some(tt) => tt.span().to(&header.span),
            None => header.span,
        };
        
        // the last type in a section can never be forward, so every legal forward declaration
        // will end with a semicolon
        if header.forward {
            StructDecl {
                kw_span,
                kind,
                name,
                where_clause: header.where_clause,
                packed,
                tags: Vec::new(),

                forward: true,
                implements: header.supers,
                
                sections: Vec::new(),
                span,
                end_kw_span: None,
            }
        } else {
            let default_access = match kind {
                StructKind::Class => Access::Private,
                StructKind::Record => Access::Public,
            };

            let sections = parse_struct_sections(parser, default_access);

            let (span, end_kw_span) = match parser.advance_to(Keyword::End) {
                None => (span, None),
                Some(end_span) => (span.to(&end_span), Some(end_span.into_span())),  
            };

            StructDecl {
                kw_span,
                kind,
                name,
                where_clause: header.where_clause,
                packed,
                tags,
                forward: false,
                implements: header.supers,
                sections,
                span,
                end_kw_span,
            }
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
