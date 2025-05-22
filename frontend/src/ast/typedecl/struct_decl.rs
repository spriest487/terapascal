mod member;

use crate::ast::tag::Tag;
use crate::ast::Access;
use crate::ast::Annotation;
use crate::ast::Ident;
use crate::ast::MethodOwner;
use crate::ast::TypeDeclName;
use crate::ast::{parse_implements_clause, WhereClause};
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::parse::{ParseError, TryParse};
use crate::Keyword;
use crate::Separator;
use common::span::Span;
use common::span::Spanned;
use derivative::*;
pub use member::*;
use std::fmt;

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
    pub name: A::Name,
    pub where_clause: Option<WhereClause<A::Type>>,

    pub tags: Vec<Tag<A>>,
    
    pub packed: bool,
    
    pub forward: bool,

    pub fields: Vec<FieldDecl<A>>,
    pub methods: Vec<MethodDecl<A>>,
    
    pub implements: Vec<A::Type>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> StructDecl<A> {    
    pub fn find_field(&self, by_ident: &Ident) -> Option<&FieldDecl<A>> {
        self.fields.iter().find(|field| field.ident == *by_ident)
    }
    
    pub fn fields(&self) -> impl Iterator<Item=&FieldDecl<A>> {
        self.fields.iter()
    }

    pub fn methods(&self) -> impl Iterator<Item=&MethodDecl<A>> {
        self.methods.iter()
    }
}

impl<A: Annotation> MethodOwner<A> for StructDecl<A> {
    fn methods(&self) -> &[MethodDecl<A>] {
        self.methods.as_slice()
    }
} 

impl StructDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: TypeDeclName, tags: Vec<Tag>) -> ParseResult<Self> {
        let packed_kw = tokens.match_one_maybe(Keyword::Packed);
        let packed = packed_kw.is_some();
        
        let kw_token = if packed {
            tokens.match_one(Keyword::Record)?
        } else {
            tokens.match_one(Keyword::Record | Keyword::Class)?
        };

        let kind = match &kw_token {
            tt if tt.is_keyword(Keyword::Class) => StructKind::Class,
            tt if tt.is_keyword(Keyword::Record) => StructKind::Record,
            _ => unreachable!(),
        };

        let span = match packed_kw {
            Some(tt) => tt.span().to(kw_token.span()),
            None => kw_token.into_span(),
        };
        
        // the last type in a section can never be forward, so every legal forward declaration
        // will end with a semicolon
        if tokens.look_ahead().match_one(Separator::Semicolon).is_some() {
            if !tags.is_empty() {
                return Err(ParseError::forward_decl_tags(name.span, &tags).into());
            }
            
            Ok(StructDecl {
                kind,
                name,
                where_clause: None,
                packed,
                tags: Vec::new(),

                forward: true,
                implements: Vec::new(),
                
                methods: Vec::new(),
                fields: Vec::new(),
                span,
            })
        } else {
            let implements = parse_implements_clause(tokens)?;

            let where_clause = WhereClause::try_parse(tokens)?;
            
            let default_access = match kind {
                StructKind::Class => Access::Private,
                StructKind::Record => Access::Public,
            };

            let members = parse_struct_members(tokens, default_access)?;
            
            let mut methods = Vec::new();
            let mut fields = Vec::new();
            for member in members {
                match member {
                    StructMemberDecl::Field(field) => fields.push(field),
                    StructMemberDecl::MethodDecl(method) => methods.push(method),
                }
            }

            let end_token = tokens.match_one(Keyword::End)?;

            Ok(StructDecl {
                kind,
                name,
                where_clause,
                packed,
                tags,
                forward: false,
                implements,
                fields,
                methods,
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
        
        let mut access = Access::Public;
        
        for field in &self.fields {
            write_access_if_changed(f, &mut access, field.access)?;
            writeln!(f, "  {};", field)?
        }

        for method in &self.methods {
            write_access_if_changed(f, &mut access, method.access)?;
            writeln!(f, "  {};", method.func_decl)?
        }

        write!(f, "end")
    }
}
