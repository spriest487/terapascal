mod enum_decl;
mod iface_decl;
mod struct_decl;
mod variant_decl;
mod set_decl;
mod access;

pub use self::access::Access;
pub use self::access::IFACE_METHOD_ACCESS;
pub use self::enum_decl::*;
pub use self::iface_decl::*;
pub use self::set_decl::*;
pub use self::struct_decl::*;
pub use self::variant_decl::*;
use crate::ast::tag::Tag;
use crate::ast::unit::AliasDecl;
use crate::ast::FunctionDeclKind;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::Operator;
use crate::ast::TypeList;
use crate::ast::TypeName;
use crate::ast::WhereClause;
use crate::ast::{Annotation, DeclName};
use crate::parse::InvalidTagLocation;
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::parse::TryParse;
use crate::DelimiterPair;
use crate::Separator;
use crate::TokenTree;
use derivative::*;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct TypeDecl<A: Annotation = Span> {
    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub kw_span: Span,
    
    pub items: Vec<TypeDeclItem<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl Parse for TypeDecl<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw_tt = tokens.match_one(Keyword::Type)?;

        let items = TypeDeclItem::parse_seq(tokens)?;

        let last_item = items.last().ok_or_else(|| {
            TracedError::trace(ParseError::EmptyTypeDecl {
                span: kw_tt.clone().into_span(),
            })
        })?;

        let kw_span = kw_tt.into_span();
        let span = kw_span.to(last_item.span());

        Ok(TypeDecl {
            kw_span,
            items,
            span, 
        })
    }
}

impl<A: Annotation> Spanned for TypeDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for TypeDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "type")?;
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f, ";")?;
            }
            write!(f, "\t{}", item)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeDeclItem<A: Annotation = Span> {
    Struct(Arc<StructDecl<A>>),
    Interface(Arc<InterfaceDecl<A>>),
    Variant(Arc<VariantDecl<A>>),
    Alias(Arc<AliasDecl<A>>),
    Enum(Arc<EnumDecl<A>>),
    Set(Arc<SetDecl<A>>),
}

impl<A: Annotation> TypeDeclItem<A> {
    pub fn constraint_clause(&self) -> Option<&WhereClause<A::Type>> {
        match self {
            TypeDeclItem::Struct(def) => def.where_clause.as_ref(),
            
            // TODO
            TypeDeclItem::Interface(_def) => None,
            TypeDeclItem::Variant(_def) => None,
            
            TypeDeclItem::Alias(_)
            | TypeDeclItem::Enum(_)
            | TypeDeclItem::Set(_) => None,
        }
    }
}

impl<A: Annotation> TypeDeclItem<A> {
    pub fn name(&self) -> &A::DeclName {
        match self {
            TypeDeclItem::Struct(class) => &class.name,
            TypeDeclItem::Interface(iface) => &iface.name,
            TypeDeclItem::Variant(variant) => &variant.name,
            TypeDeclItem::Alias(alias) => &alias.name,
            TypeDeclItem::Enum(enum_decl) => &enum_decl.name,
            TypeDeclItem::Set(set_decl) => &set_decl.name,
        }
    }

    pub fn start_matcher() -> Matcher {
        // ident or tag group
        Matcher::AnyIdent | DelimiterPair::SquareBracket
    }
}

impl ParseSeq for TypeDeclItem<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        TypeDeclItem::parse(tokens)
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        // skip past tags, because a valid function decl following a type block could start with
        // any number of these
        loop {
            match tokens.next() {
                Some(tt) if tt.is_delimited(DelimiterPair::SquareBracket) => continue,
                Some(TokenTree::Ident(..)) => break true,
                _ => break false,
            }
        }
    }
}

pub fn iface_method_start() -> Matcher {
    Keyword::Function 
        | Keyword::Procedure
        | DelimiterPair::SquareBracket //tags
}

pub fn type_method_start() -> Matcher {
    iface_method_start() 
        | Keyword::Class 
        | Keyword::Constructor 
        | Keyword::Destructor 
}

/// the common part of a typedecl before the `=`, eg in `type X[Y] = class...`, `X<Y>` is the decl
/// name. we parse it first and pass it into the parse functions for specific decl kinds.
/// this isn't quite the same thing as a TypeName, which can be a full qualified path - a decl
/// name is a single unqualified ident + maybe a type parameter list
#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct DeclIdent {
    pub ident: Ident,
    pub type_params: Option<TypeList<Ident>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl fmt::Display for DeclIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;

        if let Some(type_params) = &self.type_params {
            write!(f, "{}", type_params)?;
        }

        Ok(())
    }
}

impl DeclName for DeclIdent {
    fn ident(&self) -> &Ident {
        &self.ident
    }

    fn type_params_len(&self) -> usize {
        self.type_params.as_ref().map(|list| list.len()).unwrap_or(0)
    }

    fn type_param_name_span(&self, index: usize) -> Option<&Span> {
        let param = self.type_params.as_ref()?.items.get(index)?;
        Some(&param.span)
    }
}

impl Spanned for DeclIdent {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl From<Ident> for DeclIdent {
    fn from(ident: Ident) -> Self {
        DeclIdent {
            span: ident.span().clone(),
            ident,
            type_params: None,
        }
    }
}

impl DeclIdent {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let ident = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();

        let type_params = TypeList::try_parse_type_params(tokens)?;

        let span = match &type_params {
            Some(type_param_list) => ident.span().to(type_param_list.span()),
            None => ident.span().clone(),
        };

        Ok(Self {
            ident,
            type_params,
            span,
        })
    }
}

impl Parse for TypeDeclItem<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let tags = Tag::parse_seq(tokens)?;

        let name = DeclIdent::parse(tokens)?;
        tokens.match_one(Operator::Equals)?;

        let struct_kw_matcher = Keyword::Packed | Keyword::Class | Keyword::Record;
        let decl_start_matcher = struct_kw_matcher.clone().or(Keyword::Variant);

        match tokens.look_ahead().next() {
            Some(ref tt) if struct_kw_matcher.is_match(tt) => {
                let struct_decl = StructDecl::parse(tokens, name, tags)?;
                Ok(TypeDeclItem::Struct(Arc::new(struct_decl)))
            },

            Some(tt) if tt.is_keyword(Keyword::Interface) => {
                let iface_decl = InterfaceDecl::parse(tokens, name, tags)?;
                Ok(TypeDeclItem::Interface(Arc::new(iface_decl)))
            },

            Some(tt) if tt.is_keyword(Keyword::Variant) => {
                let variant_decl = VariantDecl::parse(tokens, name, tags)?;
                Ok(TypeDeclItem::Variant(Arc::new(variant_decl)))
            },
            
            Some(tt) if tt.is_delimited(DelimiterPair::Bracket) => {
                if !tags.is_empty() {
                    return Err(ParseError::invalid_tag_loc(
                        InvalidTagLocation::EnumDecl,
                        tt.span().clone(),
                        &tags
                    ).into());
                }
                
                let enum_decl = EnumDecl::parse(name, tokens)?;
                Ok(TypeDeclItem::Enum(Arc::new(enum_decl)))
            },

            Some(tt) if tt.is_keyword(Keyword::Set) => {
                if !tags.is_empty() {
                    return Err(ParseError::invalid_tag_loc(
                        InvalidTagLocation::SetDecl,
                        tt.span().clone(),
                        &tags
                    ).into());
                }
                
                let set_decl = SetDecl::parse(name, tokens)?;
                Ok(TypeDeclItem::Set(Arc::new(set_decl)))
            }

            // if it isn't a type def keyword, then it must be the name of an existing type to
            // declare an alias
            Some(tt) => {
                if !tags.is_empty() {
                    return Err(ParseError::invalid_tag_loc(
                        InvalidTagLocation::AliasDecl,
                        tt.span().clone(),
                        &tags
                    ).into());
                }

                let alias_decl = AliasDecl::parse(tokens, name)?;
                Ok(TypeDeclItem::Alias(Arc::new(alias_decl)))
            },

            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                decl_start_matcher.clone(),
                tokens.context().clone(),
            ))),
        }
    }
}

impl<A: Annotation> Spanned for TypeDeclItem<A> {
    fn span(&self) -> &Span {
        match self {
            TypeDeclItem::Struct(class) => class.span(),
            TypeDeclItem::Interface(iface) => iface.span(),
            TypeDeclItem::Variant(variant) => variant.span(),
            TypeDeclItem::Alias(alias) => alias.span(),
            TypeDeclItem::Enum(enum_decl) => enum_decl.span(),
            TypeDeclItem::Set(set_decl) => set_decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for TypeDeclItem<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDeclItem::Struct(class) => write!(f, "{}", class),
            TypeDeclItem::Interface(iface) => write!(f, "{}", iface),
            TypeDeclItem::Variant(variant) => write!(f, "{}", variant),
            TypeDeclItem::Alias(alias) => write!(f, "{}", alias),
            TypeDeclItem::Enum(enum_decl) => write!(f, "{}", enum_decl),
            TypeDeclItem::Set(set_decl) => write!(f, "{}", set_decl),
        }
    }
}

#[derive(Debug, Clone)]
struct TypeDeclStart {
    keyword: TokenTree,

    where_clause: Option<WhereClause>,
    supers: Vec<TypeName>,
    
    forward: bool,
    
    span: Span,
}

impl TypeDeclStart {
    pub fn parse(
        tokens: &mut TokenStream,
        kw_matcher: impl Into<Matcher>,
        tags: &[Tag],
        name_span: &Span,
    ) -> ParseResult<Self> {
        let kw_tt = tokens.match_one(kw_matcher)?;
        let mut span = kw_tt.span().clone();
        
        // a forward decl can have a where clause before the semicolon
        let where_clause = WhereClause::try_parse(tokens)?;
        
        if let Some(where_span) = where_clause.as_ref().map(|x|x.span()) {
            span = span.to(where_span);
        }
        
        let mut result = Self {
            keyword: kw_tt,
            where_clause,
            forward: true,
            supers: Vec::new(),
            span,
        };

        // is this a forward decl?
        match tokens.look_ahead().match_one(Separator::Semicolon) {
            None => {
                result.forward = false;

                // for non-forward decls, the optional supers clause might appear before the
                // where clause, in which case there may still be a where clause afterwards
                if result.where_clause.is_none() {
                    result.supers = parse_implements_clause(tokens)?;
                    
                    if !result.supers.is_empty() {
                        let super_last_span = result.supers[result.supers.len() - 1].span();
                        result.span = result.span.to(super_last_span);
                    }
                    
                    if let Some(where_clause) = WhereClause::try_parse(tokens)? {
                        result.span = result.span.to(where_clause.span());
                        result.where_clause = Some(where_clause);
                    }
                }
            }

            Some(tt) => {
                if !tags.is_empty() {
                    // forward type decls can't have tags
                    return Err(ParseError::forward_decl_tags(name_span.clone(), &tags).into())
                }
                
                result.span = result.span.to(tt.span());
            }
        }
        
        Ok(result)
    }    
}


pub fn parse_implements_clause(
    tokens: &mut TokenStream,
) -> ParseResult<Vec<TypeName>> {
    let mut implements = Vec::new();

    if tokens.match_one_maybe(Keyword::Of).is_some() {
        loop {
            let implement_iface = TypeName::parse(tokens)?;

            implements.push(implement_iface);

            if tokens.match_one_maybe(Separator::Comma).is_none() {
                break;
            }
        }
    }
    
    Ok(implements)
}

pub trait MethodOwner<A: Annotation> {
    fn methods(&self) -> &[MethodDecl<A>];

    fn find_methods<'a>(
        &'a self, 
        ident: &'a Ident
    ) -> impl Iterator<Item=(usize, &'a MethodDecl<A>)> 
    where 
        A: 'a 
    {
        self.methods()
            .iter()
            .enumerate()
            .filter(move |(_, m)| m.func_decl.ident() == ident)
    }

    fn find_dtor_index(&self) -> Option<usize> {
        self.methods()
            .iter()
            .position(|m| m.func_decl.kind == FunctionDeclKind::Destructor)
    }
}
