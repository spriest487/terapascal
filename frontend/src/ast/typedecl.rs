mod enum_decl;
mod iface_decl;
mod struct_decl;
mod variant_decl;
mod set_decl;
mod access;
mod supers_clause;

pub use self::access::Access;
pub use self::access::IFACE_METHOD_ACCESS;
pub use self::enum_decl::*;
pub use self::iface_decl::*;
pub use self::set_decl::*;
pub use self::struct_decl::*;
pub use self::supers_clause::*;
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
use crate::parse::{ContinueParse, InvalidTagLocation, Parser};
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::parse::TryParse;
use crate::parse::LookAheadTokenStream;
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

impl TypeDecl {
    pub fn parse(parser: &mut Parser) -> ParseResult<Self> {
        let kw_tt = parser.tokens().match_one(Keyword::Type)?;

        let mut items = Vec::new();

        while TypeDeclItem::has_more(&items, &mut parser.tokens().look_ahead()) {
            if let Some(item) = TypeDeclItem::parse_item(&items, parser)
                .ok_or_continue(parser.errors()) 
            {
                items.push(item);
            }
        }

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
    pub fn constraint_clause(&self) -> Option<&WhereClause<A>> {
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

    pub fn tags(&self) -> &[Tag<A>] {
        match self {
            TypeDeclItem::Struct(class) => &class.tags,
            TypeDeclItem::Interface(iface) => &iface.tags,
            TypeDeclItem::Variant(variant) => &variant.tags,
            
            TypeDeclItem::Alias(..)
            | TypeDeclItem::Enum(..)
            | TypeDeclItem::Set(..) => &[],
        }
    }

    pub fn start_matcher() -> Matcher {
        // ident or tag group
        Matcher::AnyIdent | DelimiterPair::SquareBracket
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

impl TypeDeclItem {
    fn parse(parser: &mut Parser) -> ParseResult<Self> {
        let tags = Tag::parse_seq(parser.tokens())
            .or_continue_with(parser.errors(), Vec::new);

        // name and the initial keyword are enough to continue parsing the type, even if there's
        // parse errors in the rest of the type header, so we shouldn't return an error after this
        // point if possible
        let name = DeclIdent::parse(parser.tokens())?;
        parser.tokens().match_one(Operator::Equals)?;

        let struct_kw_matcher = Keyword::Packed | Keyword::Class | Keyword::Record;
        let decl_kw_matcher = struct_kw_matcher.clone() | Keyword::Variant | Keyword::Interface;
        
        // get the first token from lookahead, since each kind of decl expects to start
        // parsing from the keyword
        let Some(first_token) = parser
            .tokens()
            .look_ahead()
            .next()
            .cloned()
        else {
            return Err(TracedError::trace(ParseError::UnexpectedEOF(
                decl_kw_matcher.clone(),
                parser.tokens().context().clone(),
            )));
        };
        
        match first_token.as_keyword() {
            Some(Keyword::Packed | Keyword::Record | Keyword::Class) => {
                let struct_decl = StructDecl::parse(parser, name, tags, first_token);
                Ok(TypeDeclItem::Struct(Arc::new(struct_decl)))
            }

            Some(Keyword::Variant) => {
                let variant_decl = VariantDecl::parse(parser, name, tags, first_token);
                Ok(TypeDeclItem::Variant(Arc::new(variant_decl)))
            }
            
            Some(Keyword::Interface) => {
                let iface_decl = InterfaceDecl::parse(parser, name, tags, first_token)
                    .map_err(|err| {
                        parser.tokens().advance_to(Keyword::End).and_continue(parser.errors());
                        err
                    })?;

                Ok(TypeDeclItem::Interface(Arc::new(iface_decl)))
            }
            
            Some(Keyword::Set) => {
                if !tags.is_empty() {
                    return Err(ParseError::invalid_tag_loc(
                        InvalidTagLocation::SetDecl,
                        first_token.span().clone(),
                        &tags
                    ).into());
                }

                let set_decl = SetDecl::parse(name, parser.tokens())?;
                Ok(TypeDeclItem::Set(Arc::new(set_decl)))
            }
            
            _ if first_token.is_delimited(DelimiterPair::Bracket) => {
                if !tags.is_empty() {
                    return Err(ParseError::invalid_tag_loc(
                        InvalidTagLocation::EnumDecl,
                        first_token.span().clone(),
                        &tags
                    ).into());
                }

                let enum_decl = EnumDecl::parse(name, parser.tokens())?;
                Ok(TypeDeclItem::Enum(Arc::new(enum_decl)))
            }
            
            _ => {
                // if it isn't a type def keyword, then it must be the name of an existing type to
                // declare an alias
                if !tags.is_empty() {
                    return Err(ParseError::invalid_tag_loc(
                        InvalidTagLocation::AliasDecl,
                        first_token.span().clone(),
                        &tags
                    ).into());
                }

                let alias_decl = AliasDecl::parse(parser.tokens(), name)?;
                Ok(TypeDeclItem::Alias(Arc::new(alias_decl)))
            }
        }
    }

    fn parse_item(prev: &[Self], parser: &mut Parser) -> ParseResult<Self> {
        if !prev.is_empty() {
            parser.tokens().advance_to(Separator::Semicolon).and_continue(parser.errors());
        }

        TypeDeclItem::parse(parser)
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
struct TypeDeclHeader {
    keyword: TokenTree,

    where_clause: Option<WhereClause>,
    supers: Option<SupersClause>,

    forward: bool,
    
    span: Span,
}

impl TypeDeclHeader {
    pub fn new(keyword_token: TokenTree) -> Self {
        Self {
            span: keyword_token.span().clone(),
            keyword: keyword_token,
            where_clause: None,
            supers: None,
            forward: false,
        }
    }
    
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
            supers: None,
            span,
        };

        // is this a forward decl?
        match tokens.look_ahead().match_one(Separator::Semicolon) {
            None => {
                result.forward = false;

                // for non-forward decls, the optional supers clause might appear before the
                // where clause, in which case there may still be a where clause afterwards
                if result.where_clause.is_none() {
                    result.supers = SupersClause::parse(tokens)?;

                    if let Some(supers) = &result.supers {
                        result.span.extend(&supers.span);
                    }
                    
                    if let Some(where_clause) = WhereClause::try_parse(tokens)? {
                        result.span.extend(where_clause.span());
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
    
    pub fn parse_or_empty(
        parser: &mut Parser,
        kw_matcher: impl Into<Matcher>,
        keyword_token: TokenTree,
        tags: &[Tag],
        name_span: &Span
    ) -> Self {
        let kw_matcher = kw_matcher.into();
        
        Self::parse(parser.tokens(), kw_matcher, tags, name_span)
            .or_continue_with(parser.errors(), || {
                Self::new(keyword_token)
            })
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
    fn methods<'a>(&'a self) -> impl Iterator<Item=&'a MethodDecl<A>>
    where A: 'a;

    fn find_methods<'a>(
        &'a self, 
        ident: &'a Ident
    ) -> impl Iterator<Item=(usize, &'a MethodDecl<A>)> 
    where 
        A: 'a 
    {
        self.methods()
            .enumerate()
            .filter(move |(_, m)| m.func_decl.ident() == ident)
    }

    fn find_dtor_index(&self) -> Option<usize> {
        self.methods()
            .position(|m| m.func_decl.kind == FunctionDeclKind::Destructor)
    }
}
