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
use crate::ast::Annotation;
use crate::ast::FunctionDeclKind;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::Operator;
use crate::ast::TypeList;
use crate::ast::TypeName;
use crate::ast::TypeParam;
use crate::parse::InvalidTagLocation;
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::DelimiterPair;
use crate::Separator;
use common::span::Span;
use common::span::Spanned;
use common::TracedError;
use derivative::*;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct TypeDecl<A: Annotation = Span> {
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

        let span = kw_tt.span().to(last_item.span());

        Ok(TypeDecl { span, items })
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
    Struct(Rc<StructDecl<A>>),
    Interface(Rc<InterfaceDecl<A>>),
    Variant(Rc<VariantDecl<A>>),
    Alias(Rc<AliasDecl<A>>),
    Enum(Rc<EnumDecl<A>>),
    Set(Rc<SetDecl<A>>),
}

impl<A: Annotation> TypeDeclItem<A> {
    pub fn name(&self) -> &A::Name {
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

        tokens.match_one(Self::start_matcher()).is_some()
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
pub struct TypeDeclName {
    pub ident: Ident,
    pub type_params: Option<TypeList<TypeParam<TypeName>>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl fmt::Display for TypeDeclName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;

        if let Some(type_params) = &self.type_params {
            write!(f, "{}", type_params)?;
        }

        Ok(())
    }
}

impl Spanned for TypeDeclName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl From<Ident> for TypeDeclName {
    fn from(ident: Ident) -> Self {
        TypeDeclName {
            span: ident.span().clone(),
            ident,
            type_params: None,
        }
    }
}

impl TypeDeclName {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let ident = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();
        
        let type_params = TypeList::try_parse_type_params(tokens)?
            .map(|list| {
                list.map(|name, _pos| TypeParam {
                    name,
                    
                    // in the context of a name on its own, type params are parsed without 
                    // constraints, which we need to parse later and add to this decl if needed
                    constraint: None,
                })
            });
        

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

        let name = TypeDeclName::parse(tokens)?;
        tokens.match_one(Operator::Equals)?;

        let struct_kw_matcher = Keyword::Packed | Keyword::Class | Keyword::Record;
        let decl_start_matcher = struct_kw_matcher.clone().or(Keyword::Variant);

        match tokens.look_ahead().next() {
            Some(ref tt) if struct_kw_matcher.is_match(tt) => {
                let struct_decl = StructDecl::parse(tokens, name, tags)?;
                Ok(TypeDeclItem::Struct(Rc::new(struct_decl)))
            },

            Some(tt) if tt.is_keyword(Keyword::Interface) => {
                let iface_decl = InterfaceDecl::parse(tokens, name, tags)?;
                Ok(TypeDeclItem::Interface(Rc::new(iface_decl)))
            },

            Some(tt) if tt.is_keyword(Keyword::Variant) => {
                let variant_decl = VariantDecl::parse(tokens, name, tags)?;
                Ok(TypeDeclItem::Variant(Rc::new(variant_decl)))
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
                Ok(TypeDeclItem::Enum(Rc::new(enum_decl)))
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
                Ok(TypeDeclItem::Set(Rc::new(set_decl)))
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
                Ok(TypeDeclItem::Alias(Rc::new(alias_decl)))
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
