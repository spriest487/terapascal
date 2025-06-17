use crate::ast::type_name::IdentTypeName;
use crate::ast::type_name::TypeName;
use crate::ast::IdentPath;
use crate::ast::Keyword;
use crate::ast::{Annotation, Ident};
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::parse::TryParse;
use crate::Separator;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct TypeConstraint<A: Annotation = Span> {
    pub name: Ident,

    pub is_ty: A::TypeName,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub is_kw_span: Option<Span>,
    
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> fmt::Display for TypeConstraint<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} is {}", self.name, self.is_ty)
    }
}

impl<A: Annotation> Spanned for TypeConstraint<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct WhereClause<A: Annotation = Span> {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub where_kw_span: Span,
    
    pub constraints: Vec<TypeConstraint<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> fmt::Display for WhereClause<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "where ")?;
        for (i, constraint) in self.constraints.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", constraint)?;
        }
        Ok(())
    }
}

impl TryParse for WhereClause<Span> {
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        match tokens.look_ahead().match_one(Keyword::Where) {
            None => Ok(None),
            Some(..) => Self::parse(tokens).map(Some),
        }
    }
}

impl<A: Annotation> Spanned for WhereClause<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl WhereClause<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let where_kw = tokens.match_one(Keyword::Where)?;

        let constraints: Vec<_> = WhereClauseItem::parse_seq(tokens)?
            .into_iter()
            .map(|i| i.0)
            .collect();

        let span = constraints
            .iter()
            .fold(where_kw.span().clone(), |span, constraint| {
                span.to(constraint.span())
            });

        let clause = Self {
            where_kw_span: where_kw.into_span(),
            constraints,
            span
        };

        if clause.constraints.is_empty() {
            Err(TracedError::trace(ParseError::EmptyWhereClause(clause)))
        } else {
            Ok(clause)
        }
    }
}

struct WhereClauseItem(TypeConstraint<Span>);

impl ParseSeq for WhereClauseItem {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if prev.len() > 0 {
            tokens.match_one(Separator::Semicolon)?;
        }

        let param_ident = Ident::parse(tokens)?;
        let is_kw_tt = tokens.match_one(Keyword::Is)?;

        let is_ty_path = IdentPath::parse(tokens)?;

        let is_ty = TypeName::Ident(IdentTypeName {
            span: is_ty_path.span().clone(),
            type_args: None,
            ident: is_ty_path,
            indirection: 0,
        });

        Ok(WhereClauseItem(TypeConstraint {
            span: param_ident.span().to(is_ty.span()),
            name: param_ident,
            is_ty,
            is_kw_span: Some(is_kw_tt.into_span()),
        }))
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_sequence(Matcher::AnyIdent + Keyword::Is).is_some()
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq, Debug)]
pub struct TypeParam<A: Annotation = Span> {
    pub name: Ident,
    pub constraint: Option<TypeConstraint<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for TypeParam<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> TypeParam<A> {
    pub fn new(name: Ident) -> Self {
        Self {
            span: name.span.clone(),
            name,
            constraint: None,
        }
    }
    
    pub fn with_constraint(name: Ident, constraint: TypeConstraint<A>) -> Self {
        Self {
            span: name.span.to(constraint.span()),
            name,
            constraint: Some(constraint),
        }
    }
}

impl<A: Annotation> fmt::Display for TypeParam<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(constraint) = &self.constraint {
            write!(f, " is {}", constraint.is_ty)?;
        }

        Ok(())
    }
}
