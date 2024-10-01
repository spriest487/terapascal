mod ty_path;
mod function_name;

pub use self::function_name::FunctionTypeName;
pub use self::function_name::FunctionTypeNameParam;
pub use self::ty_path::TypePath;
use crate::ast::{Expr, TypeArgList};
use crate::ast::IdentPath;
use crate::ast::TypeAnnotation;
use crate::ast::TypeList;
use crate::parse::LookAheadTokenStream;
use crate::parse::Match;
use crate::parse::MatchOneOf;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::DelimiterPair;
use crate::Ident;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use crate::TokenTree;
use common::span::Span;
use common::span::Spanned;
use derivative::Derivative;
use std::fmt;
use std::hash::Hash;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub struct IdentTypeName {
    pub ident: IdentPath,
    pub type_args: Option<TypeArgList>,
    pub indirection: usize,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl IdentTypeName {
    pub fn is_single_ident(&self) -> bool {
        self.ident.len() == 1 && self.indirection == 0 && self.type_args.is_none()
    }
}

impl Spanned for IdentTypeName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for IdentTypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..self.indirection {
            write!(f, "^")?;
        }
        write!(f, "{}", self.ident)?;

        if let Some(type_args) = &self.type_args {
            write!(f, "[")?;
            for (i, arg) in type_args.items.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
            }
            write!(f, "]")?;
        }

        Ok(())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub struct ArrayTypeName {
    pub element: Box<TypeName>,
    pub dim: Option<Box<Expr<Span>>>,
    pub indirection: usize,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl Spanned for ArrayTypeName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for ArrayTypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.dim {
            Some(dim) => write!(f, "array[{}] of {}", dim, self.element),
            None => write!(f, "array of {}", self.element),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeName {
    /// type is unknown or unnamed at parse time
    Unspecified(Span),

    Ident(IdentTypeName),
    Array(ArrayTypeName),

    Function(FunctionTypeName),
}

impl Spanned for TypeName {
    fn span(&self) -> &Span {
        match self {
            TypeName::Ident(i) => i.span(),
            TypeName::Array(a) => a.span(),
            TypeName::Unspecified(span) => span,
            TypeName::Function(f) => f.span(),
        }
    }
}

impl TypeAnnotation for TypeName {
    fn is_known(&self) -> bool {
        match self {
            TypeName::Unspecified(_) => false,
            _ => true,
        }
    }
}

impl Parse for TypeName {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let mut indirection = 0;
        let mut indirection_span = None;

        while let Some(deref_tt) = tokens.match_one_maybe(Operator::Caret) {
            if indirection_span.is_none() {
                indirection_span = Some(deref_tt.span().clone());
            }
            indirection += 1;
        }

        match tokens.match_one_maybe(Keyword::Array.or(Keyword::Function).or(Keyword::Procedure)) {
            None => Self::parse_named_type(tokens, indirection, indirection_span.as_ref()),

            Some(array_kw) if array_kw.is_keyword(Keyword::Array) => {
                Self::parse_array_type(tokens, array_kw.span(), indirection, indirection_span)
            },

            Some(fn_kw)
                if fn_kw.is_keyword(Keyword::Function) || fn_kw.is_keyword(Keyword::Procedure) =>
            {
                Self::parse_function_type(tokens, fn_kw.into_span(), indirection, indirection_span)
            },

            Some(..) => unreachable!(),
        }
    }
}

impl From<Ident> for TypeName {
    fn from(value: Ident) -> Self {
        TypeName::Ident(IdentTypeName {
            span: value.span().clone(),
            ident: IdentPath::from(value),
            type_args: None,
            indirection: 0,
        })
    }
}

impl Match for TypeName {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool {
        tokens.match_one(Self::start_matcher()).is_some()
    }
}

impl TypeName {
    pub fn start_matcher() -> Matcher {
        Keyword::Array
            .or(Keyword::Function)
            .or(Keyword::Procedure)
            .or(Matcher::AnyIdent)
    }

    fn parse_array_type(
        tokens: &mut TokenStream,
        array_kw_span: &Span,
        indirection: usize,
        indirection_span: Option<Span>,
    ) -> ParseResult<Self> {
        // `array of` means the array is dynamic (no dimension)
        let dim = match tokens.look_ahead().match_one(Keyword::Of) {
            Some(_) => None,

            None => match tokens.match_one(Matcher::Delimited(DelimiterPair::SquareBracket))? {
                TokenTree::Delimited(group) => {
                    let mut dim_tokens = group.to_inner_tokens();
                    let dim_expr = Expr::parse(&mut dim_tokens)?;
                    dim_tokens.finish()?;

                    Some(Box::new(dim_expr))
                },

                _ => unreachable!("match failed"),
            },
        };

        tokens.match_one(Keyword::Of)?;

        let element = Self::parse(tokens)?;

        let array_span = array_kw_span.to(element.span());
        let span = match indirection_span {
            Some(indir_span) => indir_span.to(&array_span),
            None => array_span,
        };

        Ok(TypeName::Array(ArrayTypeName {
            dim,
            span,
            indirection,
            element: Box::new(element),
        }))
    }

    fn parse_function_type(
        tokens: &mut TokenStream,
        kw_span: Span,
        indirection: usize,
        indirection_span: Option<Span>,
    ) -> ParseResult<Self> {
        let span_begin = indirection_span.unwrap_or_else(|| kw_span.clone());
        let mut span_end = kw_span;

        let params = match tokens.match_one_maybe(DelimiterPair::Bracket) {
            Some(TokenTree::Delimited(group)) => {
                span_end = group.close.clone();

                let mut params_tokens = group.to_inner_tokens();
                let params = FunctionTypeNameParam::parse_seq(&mut params_tokens)?;
                params_tokens.finish()?;

                params
            },

            Some(..) => unreachable!(),

            None => Vec::new(),
        };

        let return_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(..) => {
                let return_ty = TypeName::parse(tokens)?;

                span_end = return_ty.span().clone();

                Some(Box::new(return_ty))
            },
            None => None,
        };

        let func_ty_name = FunctionTypeName {
            indirection,
            params,
            span: span_begin.to(&span_end),
            return_ty,
        };

        Ok(TypeName::Function(func_ty_name))
    }

    fn parse_named_type(
        tokens: &mut TokenStream,
        indirection: usize,
        indirection_span: Option<&Span>,
    ) -> ParseResult<Self> {
        let ident = IdentPath::parse(tokens)?;

        let (type_args, name_span) =
            match tokens.look_ahead().match_one(DelimiterPair::SquareBracket) {
                Some(..) => {
                    let type_args = TypeList::parse_type_args(tokens)?;
                    let name_span = ident.span().to(type_args.span());

                    (Some(type_args), name_span)
                },
                None => {
                    let name_span = ident.span().clone();
                    (None, name_span)
                },
            };

        let span = match indirection_span {
            Some(indir_span) => indir_span.to(&name_span),
            None => name_span,
        };

        Ok(TypeName::Ident(IdentTypeName {
            ident,
            indirection,
            type_args,
            span,
        }))
    }
    
    pub fn into_single_ident(self) -> Result<Ident, Self> {
        match self {
            TypeName::Ident(name) if name.is_single_ident() => {
                Ok(name.ident.into_vec().remove(0))
            }
            other => Err(other)
        }
    } 
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeName::Ident(ident_type_name) => write!(f, "{}", ident_type_name),
            TypeName::Array(array_type_name) => write!(f, "{}", array_type_name),
            TypeName::Function(func_type_name) => write!(f, "{}", func_type_name),
            TypeName::Unspecified(_) => write!(f, "<unknown type>"),
        }
    }
}
