mod ty_path;
mod function_name;

pub use self::function_name::FunctionTypeName;
pub use self::function_name::FunctionTypeNameParam;
pub use self::ty_path::TypePath;
use crate::ast::Expr;
use crate::ast::IdentPath;
use crate::ast::SemanticHint;
use crate::ast::TypeAnnotation;
use crate::ast::TypeArgList;
use crate::ast::TypeList;
use crate::parse::LookAheadTokenStream;
use crate::parse::Match;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::DelimiterPair;
use crate::ast::Ident;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use crate::TokenTree;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::{MaybeSpanned, Span};
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

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

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub of_kw: Span,
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

#[derive(Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub enum TypeName {
    /// type is unknown or unnamed at parse time
    Unspecified,

    Ident(IdentTypeName),
    Array(ArrayTypeName),
    
    Weak(
        Box<TypeName>,
        #[derivative(Debug = "ignore")]
        #[derivative(Hash = "ignore")]
        #[derivative(PartialEq = "ignore")]
        Span
    ),

    Function(FunctionTypeName),
}

impl MaybeSpanned for TypeName {
    fn get_span(&self) -> Option<&Span> {
        match self {
            TypeName::Ident(i) => Some(i.span()),
            TypeName::Array(a) => Some(a.span()),
            TypeName::Unspecified => None,
            TypeName::Function(f) => Some(f.span()),
            TypeName::Weak(_, span) => Some(span),
        }
    }
}

impl TypeAnnotation for TypeName {
    fn is_known(&self) -> bool {
        match self {
            TypeName::Unspecified => false,
            _ => true,
        }
    }

    fn semantic_hint(&self) -> SemanticHint {
        SemanticHint::Type
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

        match tokens.look_ahead().match_one(Self::start_matcher()) {
            Some(TokenTree::Ident(..)) => Self::parse_named_type(tokens, indirection, indirection_span.as_ref()),

            Some(array_kw) if array_kw.is_keyword(Keyword::Array) => {
                let kw_span = array_kw.span().clone();
                tokens.advance(1);
                Self::parse_array_type(tokens, &kw_span, indirection, indirection_span)
            },

            Some(fn_kw)
                if fn_kw.is_keyword(Keyword::Function) || fn_kw.is_keyword(Keyword::Procedure) =>
            {
                let kw_span = fn_kw.span().clone();
                tokens.advance(1);
                Self::parse_function_type(tokens, kw_span, indirection, indirection_span)
            },
            
            Some(weak_kw) if weak_kw.is_keyword(Keyword::Weak) => {
                let mut span = weak_kw.span().clone();
                
                tokens.advance(1);
                let weak_ty = Self::parse(tokens)?;
                
                // a type can't be "weak weak"
                if let Some(next_weak) = tokens.look_ahead().match_one(Keyword::Weak) {
                    let expected = Some(start_non_weak_matcher());
                    let err = ParseError::UnexpectedToken(Box::new(next_weak.clone()), expected);
                    return Err(TracedError::trace(err))
                }

                span.maybe_extend(&weak_ty);
                
                Ok(TypeName::Weak(Box::new(weak_ty), span))
            }

            Some(bad) => {
                unreachable!("matcher should exclude {}", bad)
            }
            
            None => {
                tokens
                    .match_one(Self::start_matcher())
                    .map(|_| unreachable!("will never succeed"))
            }
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

fn start_non_weak_matcher() -> Matcher {
    Keyword::Array | Keyword::Function | Keyword::Procedure | Matcher::AnyIdent
}

impl TypeName {
    pub fn start_matcher() -> Matcher {
        start_non_weak_matcher() | Keyword::Weak
    }

    fn parse_array_type(
        tokens: &mut TokenStream,
        array_kw_span: &Span,
        indirection: usize,
        indirection_span: Option<Span>,
    ) -> ParseResult<Self> {
        let mut span = array_kw_span.clone();

        // `array of` means the array is dynamic (no dimension)
        let dim = match tokens.look_ahead().match_one(Keyword::Of) {
            Some(_) => None,

            None => match tokens.match_one(Matcher::Delimited(DelimiterPair::SquareBracket))? {
                TokenTree::Delimited(group) => {
                    span.extend(&group.close);

                    let mut dim_tokens = group.into_inner_tokens();
                    let dim_expr = Expr::parse(&mut dim_tokens)?;
                    dim_tokens.finish()?;

                    Some(Box::new(dim_expr))
                },

                _ => unreachable!("match failed"),
            },
        };

        let of_kw = tokens.match_one(Keyword::Of)?.into_span();
        span.extend(&of_kw);

        let element = Self::parse(tokens)?;
        if let Some(element_span) = element.get_span() {
            span.extend(element_span);
        }
        
        if let Some(indirection_span) = indirection_span {
            span.extend(&indirection_span);
        }

        Ok(TypeName::Array(ArrayTypeName {
            dim,
            span,
            indirection,
            of_kw,
            element: Box::new(element),
        }))
    }

    fn parse_function_type(
        tokens: &mut TokenStream,
        kw_span: Span,
        indirection: usize,
        indirection_span: Option<Span>,
    ) -> ParseResult<Self> {
        let mut span = indirection_span.unwrap_or_else(|| kw_span.clone());

        let params = match tokens.match_one_maybe(DelimiterPair::Bracket) {
            Some(TokenTree::Delimited(group)) => {
                span.extend(&group.close);

                let mut params_tokens = group.into_inner_tokens();
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
                span.maybe_extend(&return_ty);

                Some(Box::new(return_ty))
            },
            None => None,
        };

        let func_ty_name = FunctionTypeName {
            indirection,
            params,
            span,
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
            TypeName::Weak(type_name, ..) => write!(f, "weak {}", type_name),
            TypeName::Unspecified => write!(f, "<unknown type>"),
        }
    }
}
