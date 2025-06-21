use crate::ast::FunctionParamMod;
use crate::ast::TypeName;
use crate::parse::LookAheadTokenStream;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::ast::Ident;
use crate::Keyword;
use crate::Separator;
use crate::TokenStream;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub struct FunctionTypeName {
    pub params: Vec<FunctionTypeNameParam>,
    pub return_ty: Option<Box<TypeName>>,

    pub indirection: usize,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl Spanned for FunctionTypeName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for FunctionTypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function")?;
        
        if !self.params.is_empty() {
            write!(f, "(")?;
            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, "; ")?;
                }
                write!(f, "{}", param)?;
            }
            write!(f, ")")?;
        }

        if let Some(return_ty) = &self.return_ty {
            write!(f, ": {}", return_ty)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, Derivative)]
#[derivative(Hash, PartialEq)]
pub struct FunctionTypeNameParam {
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub name: Option<Ident>,

    pub ty: TypeName,
    pub modifier: Option<FunctionParamMod>,
}

impl ParseSeq for FunctionTypeNameParam {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let modifier = match tokens.match_one_maybe(Keyword::Var | Keyword::Out) {
            Some(tt) if tt.is_keyword(Keyword::Var) => Some(FunctionParamMod::Var),
            Some(tt) if tt.is_keyword(Keyword::Out) => Some(FunctionParamMod::Out),
            Some(..) => unreachable!(),
            None => None,
        };

        let mut ty = TypeName::parse(tokens)?;
        let mut param_name = None;
        
        // function types can have parameter names but they're optional and just for documentation
        // purposes, so if the previous typename is a single ident, it might actually be a param
        // name if there's a colon next
        if tokens.look_ahead().match_one(Separator::Colon).is_some() {
            match ty.into_single_ident() {
                Ok(ident) => {
                    // skip the colon we know is there
                    tokens.advance(1);

                    param_name = Some(ident);
                    ty = TypeName::parse(tokens)?;
                },

                Err(actually_ty) => {
                    ty = actually_ty;
                },
            };
        };

        Ok(FunctionTypeNameParam { name: param_name, ty, modifier })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens
            .match_one(Keyword::Var | Keyword::Out | TypeName::start_matcher())
            .is_some()
    }
}

impl fmt::Display for FunctionTypeNameParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}: ", name)?;
        }
        
        if let Some(modifier) = &self.modifier {
            write!(f, "{} ", modifier)?;
        }
        write!(f, "{}", self.ty)
    }
}
