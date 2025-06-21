use crate::ast;
use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::ast::ObjectCtor;
use crate::ast::TypeArgList;
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::Separator;
use crate::TokenTree;
use derivative::*;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct Call<A: Annotation = Span> {
    pub target: Expr<A>,
    pub args: Vec<Expr<A>>,

    pub type_args: Option<TypeArgList<A>>,

    pub annotation: A,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub args_span: Option<Span>,
}

impl<A: Annotation> fmt::Display for Call<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.target)?;

        if let Some(type_args) = self.type_args.as_ref() {
            write!(f, "{}", type_args)?;
        }

        write!(f, "(")?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl Call {
    // at the parsing stage, we can't tell empty object ctors and function calls apart.
    // if the typechecker finds a call expression that isn't valid as a function call, it can
    // try reinterpreting it as a constructor.
    pub fn try_into_empty_object_ctor(self) -> Option<ObjectCtor> {
        if !self.args.is_empty() {
            return None;
        }
        
        // there must be a literal arg group in this call, because the () is required for a ctor 
        let Some(args_span) = self.args_span else {
            return None;
        };

        let ctor = ObjectCtor {
            type_expr: Some(self.target),

            args: ast::ObjectCtorArgs {
                span: args_span,
                members: Vec::new(),
            },
            type_args: self.type_args,
            
            annotation: self.annotation,
        };

        Some(ctor)
    }
}

impl<A: Annotation> Spanned for Call<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct VariantCtorCall<A: Annotation> {
    pub variant: Arc<A::DeclName>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub variant_name_span: Span,
    pub case: Ident,

    pub arg: Option<Expr<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for VariantCtorCall<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}(", self.variant, self.case)?;
        if let Some(arg) = &self.arg {
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl<A: Annotation> Spanned for VariantCtorCall<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

struct ArgListItem(Expr<Span>);

impl ParseSeq for ArgListItem {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Comma)?;
        }

        let arg = Expr::parse(tokens)?;
        Ok(ArgListItem(arg))
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Comma).is_none() {
            return false;
        }

        tokens.match_one(Matcher::ExprOperandStart).is_some()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ArgList<A: Annotation = Span> {
    pub open: Span,
    pub close: Span,
    pub args: Vec<Expr<A>>,
}

impl ArgList<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let brackets = tokens.match_one(Matcher::Delimited(DelimiterPair::Bracket))?;

        let args_group = match brackets {
            TokenTree::Delimited(group @ DelimitedGroup { delim: DelimiterPair::Bracket, .. }) => group,
            _ => unreachable!(),
        };

        let open = args_group.open.clone();
        let close = args_group.close.clone();
        let mut args_tokens = args_group.into_inner_tokens();

        let args = ArgListItem::parse_seq(&mut args_tokens)?
            .into_iter()
            .map(|a| a.0)
            .collect();

        args_tokens.finish()?;

        Ok(ArgList { args, open, close })
    }

    pub fn list_span(&self) -> Span {
        self.open.to(&self.close)
    }
}

