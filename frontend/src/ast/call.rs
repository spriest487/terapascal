use crate::ast;
use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::ast::ObjectCtor;
use crate::ast::TypeArgList;
use crate::ast::TypeList;
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
#[derivative(Debug, PartialEq, Hash)]
pub struct MethodCall<A: Annotation> {
    // for non-virtual calls, the interface type is the same as the self type
    pub iface_type: A::Type,
    
    // for static calls, the self type should be Nothing
    pub self_type: A::Type,
    
    // if the method is invoked via its qualified name, e.g. MyType.MyMethod(itself), the span
    // of the type name part
    pub self_type_qual_span: Option<Span>,

    // index of the method in the interface type's method list
    pub iface_method_index: usize,

    pub func_type: A::Type,

    pub method_name: Ident,

    pub args: Vec<Expr<A>>,
    pub type_args: Option<TypeList<A::TypeName>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub annotation: A,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub args_span: Span,
}

impl<A: Annotation> Spanned for MethodCall<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for MethodCall<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}(", self.iface_type, self.method_name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct FunctionCall<A: Annotation = Span> {
    pub target: Expr<A>,
    pub args: Vec<Expr<A>>,

    pub type_args: Option<TypeArgList<A>>,

    pub annotation: A,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub args_span: Option<Span>,
}

impl<A: Annotation> fmt::Display for FunctionCall<A> {
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

impl FunctionCall {
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

impl<A: Annotation> Spanned for FunctionCall<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct FunctionCallNoArgs<A: Annotation> {
    pub target: Expr<A>,
    
    // for free functions called via method syntax, the implicit self argument
    pub self_arg: Option<Expr<A>>,
    
    pub type_args: Option<TypeList<A::TypeName>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for FunctionCallNoArgs<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.target)
    }
}

impl<A: Annotation> Spanned for FunctionCallNoArgs<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}


#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct MethodCallNoArgs<A: Annotation> {
    // with no arg list, there must be a target expression (the self arg)
    pub target: Expr<A>,
 
    pub method_name: Ident,

    pub type_args: Option<TypeList<A::TypeName>>,

    // for virtual calls, the owning type is the interface and the self type is the implementor
    // here the self-type is the type of the argument expression - if there is none, it must be
    // a call to a static method, so we only need to know the owning type
    pub owning_type: A::Type,
    
    // a no-args call to a non-class method should have an implied target
    // e.g. the `a` in the call `a.B`
    pub self_arg: Option<Expr<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for MethodCallNoArgs<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.target)
    }
}

impl<A: Annotation> Spanned for MethodCallNoArgs<A> {
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

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Call<A: Annotation> {
    // call to a standalone function
    Function(FunctionCall<A>),
}

impl<A: Annotation> fmt::Display for Call<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Call::Function(call) => write!(f, "{}", call),
        }
    }
}

impl<A: Annotation> Spanned for Call<A> {
    fn span(&self) -> &Span {
        match self {
            Call::Function(call) => call.span(),
        }
    }
}

impl<A: Annotation> Call<A> {
    pub fn name(&self) -> &str {
        match self {
            Call::Function(_) => "function call",
        }
    }
    
    pub fn annotation(&self) -> &A {
        match self {
            Call::Function(call) => &call.annotation,
        }
    }

    pub fn annotation_mut(&mut self) -> &mut A {
        match self {
            Call::Function(call) => &mut call.annotation,
        }
    }
    
    pub fn args(&self) -> &[Expr<A>] {
        match self {
            Call::Function(func_call) => &func_call.args,
        }
    }
    
    pub fn as_func_call(&self) -> Option<&FunctionCall<A>> {
        match self {
            Call::Function(func_call) => Some(func_call),
        }
    }
    
    pub fn try_into_func_call(self) -> Option<FunctionCall<A>> {
        match self {
            Call::Function(func_call) => Some(func_call),
        }
    }
    
    pub fn type_qualification_span(&self) -> Option<&Span> {
        match self {
            _ => None,
        }
    }
    
    pub fn target_expr(&self) -> Option<&Expr<A>> {
        match self {
            Call::Function(call) => Some(&call.target),
        }
    }
    
    pub fn method_name_span(&self) -> Option<&Span> {
        match self {
            _ => None,
        }
    }
    
    pub fn type_args(&self) -> Option<&TypeList<A::TypeName>> {
        match self {
            Call::Function(call) => call.type_args.as_ref(),
        }
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
        let mut args_tokens = args_group.to_inner_tokens();

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

