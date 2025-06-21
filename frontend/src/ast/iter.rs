use crate::ast::{Annotation, Ident};
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::TypeAnnotation;
use crate::ast::TypeName;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub enum ForLoopCounterInit<A: Annotation> {
    // `var name: Type := init` 
    Binding {
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        binding_kw_span: Span,
        
        name: Ident,
        ty: A::TypeName,

        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        assign_op_span: Span,

        init: Expr<A>,
    },
    
    // `counter := value`
    Assignment {
        counter: Box<Expr<A>>,

        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        assign_op_span: Span,

        value: Box<Expr<A>>,
    },
}

impl<A: Annotation> fmt::Display for ForLoopCounterInit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForLoopCounterInit::Binding { name, ty, init, .. } => {
                write!(f, "var {}", name)?;
                if ty.is_known() {
                    write!(f, ": {}", ty)?;
                }
                
                write!(f, " := {}", init)
            },

            ForLoopCounterInit::Assignment { counter, value, .. } => {
                write!(f, "{} := {}", counter, value)
            },
        }
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ForLoopCounterRange<A: Annotation = Span> {
    pub to_expr: Expr<A>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub to_kw_span: Span,

    pub init: ForLoopCounterInit<A>,
}

// var binding_name: binding_ty in src_expr
#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ForLoopSequenceRange<A: Annotation = Span> {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub binding_kw_span: Span,
    
    pub binding_name: Ident,
    pub binding_ty: A::TypeName,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub in_kw_span: Span,
    
    pub src_expr: Expr<A>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ForLoopRange<A: Annotation = Span> {
    UpTo(ForLoopCounterRange<A>),
    InSequence(ForLoopSequenceRange<A>)
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ForLoop<A: Annotation> {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub for_kw_span: Span,

    pub range: ForLoopRange<A>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub do_kw_span: Span,

    pub body: Box<Stmt<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for ForLoop<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "for")?;
        match &self.range {
            ForLoopRange::UpTo(ForLoopCounterRange { init, to_expr, .. }) => {
                write!(f, " {} to {}", init, to_expr)?;
            }

            ForLoopRange::InSequence(ForLoopSequenceRange { binding_name, binding_ty, src_expr, .. }) => {
                write!(f, " {}", binding_name)?;
                if binding_ty.is_known() {
                    write!(f, ": {}", binding_ty)?;
                }
                write!(f, " in {}", src_expr)?;
            }
        }
        write!(f, " do {}", self.body)
    }
}

impl<A: Annotation> Spanned for ForLoop<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl ForLoop<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let for_kw = tokens.match_one(Keyword::For)?;
        
        let range = match tokens.match_one_maybe(Keyword::Var) {
            Some(var_kw) => {
                let binding_name = Ident::parse(tokens)?;

                let binding_ty = match tokens.match_one_maybe(Separator::Colon) {
                    Some(..) => TypeName::parse(tokens)?,
                    None => TypeName::Unspecified(binding_name.span.clone()),
                };

                if let Some(in_tt) = tokens.match_one_maybe(Operator::In) {
                    let seq_expr = Expr::parse(tokens)?;
                    
                    ForLoopRange::InSequence(ForLoopSequenceRange {
                        binding_kw_span: var_kw.into_span(),
                        binding_name,
                        binding_ty,
                        in_kw_span: in_tt.into_span(),
                        src_expr: seq_expr,
                    })
                } else {
                    // assignment to initialize counter
                    let assign_tt = tokens.match_one(Operator::Assignment)?;
                    let init_expr = Expr::parse(tokens)?;
                    
                    // counter high value
                    let to_tt = tokens.match_one(Keyword::To)?;
                    let up_to_expr = Expr::parse(tokens)?;
                    
                    ForLoopRange::UpTo(ForLoopCounterRange {
                        to_expr: up_to_expr,
                        to_kw_span: to_tt.into_span(),

                        init: ForLoopCounterInit::Binding {
                            binding_kw_span: var_kw.into_span(),
                            name: binding_name,
                            ty: binding_ty,
                            init: init_expr,
                            assign_op_span: assign_tt.into_span(),
                        },
                    })
                }
            }
            
            None => {
                let counter_ident = Ident::parse(tokens)?;
                let counter_span = counter_ident.span.clone();
                let counter_expr = Expr::Ident(counter_ident, counter_span);

                let assign_tt = tokens.match_one(Operator::Assignment)?;
                let init_expr = Expr::parse(tokens)?;

                // counter high value
                let to_tt = tokens.match_one(Keyword::To)?;
                let up_to_expr = Expr::parse(tokens)?;

                ForLoopRange::UpTo(ForLoopCounterRange {
                    to_expr: up_to_expr,
                    to_kw_span: to_tt.into_span(),

                    init: ForLoopCounterInit::Assignment {
                        counter: Box::new(counter_expr),
                        value: Box::new(init_expr),
                        assign_op_span: assign_tt.into_span(),
                    },
                })
            }
        };

        let do_tt = tokens.match_one(Keyword::Do)?;
        let body = Stmt::parse(tokens)?;

        let span = for_kw.span().to(body.annotation().span());

        Ok(Self {
            for_kw_span: for_kw.into_span(),
            range,
            body: Box::new(body),
            annotation: span,
            do_kw_span: do_tt.into_span(),
        })
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct WhileLoop<A: Annotation> {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub while_kw_span: Span,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub do_kw_span: Span,
    
    pub condition: Expr<A>,
    pub body: Box<Stmt<A>>,

    pub annotation: A,
}

impl<A: Annotation> Spanned for WhileLoop<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for WhileLoop<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "while {} do {}", self.condition, self.body)
    }
}

impl WhileLoop<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::While)?;

        let condition = Expr::parse(tokens)?;

        let do_tt = tokens.match_one(Keyword::Do)?;

        let body = Stmt::parse(tokens)?;

        let span = kw.span().to(body.annotation().span());

        Ok(WhileLoop {
            while_kw_span: kw.into_span(),
            condition,
            body: Box::new(body),
            annotation: span,
            do_kw_span: do_tt.into_span(),
        })
    }
}
