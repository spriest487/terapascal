use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::TypeAnnotation;
use crate::ast::TypeName;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Ident;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ForLoopCounterInit<A: Annotation> {
    Binding {
        name: Ident,
        ty: A::Type,
        init: Expr<A>,
    },
    Assignment {
        counter: Box<Expr<A>>,
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

            ForLoopCounterInit::Assignment { counter, value } => {
                write!(f, "{} := {}", counter, value)
            },
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ForLoopCounterRange<A: Annotation = Span> {
    pub to_expr: Expr<A>,
    pub init: ForLoopCounterInit<A>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ForLoopSequenceRange<A: Annotation = Span> {
    pub binding_name: Ident,
    pub binding_ty: A::Type,
    
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
    pub range: ForLoopRange<A>,

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
            ForLoopRange::UpTo(ForLoopCounterRange { init, to_expr }) => {
                write!(f, " {} to {}", init, to_expr)?;
            }

            ForLoopRange::InSequence(ForLoopSequenceRange { binding_name, binding_ty, src_expr: seq_expr }) => {
                write!(f, " {}", binding_name)?;
                if binding_ty.is_known() {
                    write!(f, ": {}", binding_ty)?;
                }
                write!(f, " in {}", seq_expr)?;
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
            Some(_var_kw) => {
                let binding_name = Ident::parse(tokens)?;

                let binding_ty = match tokens.match_one_maybe(Separator::Colon) {
                    Some(..) => TypeName::parse(tokens)?,
                    None => TypeName::Unspecified(binding_name.span.clone()),
                };

                if tokens.match_one_maybe(Operator::In).is_some() {
                    let seq_expr = Expr::parse(tokens)?;
                    
                    ForLoopRange::InSequence(ForLoopSequenceRange {
                        binding_name,
                        binding_ty,
                        src_expr: seq_expr,
                    })
                } else {
                    // assignment to initialize counter
                    tokens.match_one(Operator::Assignment)?;
                    let init_expr = Expr::parse(tokens)?;
                    
                    // counter high value
                    tokens.match_one(Keyword::To)?;
                    let up_to_expr = Expr::parse(tokens)?;
                    
                    ForLoopRange::UpTo(ForLoopCounterRange {
                        to_expr: up_to_expr,
                        init: ForLoopCounterInit::Binding {
                            name: binding_name,
                            ty: binding_ty,
                            init: init_expr,
                        },
                    })
                }
            }
            
            None => {
                let counter_ident = Ident::parse(tokens)?;
                let counter_span = counter_ident.span.clone();
                let counter_expr = Expr::Ident(counter_ident, counter_span);

                tokens.match_one(Operator::Assignment)?;
                let init_expr = Expr::parse(tokens)?;

                // counter high value
                tokens.match_one(Keyword::To)?;
                let up_to_expr = Expr::parse(tokens)?;

                ForLoopRange::UpTo(ForLoopCounterRange {
                    to_expr: up_to_expr,
                    init: ForLoopCounterInit::Assignment {
                        counter: Box::new(counter_expr),
                        value: Box::new(init_expr),
                    },
                })
            }
        };

        tokens.match_one(Keyword::Do)?;
        let body = Stmt::parse(tokens)?;

        let span = for_kw.span().to(body.annotation().span());

        Ok(Self {
            range,
            body: Box::new(body),
            annotation: span,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct WhileLoop<A: Annotation> {
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

        let _do = tokens.match_one(Keyword::Do)?;

        let body = Stmt::parse(tokens)?;

        let span = kw.span().to(body.annotation().span());

        Ok(WhileLoop {
            condition,
            body: Box::new(body),
            annotation: span,
        })
    }
}
