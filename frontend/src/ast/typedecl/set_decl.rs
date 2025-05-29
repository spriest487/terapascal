use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::IdentTypeName;
use crate::ast::DeclIdent;
use crate::ast::TypeName;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::Keyword;
use crate::Operator;
use crate::TokenStream;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct SetDecl<A: Annotation = Span> {
    pub name: A::DeclName,
    
    pub range: Box<SetDeclRange<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl SetDecl<Span> {
    pub fn parse(name: DeclIdent, tokens: &mut TokenStream) -> ParseResult<Self> {
        let set_kw = tokens.match_one(Keyword::Set)?;
        tokens.match_one(Keyword::Of)?;

        // todo: if we support range type decls later, this could be rolled into TypeName parsing 
        let range = match Expr::parse(tokens)? {
            Expr::BinOp(bin_op) if bin_op.op == Operator::RangeInclusive => {
                SetDeclRange::Range {
                    from: bin_op.lhs,
                    to: bin_op.rhs,
                    span: bin_op.annotation,
                }
            },

            other => {
                let Some(type_name) = other.clone().try_into_ident_path() else {
                    return Err(TracedError::trace(ParseError::InvalidSetRangeExpr {
                        span: other.span().clone(),
                    }));    
                };
                let name_span = type_name.path_span();
                
                SetDeclRange::Type {
                    span: name_span.clone(),
                    ty: TypeName::Ident(IdentTypeName {
                        span: name_span,
                        ident: type_name,
                        indirection: 0,
                        type_args: None,
                    }),
                }
            },
        };

        let span = set_kw.into_span().to(range.span());

        Ok(SetDecl {
            name,
            range: Box::new(range),
            span
        })
    }
}

impl<A: Annotation> Spanned for SetDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for SetDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "set of ")?;

        match self.range.as_ref() {
            SetDeclRange::Type { ty, .. } => {
                write!(f, "{}", ty)?;
            }
            SetDeclRange::Range { from, to, .. } => {
                write!(f, "{}{}{}", from, Operator::RangeInclusive, to)?;
            }
        }

        write!(f, "]")
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub enum SetDeclRange<A: Annotation = Span> {
    Type {
        ty: A::Type,
        span: Span
    },
    Range {
        from: Expr<A>,
        to: Expr<A>,

        #[derivative(Debug = "ignore")]
        #[derivative(Hash = "ignore")]
        #[derivative(PartialEq = "ignore")]
        span: Span,
    }
}

impl<A: Annotation> Spanned for SetDeclRange<A> {
    fn span(&self) -> &Span {
        match self {
            SetDeclRange::Type { span, .. } => span,
            SetDeclRange::Range { span, .. } => span,
        }
    }
}
