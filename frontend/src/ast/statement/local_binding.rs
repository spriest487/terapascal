use crate::ast::type_name::TypeName;
use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Spanned;
use terapascal_common::span::Span;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct LocalBinding<A: Annotation = Span> {
    pub name: Ident,
    pub ty: TypeName<A>,
    pub val: Option<Expr<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub kw_span: Span,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub assign_op_span: Option<Span>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl LocalBinding {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let var_kw_tt = tokens.match_one(Keyword::Var)?;
        let kw_span = var_kw_tt.into_span();

        let name = Ident::parse(tokens)?;

        let binding_span = kw_span.to(&name);
        let mut binding = LocalBinding {
            kw_span,
            name,
            ty: TypeName::unspecified(),
            assign_op_span: None,
            val: None,
            annotation: binding_span,
        };

        if tokens.match_one_maybe(Separator::Colon).is_some() {
            binding.ty = TypeName::parse(tokens)?;
            binding.annotation.maybe_extend(&binding.ty);
        }

        if let Some(tt) = tokens.match_one_maybe(Operator::Assignment) {
            binding.assign_op_span = Some(tt.into_span());

            let val_expr = Expr::parse(tokens)?;
            binding.annotation.extend(&val_expr);
            binding.val = Some(val_expr);
        }

        Ok(binding)
    }
}

impl<A: Annotation> fmt::Display for LocalBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "var {}", self.name)?;
        if self.ty.is_known() {
            write!(f, ": {}", self.ty)?;
        }
        if let Some(val) = &self.val {
            write!(f, " := {}", val)?;
        }
        Ok(())
    }
}

impl<A: Annotation> Spanned for LocalBinding<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
