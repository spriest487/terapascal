use crate::ast::type_name::TypeName;
use crate::ast::{Annotation, Ident};
use crate::ast::Operator;
use crate::ast::UncheckedType;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::MaybeSpanned;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub enum MatchPattern<A: Annotation = Span> {
    Name {
        name: TypeName<A>,
        
        // not populated by the parser (requires type info). for patterns referencing
        // variant cases, we remove the last element from the ident type path contained in the
        // typename, and store it separately here
        case: Option<Ident>,

        // this represents the actual meaning of the typename
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        annotation: A,
        
        // if we create a binding from this pattern, the type of that binding
        binding_type: A::Type,
    },
    Not {
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        not_kw: Span,

        pattern: Box<Self>,

        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        span: Span,
    },
}

impl<A: Annotation> MatchPattern<A> {
    pub fn visit_references<F>(&self, f: &mut F) 
    where
        F: FnMut(&TypeName<A>, &A)
    {
        match self {
            MatchPattern::Name { name, annotation, .. } => {
                f(name, annotation)
            }
            MatchPattern::Not { pattern, .. } => {
                pattern.visit_references(f)
            }
        }
    }
}

impl MatchPattern {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        match tokens.match_one_maybe(Operator::Not) {
            Some(not_kw) => {
                let pattern = Self::parse(tokens)?;
                let not_kw = not_kw.into_span();
                let span = not_kw.to(&pattern);

                Ok(Self::Not {
                    not_kw,
                    pattern: Box::new(pattern),
                    span,
                })
            },

            None => {
                let name = TypeName::parse(tokens)?;
                let annotation = name.get_span().unwrap().clone();

                Ok(Self::Name {
                    name,
                    annotation,
                    binding_type: UncheckedType,
                    case: None,
                })
            },
        }
    }
}

impl<A: Annotation> fmt::Display for MatchPattern<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MatchPattern::Name { name, .. } => write!(f, "{}", name),
            MatchPattern::Not { pattern, .. } => write!(f, "not {}", pattern),
        }
    }
}

impl<A: Annotation> Spanned for MatchPattern<A> {
    fn span(&self) -> &Span {
        match self {
            MatchPattern::Name { annotation, .. } => annotation.span(),
            MatchPattern::Not { span, .. } => span,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum BindingDeclKind {
    Const,
    Var,
}

impl fmt::Display for BindingDeclKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BindingDeclKind::Var => writeln!(f, "var"),
            BindingDeclKind::Const => writeln!(f, "const"),
        }
    }
}
