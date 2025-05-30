use crate::ast::type_name::IdentTypeName;
use crate::ast::type_name::TypeName;
use crate::ast::{Annotation, IdentPath};
use crate::ast::Operator;
use crate::ast::{Ident, Pattern, PatternSemanticElement};
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::parse::Matcher;
use crate::{Keyword, TokenTree};
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub enum TypeNamePatternKind {
    Is,
    IsWithBinding {
        binding: Ident,
    },
    IsNot {
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        not_kw_span: Span,
    },
}

impl TypeNamePatternKind {
    pub fn binding(&self) -> Option<&Ident> {
        match self {
            TypeNamePatternKind::IsWithBinding { binding: name, .. } => Some(name),
            _ => None,
        }
    }
}

impl TypeNamePatternKind {
    fn semantic_elements<A: Annotation>(&self,
        elements: &mut Vec<PatternSemanticElement<A>>,
        pattern_element: PatternSemanticElement<A>,
    ) {
        match self {
            TypeNamePatternKind::Is => { 
                elements.push(pattern_element);
            }
            TypeNamePatternKind::IsWithBinding { binding } => {
                elements.push(pattern_element);
                elements.push(PatternSemanticElement::Binding(binding.span.clone()));
            }
            TypeNamePatternKind::IsNot { not_kw_span } => {
                elements.push(PatternSemanticElement::Keyword(not_kw_span.clone()));
                elements.push(pattern_element);
            }
        }
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub enum TypeNamePattern {
    TypeName {
        name: IdentPath,
        kind: TypeNamePatternKind,

        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        span: Span,
    },
    ExactType {
        name: TypeName,
        kind: TypeNamePatternKind,

        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        span: Span,
    },
}

impl TypeNamePattern {
    pub fn kind(&self) -> &TypeNamePatternKind {
        match self {
            TypeNamePattern::ExactType { kind, .. } => kind,
            TypeNamePattern::TypeName { kind, .. } => kind,
        }
    }
    
    pub fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        match tokens.look_ahead().match_one(Keyword::Is) {
            Some(..) => {
                let pattern = TypeNamePattern::parse(tokens)?;
                Ok(Some(pattern))
            },

            None => Ok(None),
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let not_kw = tokens.match_one_maybe(Operator::Not);
        let name = TypeName::parse(tokens)?;

        let pattern_path = match &name {
            TypeName::Ident(IdentTypeName {
                ident,
                type_args,
                indirection,
                ..
            }) => {
                if ident.as_slice().len() >= 2 && type_args.is_none() && *indirection == 0 {
                    Some(ident)
                } else {
                    None
                }
            },

            _ => None,
        };

        let binding = if not_kw.is_none() {
            tokens
                .match_one_maybe(Matcher::AnyIdent)
                .and_then(TokenTree::into_ident)
        } else {
            None
        };

        let (kind, span) = match (binding, not_kw) {
            (Some(binding), None) => {
                let span = name.span().to(binding.span());
                let kind = TypeNamePatternKind::IsWithBinding {
                    binding, 
                };

                (kind, span)
            },

            (None, Some(not_kw)) => {
                let span = not_kw.span().to(name.span());
                let kind = TypeNamePatternKind::IsNot {
                    not_kw_span: not_kw.into_span(),
                };

                (kind, span)
            },

            (None, None) => {
                let span = name.span().clone();
                let kind = TypeNamePatternKind::Is;
                (kind, span)
            },

            (Some(..), Some(..)) => {
                unreachable!()
            },
        };

        match pattern_path {
            Some(pattern_path) => Ok(TypeNamePattern::TypeName {
                name: pattern_path.clone(),
                span,
                kind,
            }),

            None => Ok(TypeNamePattern::ExactType { 
                name, 
                span, 
                kind 
            }),
        }
    }
}

impl fmt::Display for TypeNamePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeNamePattern::TypeName { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot { .. } = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding { binding, .. } = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            },

            TypeNamePattern::ExactType { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot { .. } = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding { binding, .. } = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            },
        }
    }
}

impl Pattern for TypeNamePattern {
    type Annotation = Span;

    fn semantic_elements(&self) -> Vec<PatternSemanticElement<Self::Annotation>> {
        let mut elements = Vec::new();

        match self {
            TypeNamePattern::TypeName { name, kind, .. } => {
                kind.semantic_elements(&mut elements, PatternSemanticElement::Path(name.path_span()));
            }

            TypeNamePattern::ExactType { name, kind, .. } => {
                kind.semantic_elements(&mut elements, PatternSemanticElement::Type(name.clone()));
            }
        }
        
        elements
    }
}

impl Spanned for TypeNamePattern {
    fn span(&self) -> &Span {
        match self {
            TypeNamePattern::TypeName { span, .. } => span,
            TypeNamePattern::ExactType { span, .. } => span,
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
