use crate::ast::type_name::IdentTypeName;
use crate::ast::type_name::TypeName;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Operator;
use crate::TokenTree;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeNamePatternKind {
    Is,
    IsWithBinding(Ident),
    IsNot,
}

impl TypeNamePatternKind {
    pub fn binding(&self) -> Option<&Ident> {
        match self {
            TypeNamePatternKind::IsWithBinding(binding) => Some(binding),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeNamePattern {
    TypeName {
        name: IdentPath,
        kind: TypeNamePatternKind,
        span: Span,
    },
    ExactType {
        name: TypeName,
        kind: TypeNamePatternKind,
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

        let span = match (&not_kw, &binding) {
            (Some(not_kw), None) => not_kw.span().to(name.span()),
            (None, Some(binding)) => name.span().to(binding.span()),
            _ => name.span().clone(),
        };

        let kind = match binding {
            Some(binding) => {
                assert!(not_kw.is_none());
                TypeNamePatternKind::IsWithBinding(binding)
            },
            None if not_kw.is_some() => TypeNamePatternKind::IsNot,
            None => TypeNamePatternKind::Is,
        };

        match pattern_path {
            Some(pattern_path) => Ok(TypeNamePattern::TypeName {
                name: pattern_path.clone(),
                span,
                kind,
            }),

            None => Ok(TypeNamePattern::ExactType { name, span, kind }),
        }
    }
}

impl fmt::Display for TypeNamePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeNamePattern::TypeName { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding(binding) = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            },

            TypeNamePattern::ExactType { name, kind, .. } => {
                if let TypeNamePatternKind::IsNot = kind {
                    write!(f, "not ")?;
                }
                write!(f, "{}", name)?;
                if let TypeNamePatternKind::IsWithBinding(binding) = kind {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            },
        }
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
