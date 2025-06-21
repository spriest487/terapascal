mod matcher;
mod token_stream;

pub use self::matcher::*;
pub use self::token_stream::*;
use crate::ast::TypeName;
use crate::ast::*;
use crate::token_tree::*;
use terapascal_common::span::*;
use terapascal_common::{DiagnosticLabel, Severity};
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_common::TracedError;
use std::fmt;
use std::iter::once;

#[derive(Debug)]
pub struct IllegalStatement<A: Annotation = Span>(pub Box<Expr<A>>);

impl<A: Annotation> fmt::Display for IllegalStatement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expected a statement, got expression `{}`", self.0)
    }
}

impl<A: Annotation> IllegalStatement<A> {
    pub fn title(&self) -> String {
        "Illegal statement".to_string()
    }
}

impl<A: Annotation> From<Expr<A>> for IllegalStatement<A> {
    fn from(expr: Expr<A>) -> Self {
        IllegalStatement(Box::new(expr))
    }
}

#[derive(Debug)]
pub enum ParseError {
    InvalidUnitFilename(Span),
    
    UnexpectedToken(Box<TokenTree>, Option<Matcher>),
    UnexpectedTokens(Span, Option<Matcher>),
    UnexpectedEOF(Matcher, Span),
    EmptyOperand { operator: Span, before: bool },
    UnexpectedOperator { operator: Span },

    StatementIsIllegal(Box<Stmt>),
    ExprIsIllegal(IllegalStatement),
    IsExpr(IllegalStatement),

    UnterminatedStatement { span: Span },
    InvalidForLoopInit(Stmt<Span>),
    
    DuplicateModifier { new: DeclMod, existing: DeclMod },
    
    CtorWithTypeArgs { span: Span },
    InvalidAssignmentExpr { span: Span },
    
    InvalidSetRangeExpr { span: Span },
    
    EmptyTypeParamList(TypeIdentList),
    EmptyTypeArgList(TypeArgList),
    InvalidTypeParamName(Span),
    
    EmptyWhereClause(WhereClause),
    
    InvalidFunctionImplType(TypeName),
    EmptyConstOrVarDecl { span: Span },
    MultiVarDeclHasInitExpr { span: Span },
    
    EmptyTypeDecl { span: Span },
    InvalidTagLocation { location: InvalidTagLocation, span: Span, tags_span: Span },
    
    AggregateBlock(AggregateParseError<Block>),
}

impl ParseError {
    pub fn is_expr(expr: impl Into<Box<Expr>>) -> Self {
        Self::IsExpr(IllegalStatement(expr.into()))
    }

    pub fn illegal_expr(expr: impl Into<Box<Expr>>) -> Self {
        Self::ExprIsIllegal(IllegalStatement(expr.into()))
    }

    pub fn is_stmt(stmt: impl Into<Box<Stmt>>) -> Self {
        Self::StatementIsIllegal(stmt.into())
    }
    
    pub fn expr_is_illegal(self) -> Self {
        match self {
            ParseError::IsExpr(expr) => ParseError::ExprIsIllegal(expr), 
            other => other,
        }
    }
    
    pub fn invalid_tag_loc(
        location: InvalidTagLocation,
        span: Span,
        tags: &[impl Spanned]
    ) -> Self {
        ParseError::InvalidTagLocation {
            location,
            span,
            tags_span: Span::range(tags).unwrap(),
        }
    }

    pub fn forward_decl_tags(span: Span, tags: &[impl Spanned]) -> Self {
        Self::invalid_tag_loc(InvalidTagLocation::ForwardDecl, span, tags)
    }

    pub fn alias_decl_tags(span: Span, tags: &[impl Spanned]) -> Self {
        Self::invalid_tag_loc(InvalidTagLocation::AliasDecl, span, tags)
    }
}

pub type ParseResult<T> = Result<T, TracedError<ParseError>>;

impl Spanned for ParseError {
    fn span(&self) -> &Span {
        match self {
            ParseError::InvalidUnitFilename(span) => span,
            ParseError::UnexpectedToken(tt, _) => match tt.as_ref() {
                TokenTree::Delimited(group) if group.inner.is_empty() => &group.span,
                TokenTree::Delimited(group) => group.open.span(),
                _ => tt.span(),
            },
            ParseError::UnexpectedTokens(span, _) => span,
            ParseError::UnexpectedEOF(_, tt) => tt.span(),
            ParseError::EmptyOperand { operator, .. } => operator.span(),
            ParseError::UnexpectedOperator { operator } => operator.span(),
            ParseError::StatementIsIllegal(stmt) => stmt.span(),
            ParseError::IsExpr(IllegalStatement(expr)) => expr.annotation().span(),
            ParseError::ExprIsIllegal(IllegalStatement(expr)) => expr.annotation().span(),
            ParseError::DuplicateModifier { new, .. } => new.span(),
            ParseError::CtorWithTypeArgs { span } => span,
            ParseError::EmptyTypeParamList(tl) => tl.span(),
            ParseError::EmptyTypeArgList(tl) => tl.span(),
            ParseError::EmptyWhereClause(c) => c.span(),
            ParseError::UnterminatedStatement { span } => span,
            ParseError::InvalidFunctionImplType(tn) => tn.span(),
            ParseError::InvalidAssignmentExpr { span } => span,
            ParseError::EmptyConstOrVarDecl { span, .. } => span,
            ParseError::MultiVarDeclHasInitExpr { span, .. } => span,
            ParseError::EmptyTypeDecl { span, .. } => span,
            ParseError::InvalidForLoopInit(stmt) => stmt.span(),
            ParseError::InvalidTypeParamName(span) => span,
            ParseError::InvalidSetRangeExpr { span } => span,
            ParseError::InvalidTagLocation { span, .. } => span,
            ParseError::AggregateBlock(err) => err.first.span(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidUnitFilename(..) => write!(f, "Invalid unit filename"),

            ParseError::UnexpectedToken(..) => write!(f, "Unexpected token"),
            ParseError::UnexpectedTokens(..) => write!(f, "Unexpected tokens"),
            ParseError::UnexpectedEOF(..) => write!(f, "Unexpected end of sequence"),
            ParseError::EmptyOperand { .. } => write!(f, "Empty operand"),
            ParseError::UnexpectedOperator { .. } => write!(f, "Unexpected operator"),

            ParseError::StatementIsIllegal(..) => write!(f, "Statement is not legal here"),
            
            ParseError::ExprIsIllegal(invalid) => write!(f, "{}", invalid.title()),
            ParseError::IsExpr(invalid) => write!(f, "{}", invalid.title()),

            ParseError::UnterminatedStatement { .. } => write!(f, "Unterminated stmt"),
            ParseError::InvalidForLoopInit( .. ) => write!(f, "Invalid for-loop initialization stmt"),
            
            ParseError::DuplicateModifier { .. } => write!(f, "Duplicate modifier"),
            ParseError::CtorWithTypeArgs { .. } => write!(f, "Constructor with type args"),

            ParseError::InvalidSetRangeExpr { .. } => write!(f, "Invalid range expression for set declaration"),
            
            ParseError::EmptyWhereClause(..) => write!(f, "Empty `where` clause"),

            ParseError::EmptyTypeParamList { .. } => write!(f, "Empty type parameter list"),
            ParseError::EmptyTypeArgList { .. } => write!(f, "Empty type argument list"),
            ParseError::InvalidTypeParamName( .. ) => write!(f, "Invalid type parameter name"),

            ParseError::InvalidFunctionImplType(..) => write!(f, "Invalid interface type for method"),
            ParseError::InvalidAssignmentExpr { .. } => write!(f, "Illegal assignment"),
            ParseError::EmptyConstOrVarDecl { .. } => write!(f, "Empty const or variable declaration"),
            ParseError::MultiVarDeclHasInitExpr { .. } => write!(f, "Multiple const or variable declaration has initialization expression"),
            
            ParseError::EmptyTypeDecl { .. } => write!(f, "Empty type declaration"),
            ParseError::InvalidTagLocation { .. } => write!(f, "Invalid tag location"),
            
            ParseError::AggregateBlock(agg) => write!(f, "{}", agg.first.err),
        }
    }
}

impl DiagnosticOutput for ParseError {
    fn label(&self) -> Option<DiagnosticLabel> {
        let text = match self {
            ParseError::InvalidUnitFilename(..) => None,
            
            ParseError::UnexpectedToken(tt, Some(expected)) => {
                Some(format!("found {}, expected {}", tt, expected))
            }

            ParseError::UnexpectedToken(tt, None) => Some(format!("unexpected {}", tt)),

            ParseError::UnexpectedTokens(_, Some(expected)) => {
                Some(format!("expected {}", expected))
            }

            ParseError::UnexpectedTokens(_, None) => {
                None
            }

            ParseError::UnexpectedEOF(expected, _tt) => {
                Some(format!("expected {} but reached end of sequence", expected))
            }

            ParseError::EmptyOperand { operator, before } => {
                let pos_name = if *before { "before" } else { "after" };
                Some(format!("expected operand {} {}", pos_name, operator))
            }

            ParseError::UnexpectedOperator { .. } => Some("expected operand, found operator".to_string()),
            
            ParseError::StatementIsIllegal(..) => None,
            ParseError::ExprIsIllegal(..) => None,

            ParseError::IsExpr(invalid_stmt) => Some(invalid_stmt.to_string()),

            ParseError::DuplicateModifier { new, .. } => Some(format!(
                "the modifier `{}` is already present on this declaration",
                new.keyword(),
            )),
            
            ParseError::InvalidSetRangeExpr { .. } => None,

            ParseError::CtorWithTypeArgs { .. } => {
                Some("Object constructor expr cannot explicitly specify type args".to_string())
            }

            ParseError::EmptyTypeParamList { .. } => {
                Some("type parameter list must contain one or more identifiers".to_string())
            }

            ParseError::EmptyTypeArgList { .. } => {
                Some("type argument list must contain one or more type names".to_string())
            }

            ParseError::EmptyWhereClause(..) => {
                Some("`where` clause must contain one or more type constraints in the form `Type is InterfaceName`".to_string())
            }

            ParseError::InvalidTypeParamName(..) => None,

            ParseError::UnterminatedStatement { .. } => {
                Some("statement here is unterminated".to_string())
            }

            ParseError::InvalidFunctionImplType(ty) => {
                Some(format!("type {} cannot have interface implementation functions declared for it", ty))
            }

            ParseError::InvalidAssignmentExpr { .. } => {
                Some("Assignment operator can only be used in an assignment stmt".to_string())
            }

            ParseError::EmptyTypeDecl { .. } => {
                Some("type declaration must contain one or more types".to_string())
            }

            ParseError::EmptyConstOrVarDecl { .. } => {
                Some("declaration must contain one or more constants".to_string())
            }
            
            ParseError::MultiVarDeclHasInitExpr { .. } => {
                Some("declaration with an initialization expression may only declare a single name".to_string())
            }

            ParseError::InvalidForLoopInit(stmt) => {
                Some(format!("stmt `{}` cannot be used to initialize the counter variable of a for-loop", stmt))
            }
            
            ParseError::InvalidTagLocation { location, .. } => {
                let loc_name = match location {
                    InvalidTagLocation::ForwardDecl => "forward declaration",
                    InvalidTagLocation::AliasDecl => "alias declaration",
                    InvalidTagLocation::SetDecl => "set declaration",
                    InvalidTagLocation::EnumDecl => "enum declaration",
                };

                Some(format!("{loc_name} must not have tags"))
            }
            
            ParseError::AggregateBlock(agg) => {
                agg.first.label().as_ref().and_then(|label| label.text.clone())
            }
        };

        Some(DiagnosticLabel {
            text,
            span: self.span().clone(),
        })
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            ParseError::DuplicateModifier { existing, .. } => vec![
                DiagnosticMessage::info("Duplicate modifier occurrence")
                    .with_label(DiagnosticLabel::new(existing.span().clone())
                        .with_text(format!("`{}` appears here", existing.keyword())))
            ],

            ParseError::InvalidTagLocation { tags_span, .. } => vec![
                DiagnosticMessage::info("Tags declared here".to_string())
                    .with_label(DiagnosticLabel::new(tags_span.clone()))
            ],
            
            ParseError::AggregateBlock(agg) => {
                agg.rest_messages()
            }

            _ => Vec::new(),
        }
    }
}

pub trait Parse: Sized {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>;
}

pub trait Match {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool;
}

pub trait TryParse : Sized {
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>>;
}

impl<T> TryParse for T where T : Parse + Match {
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        if Self::is_match(&mut tokens.look_ahead()) {
            Self::parse(tokens).map(Some)
        } else {
            Ok(None)
        }
    }
}

#[derive(Copy, Debug, Clone, Eq, PartialEq, Hash)]
pub enum InvalidTagLocation {
    ForwardDecl,
    AliasDecl,
    SetDecl,
    EnumDecl,
} 

#[derive(Debug)]
pub struct AggregateParseError<T> {
    pub item: Box<T>,

    pub first: Box<TracedError<ParseError>>,
    pub rest: Vec<TracedError<ParseError>>,
}

impl<T> AggregateParseError<T> {
    pub fn result(item: T, mut errors: Vec<TracedError<ParseError>>) -> AggregateParseResult<T> {
        if errors.is_empty() {
            return Ok(item);
        }

        let first_err = errors.remove(0);

        Err(AggregateParseError {
            item: Box::new(item),
            first: Box::new(first_err),
            rest: errors,
        })
    }
    
    pub fn first_title(&self) -> String {
        self.first.err.title()
    }
    
    pub fn first_label_text(&self) -> Option<String> {
        self.first.err.label().and_then(|label| label.text)
    }
    
    pub fn rest_messages(&self) -> Vec<DiagnosticMessage> {
        self.rest
            .iter()
            .flat_map(|err| {
                let main = err.main(Severity::Error);
                let see_also = err.see_also();
                
                once(main).chain(see_also)
            })
            .collect()
    }
}

impl From<AggregateParseError<Block>> for TracedError<ParseError> {
    fn from(value: AggregateParseError<Block>) -> Self {
        let bt = value.first.bt.clone();
        
        TracedError { 
            err: ParseError::AggregateBlock(value), 
            bt 
        }
    }
} 

pub type AggregateParseResult<T> = Result<T, AggregateParseError<T>>;
