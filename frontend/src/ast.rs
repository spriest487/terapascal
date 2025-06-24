pub mod block;
pub mod call;
mod case;
pub mod cast;
pub mod cond;
pub mod ctor;
pub mod expression;
pub mod function;
pub mod iter;
mod match_block;
pub mod op;
pub mod raise;
pub mod statement;
pub mod type_constraint;
pub mod typedecl;
pub mod unit;
pub mod util;
mod type_list;
mod type_name_pattern;
mod type_name;
mod ident;
pub mod keyword;
pub mod operators;
pub mod tag;

use crate::IntConstant;
pub use block::*;
pub use call::*;
pub use case::CaseBlock;
pub use case::CaseBranch;
pub use case::CaseExpr;
pub use case::CaseStmt;
pub use cast::Cast;
pub use cond::*;
pub use ctor::*;
pub use expression::*;
pub use function::*;
pub use ident::*;
pub use iter::*;
pub use keyword::*;
pub use match_block::*;
pub use op::*;
pub use operators::*;
pub use raise::*;
pub use statement::*;
use std::fmt;
use std::hash::Hash;
use terapascal_common::span::Spanned;
use terapascal_common::span::Span;
pub use type_constraint::*;
pub use type_list::*;
pub use type_name::*;
pub use type_name_pattern::*;
pub use typedecl::*;
pub use unit::*;

pub trait FunctionName<A: Annotation> : fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn ident(&self) -> &Ident;

    fn owning_type_qualifier(&self) -> Option<TypeName<A>>;

    fn type_params_len(&self) -> usize;
    fn type_param_span(&self, index: usize) -> &Span;
}

pub trait ConstExprValue<A: Annotation, Val> : Spanned + fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn as_expr(&self) -> &Expr<A>; 
}

impl<T> ConstExprValue<Span, T> for Box<Expr<Span>> {
    fn as_expr(&self) -> &Expr<Span> {
        self.as_ref()
    }
}

pub trait DeclName: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn ident(&self) -> &Ident;
    fn type_params_len(&self) -> usize;
    fn type_param_name_span(&self, index: usize) -> Option<&Span>;
}

pub enum PatternSemanticElement<A> where A: Annotation {
    Keyword(Span),
    Binding(Span),
    Type(TypeName<A>),
    VariantCase(Span),
    
    // could be either a type or an enum, not enough info (parsed AST only)
    Path(Span),
}

pub trait Pattern: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    type Annotation: Annotation;

    // TODO this could be more nicely represented as a shared AST node type
    fn semantic_elements(&self) -> Vec<PatternSemanticElement<Self::Annotation>>;
}

pub trait Annotation: Spanned + Clone + PartialEq + Eq + Hash {
    type Type: Clone + Eq + PartialEq + Hash + fmt::Debug;
    
    type DeclName: DeclName;
    
    type Pattern: Pattern<Annotation = Self>; 

    type FunctionName: FunctionName<Self>;

    type ConstStringExpr: ConstExprValue<Self, String>;
    type ConstIntegerExpr: ConstExprValue<Self, IntConstant>;

    type ConstValue: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
    
    fn semantic_hint(&self) -> SemanticHint;

    fn is_known_type(type_name: &TypeName<Self>) -> bool;
}

impl Annotation for Span {
    type Type = UncheckedType;
    
    type DeclName = DeclIdent;
    type Pattern = TypeNamePattern;
    
    type FunctionName = QualifiedFunctionName;

    type ConstStringExpr = Box<Expr<Span>>;
    type ConstIntegerExpr = Box<Expr<Span>>;
    type ConstValue = Box<Expr<Span>>;

    fn semantic_hint(&self) -> SemanticHint {
        SemanticHint::None
    }

    fn is_known_type(type_name: &TypeName<Self>) -> bool {
        *type_name != TypeName::Unspecified(UncheckedType)
    }
}

// for external tools e.g. syntax highlighting
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum SemanticHint {
    None,
    Variable,
    Function,
    Method,
    Const,
    Type,
    Enum,
    Number,
    Variant,
    VariantCase,
    Namespace,
    String,
    Property,
    Parameter,
    TypeParameter,
}
