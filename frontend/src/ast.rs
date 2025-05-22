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

pub use block::*;
pub use call::*;
pub use case::CaseBlock;
pub use case::CaseBranch;
pub use case::CaseExpr;
pub use case::CaseStmt;
pub use cast::Cast;
use common::span::Span;
use common::span::Spanned;
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
pub use type_constraint::*;
pub use type_list::*;
pub use type_name::*;
pub use type_name_pattern::*;
pub use typedecl::*;
pub use unit::*;

pub trait TypeAnnotation : fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn is_known(&self) -> bool;
}

pub trait FunctionName : fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash {
    fn ident(&self) -> &Ident;
    fn type_params_len(&self) -> usize;
}

pub trait Annotation: Spanned + Clone + PartialEq + Eq + Hash {
    type Type: TypeAnnotation + fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
    type Name: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
    type Pattern: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
    type FunctionName: FunctionName + fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;

    type ConstStringExpr: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
    type ConstIntegerExpr: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;

    type ConstExpr: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash;
}

impl Annotation for Span {
    type Type = TypeName;
    type Name = TypeDeclName;
    type Pattern = TypeNamePattern;
    type FunctionName = QualifiedFunctionName;

    type ConstStringExpr = Box<Expr<Span>>;
    type ConstIntegerExpr = Box<Expr<Span>>;
    type ConstExpr = Box<Expr<Span>>;
}

