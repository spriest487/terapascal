pub mod block;
pub mod call;
mod case;
pub mod cast;
pub mod cond;
pub mod ctor;
pub mod expression;
pub mod function;
mod ident;
pub mod iter;
pub mod keyword;
mod match_block;
pub mod op;
pub mod operators;
pub mod raise;
pub mod statement;
pub mod tag;
pub mod type_constraint;
mod type_list;
mod type_name;
mod type_name_pattern;
pub mod typedecl;
pub mod unit;
pub mod util;

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
use std::hash::Hasher;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
pub use type_constraint::*;
pub use type_list::*;
pub use type_name::*;
pub use type_name_pattern::*;
pub use typedecl::*;
pub use unit::*;

pub trait FunctionName<A: Annotation>:
    fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash
{
    fn ident(&self) -> &Ident;

    fn owning_type_qualifier(&self) -> Option<TypeName<A>>;

    fn type_params_len(&self) -> usize;
    fn type_param_span(&self, index: usize) -> &Span;
}

pub trait ConstExprValue<A: Annotation, Val>:
    Spanned + fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash
{
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

pub enum PatternSemanticElement<A>
where
    A: Annotation,
{
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

    fn is_known_type(typename: &TypeName<Self>) -> bool;

    // typenames are handled differently depending on the AST node type - in the untyped AST
    // their identity for equality/hashing and their displayed representation are based only on
    // their syntactical elements. in the typed AST, their syntactical representation doesn't
    // matter, only the canonical type they reference
    fn typename_eq(typename_a: &TypeName<Self>, typename_b: &TypeName<Self>) -> bool;
    fn typename_hash<H: Hasher>(typename: &TypeName<Self>, state: &mut H);
    fn typename_fmt(f: &mut fmt::Formatter, typename: &TypeName<Self>) -> fmt::Result;

    fn type_semantic_hint(ty: &Self::Type) -> SemanticHint;
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

    fn is_known_type(type_name: &TypeName) -> bool {
        *type_name != TypeName::Unspecified(UncheckedType)
    }

    fn typename_eq(typename_a: &TypeName, typename_b: &TypeName) -> bool {
        match (typename_a, typename_b) {
            (TypeName::Unspecified(name), TypeName::Unspecified(other_name)) => name.eq(other_name),
            (TypeName::Ident(name), TypeName::Ident(other_name)) => name.eq(other_name),
            (TypeName::Array(name), TypeName::Array(other_name)) => name.eq(other_name),
            (TypeName::Weak(name), TypeName::Weak(other_name)) => name.eq(other_name),
            (TypeName::Function(name), TypeName::Function(other_name)) => name.eq(other_name),
            _ => false,
        }
    }

    fn typename_hash<H: Hasher>(typename: &TypeName, state: &mut H) {
        match typename {
            TypeName::Ident(ident_type_name) => ident_type_name.hash(state),
            TypeName::Array(array_type_name) => array_type_name.hash(state),
            TypeName::Function(func_type_name) => func_type_name.hash(state),
            TypeName::Weak(weak_type_name, ..) => weak_type_name.hash(state),
            TypeName::Unspecified(ty) => ty.hash(state),
        }
    }

    fn typename_fmt(f: &mut fmt::Formatter, typename: &TypeName) -> fmt::Result {
        match typename {
            TypeName::Ident(ident_type_name) => write!(f, "{}", ident_type_name),
            TypeName::Array(array_type_name) => write!(f, "{}", array_type_name),
            TypeName::Function(func_type_name) => write!(f, "{}", func_type_name),
            TypeName::Weak(type_name, ..) => write!(f, "weak {}", type_name.type_name),
            TypeName::Unspecified(..) => write!(f, "<unknown type>"),
        }
    }

    fn type_semantic_hint(_ty: &Self::Type) -> SemanticHint {
        SemanticHint::Type
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
