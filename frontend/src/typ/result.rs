use crate::ast;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Operator;
use crate::ast::{Access, FunctionDeclKind};
use crate::parse::IllegalStatement;
use crate::typ::annotation::Value;
use crate::typ::ast::Call;
use crate::typ::ast::DeclMod;
use crate::typ::ast::Expr;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::Stmt;
use crate::typ::ast::TypedFunctionName;
use crate::typ::ast::VariantDef;
use crate::typ::context::NameError;
use crate::typ::GenericError;
use crate::typ::Type;
use crate::typ::ValueKind;
use crate::typ::MAX_FLAGS_BITS;
use crate::typ::FunctionSig;
use crate::IntConstant;
use common::span::*;
use common::Backtrace;
use common::DiagnosticLabel;
use common::DiagnosticMessage;
use common::DiagnosticOutput;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug)]
pub enum TypeError {
    NameError {
        err: NameError,
        span: Span
    },
    NotCallable(Box<Expr>),
    InvalidArgs {
        expected: Vec<Type>,
        actual: Vec<Type>,
        span: Span,
    },
    InvalidCallInExpression(Box<Call>),
    InvalidIndexer {
        base: Box<Expr>,
        index_ty: Type,
        span: Span,
    },
    IndexOutOfBounds {
        base_ty: Box<Type>,
        index: IntConstant,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        actual: Type,
        span: Span,
    },
    NotValueExpr {
        expected: Type,
        actual: Value,
    },
    NotMutable {
        expr: Box<Expr>,
        decl: Option<IdentPath>,
    },
    NotAddressable {
        ty: Type,
        value_kind: Option<ValueKind>,
        span: Span,
    },
    NotDerefable {
        ty: Type,
        span: Span,
    },
    NotMatchable {
        ty: Type,
        span: Span,
    },
    NotDefaultable {
        ty: Type,
        span: Span,
    },
    InvalidUnaryOp {
        op: Operator,
        operand: Type,
        span: Span,
    },
    InvalidBinOp {
        lhs: Type,
        rhs: Type,
        op: Operator,
        span: Span,
    },
    BlockOutputIsNotExpression {
        stmt: Box<Stmt>,
        expected_expr_ty: Type,
    },
    InvalidBlockOutput(Box<Expr>),
    AmbiguousSelfType {
        method: Ident,
        iface: Type,
        span: Span,
    },
    AmbiguousFunction {
        candidates: Vec<OverloadCandidate>,
        span: Span,
    },
    InvalidFunctionOverload {
        ident: Ident,
        prev_decls: Vec<Ident>,
        kind: InvalidOverloadKind,
    },
    InvalidMethodOverload {
        owning_type: Type,
        method: Ident,
        prev_decls: Vec<Ident>,
        kind: InvalidOverloadKind,
    },
    DuplicateDeclMod {
        keyword: String,
        span: Span,
        existing: Span,
    },
    IncompatibleDeclMod {
        first_keyword: String,
        second_keyword: String,
        first_span: Span,
        second_span: Span,
    },
    ExternalGenericFunction {
        func: Ident,
        extern_modifier: Span,
        ty_args: Span,
    },
    InvalidCtorType {
        ty: Type,
        span: Span,
    },
    CtorMissingMembers {
        ctor_ty: Type,
        members: Vec<Ident>,
        span: Span,
    },
    TypeHasMultipleDtors {
        owning_type: Type,
        
        new_dtor: Span,
        prev_dtor: Span,
    },
    DtorCannotHaveParams {
        span: Span,
    },
    DtorCannotHaveTypeParams {
        span: Span,  
    },
    InvalidDtorOwningType { 
        ty: Type, 
        span: Span,
    },
    MethodDeclMissingType {
        ident: Ident,
        span: Span,
        kind: FunctionDeclKind,
    },
    DuplicateParamName {
        name: Ident,
        previous: Span,
        span: Span,
    },
    DuplicateNamedArg {
        name: Ident,
        previous: Span,
        span: Span,
    },
    UndefinedSymbols {
        unit: IdentPath,
        undefined: Vec<(IdentPath, Span)>,
    },
    UnableToInferType {
        expr: Box<ast::Expr<Span>>,
    },
    UnableToInferFunctionExprType {
        func: Box<ast::AnonymousFunctionDef<Span>>,
    },
    UnableToInferSpecialization {
        generic_ty: Type,
        hint_ty: Type,
        span: Span,
    },
    UninitGlobalBinding {
        ident: Ident,
        ty: Type,
    },
    UninitBindingWithNoType {
        binding: Box<ast::LocalBinding<Span>>,
    },
    ConstDeclWithNoValue {
        span: Span,
    },
    BindingWithNoType {
        binding_name: Ident,
        span: Span,
    },
    NotInitialized {
        ident: Ident,
        usage: Span,
    },
    InvalidRefExpression {
        expr: Box<Expr>,
    },
    InvalidStatement(IllegalStatement<Value>),
    
    InvalidSetValueType {
        actual: Type,
        span: Span,
    },
    SetValuesMustBeSequential {
        from: IntConstant,
        to: IntConstant,
        span: Span,
    },
    EmptySetDecl {
        name: Option<IdentPath>,
        span: Span,
    },
    TooManySetValues {
        count: usize,
        span: Span,
    },

    EmptyVariantDecl(Box<ast::VariantDecl<Span>>),
    EmptyVariantCaseBinding {
        variant: Box<VariantDef>,
        case_index: usize,
        span: Span,
    },

    NoLoopContext {
        stmt: Box<ast::Stmt<Span>>,
    },
    NoFunctionContext {
        stmt: Box<ast::Stmt<Span>>,
    },

    InvalidWeakType {
        ty: Type,
        span: Span,
    },
    InvalidUnsizedType {
        ty: Type,
        span: Span,
    },

    InvalidMethodModifiers {
        mods: Vec<DeclMod>,
        span: Span
    },
    InvalidMethodOwningType {
        method_ident: Ident,
        span: Span,
    },
    NoMethodContext {
        method_ident: Ident,
        span: Span,
    },
    MethodDefMismatch {
        owning_ty: Type,
        method_ident: Ident,
        decls: Vec<MismatchedMethodDecl>,
        actual_kind: FunctionDeclKind,
        span: Span,
    },

    InvalidMethodInstanceType {
        ty: Type,
        span: Span,
    },
    
    InvalidImplementation {
        ty: Type,
        missing: Vec<MissingImplementation>,
        mismatched: Vec<MismatchedImplementation>,
        span: Span,
    },

    InvalidBaseType { 
        ty: Type,
        invalid_base_ty: Type,
        reason: InvalidBaseTypeReason,
        span: Span,
    },

    AbstractMethodDefinition {
        owning_ty: Type,
        method: Ident,
        span: Span,
    },

    NameNotVisible {
        name: IdentPath,
        span: Span,
    },

    TypeMemberInaccessible {
        ty: Type,
        member: Ident,
        access: Access,
        span: Span,
    },

    UnsafeConversionNotAllowed {
        from: Type,
        to: Type,
        span: Span,
    },
    UnsafeAddressOfNotAllowed {
        ty: Type,
        span: Span,
    },
    InvalidConstExpr {
        expr: Box<Expr>,
    },

    InvalidCaseExprBlock {
        span: Span,
    },

    InvalidCast {
        from: Type,
        to: Type,
        span: Span,
    },

    EmptyMatchBlock {
        span: Span,
    },
    MatchExprNotExhaustive {
        span: Span,
        missing_cases: Vec<Ident>,
    },

    InvalidExitWithValue {
        span: Span,
    },

    InvalidLoopCounterType {
        counter_ty: Type,
        span: Span,
    },
    InvalidLoopSeqType {
        seq_ty: Type,
        span: Span,
    },

    InvalidDeclWithTypeParams {
        span: Span,
        kind: InvalidTypeParamsDeclKind,
    },
    EnumValuesMustBeAscending {
        span: Span,
        prev_ident: Ident,
        prev_val: i128,

        next_ident: Ident,
        next_val: i128,
    },
    
    InvalidExplicitSpec {
        target: Value,
    },

    InvalidTagItem {
        reason: InvalidTagReason,
        span: Span
    },
}

impl TypeError {
    pub fn from_name_err(err: NameError, span: Span) -> Self {
        TypeError::NameError {
            err,
            span
        }
    }

    pub fn from_generic_err(err: GenericError, span: Span) -> Self {
        TypeError::NameError {
            err: NameError::GenericError(err),
            span,
        }
    }
    
    pub fn name_not_found(name: impl Into<IdentPath>, span: impl Into<Span>) -> Self {
        TypeError::from_name_err(NameError::NotFound { ident: name.into() }, span.into())
    }
}

pub type TypeResult<T> = Result<T, TypeError>;

impl Spanned for TypeError {
    fn span(&self) -> &Span {
        match self {
            TypeError::NameError { span, .. } => span,
            TypeError::NotCallable(expr) => expr.annotation().span(),
            TypeError::InvalidArgs { span, .. } => span,
            TypeError::InvalidCallInExpression(call) => call.annotation().span(),
            TypeError::InvalidIndexer { span, .. } => span,
            TypeError::IndexOutOfBounds { span, .. } => span,
            TypeError::TypeMismatch { span, .. } => span,
            TypeError::NotMutable { expr, .. } => expr.annotation().span(),
            TypeError::NotAddressable { span, .. } => span,
            TypeError::NotDerefable { span, .. } => span,
            TypeError::NotMatchable { span, .. } => span,
            TypeError::NotDefaultable { span, .. } => span,
            TypeError::NotValueExpr { actual, .. } => actual.span(),
            TypeError::InvalidBinOp { span, .. } => span,
            TypeError::InvalidUnaryOp { span, .. } => span,
            TypeError::BlockOutputIsNotExpression { stmt, .. } => stmt.annotation().span(),
            TypeError::InvalidBlockOutput(expr) => expr.annotation().span(),
            TypeError::AmbiguousFunction { span, .. } => span,
            TypeError::ExternalGenericFunction { func, .. } => func.span(),
            
            TypeError::DuplicateDeclMod { span, .. } => span,
            TypeError::IncompatibleDeclMod { second_span, .. } => second_span,
            
            TypeError::AmbiguousSelfType { span, .. } => span,
            TypeError::InvalidFunctionOverload { ident, .. } => ident.span(),
            TypeError::InvalidMethodOverload { method, .. } => method.span(),
            
            TypeError::InvalidCtorType { span, .. } => span,
            TypeError::CtorMissingMembers { span, .. } => span,
            TypeError::MethodDeclMissingType { span, .. } => span,
            TypeError::TypeHasMultipleDtors { new_dtor, .. } => new_dtor,
            TypeError::DtorCannotHaveParams { span } => span,
            TypeError::DtorCannotHaveTypeParams { span } => span,
            TypeError::InvalidDtorOwningType { span, .. } => span,
            TypeError::DuplicateNamedArg { span, .. } => span,
            TypeError::DuplicateParamName { span, .. } => span,
            
            TypeError::UndefinedSymbols { unit, .. } => unit.span(),
            TypeError::UnableToInferType { expr } => expr.annotation().span(),
            TypeError::UnableToInferFunctionExprType { func } => func.span(),
            TypeError::UnableToInferSpecialization { span, .. } => span,
            TypeError::UninitGlobalBinding { ident, .. } => ident.span(),
            TypeError::UninitBindingWithNoType { binding } => binding.annotation.span(),
            TypeError::BindingWithNoType { span, .. } => span,
            TypeError::NotInitialized { usage, .. } => usage.span(),
            TypeError::InvalidRefExpression { expr } => expr.annotation().span(),
            TypeError::InvalidStatement(expr) => expr.0.annotation().span(),
            
            TypeError::InvalidSetValueType { span, .. } => span,
            TypeError::SetValuesMustBeSequential { span, .. } => span,
            TypeError::TooManySetValues { span, .. } => span,
            TypeError::EmptySetDecl { span, .. } => span,

            TypeError::EmptyVariantDecl(variant) => variant.span(),
            TypeError::EmptyVariantCaseBinding { span, .. } => span,
            
            TypeError::NoLoopContext { stmt, .. } => stmt.annotation().span(),
            TypeError::NoFunctionContext { stmt, .. } => stmt.annotation().span(),
            TypeError::InvalidWeakType { span: at, .. } => at,
            TypeError::InvalidUnsizedType { span: at, .. } => at,
            
            TypeError::InvalidMethodModifiers { span, .. } => span,
            TypeError::InvalidMethodOwningType { span, .. } => span,
            TypeError::InvalidMethodInstanceType { span, .. } => span,
            TypeError::NoMethodContext { span, .. } => span,
            TypeError::MethodDefMismatch { span, .. } => span,
            TypeError::InvalidImplementation { span, .. } => span,
            TypeError::AbstractMethodDefinition { span, .. } => span,
            TypeError::InvalidBaseType { span, .. } => span,
            TypeError::NameNotVisible { span, .. } => span,
            TypeError::TypeMemberInaccessible { span, .. } => span,
            
            TypeError::UnsafeConversionNotAllowed { span, .. } => span,
            TypeError::UnsafeAddressOfNotAllowed { span, .. } => span,
            TypeError::InvalidConstExpr { expr } => expr.span(),
            TypeError::InvalidCaseExprBlock { span } => span,
            TypeError::InvalidCast { span, .. } => span,
            TypeError::EmptyMatchBlock { span, .. } => span,
            TypeError::MatchExprNotExhaustive { span, .. } => span,
            TypeError::InvalidExitWithValue { span, .. } => span,
            TypeError::ConstDeclWithNoValue { span, .. } => span,
            TypeError::InvalidLoopCounterType { span, .. } => span,
            TypeError::InvalidLoopSeqType { span, .. } => span,
            TypeError::InvalidDeclWithTypeParams { span, .. } => span,
            TypeError::EnumValuesMustBeAscending { span, .. } => span,
            TypeError::InvalidExplicitSpec { target, .. } => target.span(),
            TypeError::InvalidTagItem { span, .. } => span,
        }
    }
}

impl DiagnosticOutput for TypeError {
    fn title(&self) -> String {
        String::from(match self {
            TypeError::NameError { err, ..  } => match err {
                NameError::NotFound { .. } => "Name not found",
                NameError::MemberNotFound { .. } => "Named member not found",
                NameError::Unexpected { .. } => "Name had unexpected type",
                NameError::AlreadyDeclared { .. } => "Name already declared",
                NameError::AlreadyDefined { .. } => "Name already defined",
                NameError::Ambiguous { .. } => "Name is ambiguous",
                NameError::NoImplementationFound { .. } => "No implementation found",
                NameError::AlreadyImplemented { .. } => "Method already implemented",
                NameError::DefDeclMismatch { .. } => {
                    "Definition does not match previous declaration"
                }
                NameError::GenericError(err) => match err {
                    GenericError::ArgsLenMismatch { .. } => "Wrong number of type arguments",
                    GenericError::ParamMismatch { .. } => "Parameter list mismatch",
                    GenericError::ConstraintNotSatisfied { .. } => "Type parameter constraint not satisfied by argument",
                    GenericError::UnexpectedConstraintList => "Unexpected constraint list",
                    GenericError::UnexpectedConstraint { .. } => "Unexpected constraint",
                    GenericError::DuplicateConstraint { .. } => "Duplicate constraint",
                    GenericError::CannotInferArgs { .. } => "Cannot infer type arguments",
                    GenericError::IllegalUnspecialized { .. } => "Illegal use of unspecialized type",
                }
            }
            TypeError::NotCallable(_) => "Not callable",
            TypeError::InvalidArgs { .. } => "Invalid arguments",
            TypeError::InvalidCallInExpression(_) => "Invalid call in expression",
            TypeError::InvalidIndexer { .. } => "Invalid indexer expression",
            TypeError::IndexOutOfBounds { .. } => "Index out of bounds",
            TypeError::TypeMismatch { .. } => "Type mismatch",
            TypeError::NotMutable { .. } => "Value not mutable",
            TypeError::NotAddressable { .. } => "Value not addressable",
            TypeError::NotDerefable { .. } => "Value cannot be dereferenced",
            TypeError::NotMatchable { .. } => "Type is not matchable",
            TypeError::NotDefaultable { .. } => "Type has no default value",
            TypeError::NotValueExpr { .. } => "Expected value expression",
            TypeError::InvalidBinOp { .. } => "Invalid binary operation",
            TypeError::InvalidUnaryOp { .. } => "Invalid unary operation",
            TypeError::BlockOutputIsNotExpression { .. } => "Expected block output expression",
            TypeError::InvalidBlockOutput(_) => "Invalid block output expression",
            TypeError::AmbiguousFunction { .. } => "Function reference is ambiguous",
            TypeError::AmbiguousSelfType { .. } => "Self type of method is ambiguous",
            TypeError::ExternalGenericFunction { .. } => "Function imported from external module may not have type parameters",
            
            TypeError::DuplicateDeclMod { .. } => "Duplicate modifier",
            TypeError::IncompatibleDeclMod { .. } => "Incompatible modifiers",
            
            TypeError::InvalidCtorType { .. } => {
                "Invalid constructor expression type"
            }
            TypeError::CtorMissingMembers { .. } => {
                "Constructor is missing one or more named members"
            }
            TypeError::TypeHasMultipleDtors { .. } => {
                "Type has multiple destructors"
            }
            TypeError::DtorCannotHaveParams { .. } => {
                "Destructor cannot have parameters"
            }
            TypeError::DtorCannotHaveTypeParams { .. } => {
                "Destructor cannot have type parameters"
            }
            TypeError::InvalidDtorOwningType { .. } => {
                "Type cannot have destructor"
            }
            TypeError::MethodDeclMissingType { .. } => {
                "Method declaration missing type specification"
            }
            TypeError::UndefinedSymbols { .. } => "Undefined symbol(s)",
            TypeError::UnableToInferType { .. } => {
                "Unable to infer type of expression"
            }
            TypeError::UnableToInferFunctionExprType { .. } => {
                "Unable to infer function type of function expression"
            }
            TypeError::UnableToInferSpecialization { .. } => {
                "Unable to infer type specialization"
            }
            TypeError::UninitGlobalBinding { .. } => {
                "Uninitialized global variable"
            }
            TypeError::UninitBindingWithNoType { .. } => {
                "Uninitialized variable must have an explicit type"
            }
            TypeError::BindingWithNoType { .. } => {
                "Value bound to name must have a type"
            }
            TypeError::NotInitialized { .. } => "Use of uninitialized value",
            TypeError::InvalidRefExpression { .. } => {
                "Invalid reference expression"
            }
            TypeError::InvalidStatement(invalid_stmt) => return invalid_stmt.title(),
            
            TypeError::InvalidSetValueType { .. } => "Invalid set value type",
            TypeError::SetValuesMustBeSequential { .. } => "Set values must be sequential",
            TypeError::EmptySetDecl { .. } => "Empty set declaration",
            TypeError::TooManySetValues { .. } => "Set contains too many values",

            TypeError::EmptyVariantDecl(..) => "Empty variant declaration",
            TypeError::EmptyVariantCaseBinding { .. } => {
                "Empty variant case binding"
            }

            TypeError::NoLoopContext { .. } => "Statement requires loop context",
            TypeError::NoFunctionContext { .. } => "Statement requires function context",

            TypeError::InvalidWeakType { .. } => "Invalid weak reference type",
            TypeError::InvalidUnsizedType { .. } => "Size of type is unknown",
            
            TypeError::InvalidMethodOwningType { .. } => "Explicit interface implementation for method is invalid",
            TypeError::InvalidMethodModifiers { .. } => "Invalid method modifier(s)",

            TypeError::InvalidMethodInstanceType { .. } => {
                "Invalid instance type for method"
            },
            
            TypeError::InvalidImplementation { .. } => {
                "Incomplete interface implementation"
            }

            TypeError::InvalidFunctionOverload { .. } => "Invalid function overload",
            TypeError::InvalidMethodOverload { .. } => "Invalid method overload",

            TypeError::InvalidBaseType { .. } => {
                "Invalid base type"
            }
            
            TypeError::AbstractMethodDefinition { .. } => "Cannot define abstract method",

            TypeError::NoMethodContext { .. } => "Method requires enclosing type",
            TypeError::MethodDefMismatch { .. } => "Method definition mismatch",

            TypeError::NameNotVisible { .. } => "Name is not visible",
            
            TypeError::TypeMemberInaccessible { .. } => "Member is inaccessible",

            TypeError::DuplicateParamName { .. } => "Duplicate parameter name",
            TypeError::DuplicateNamedArg { .. } => "Duplicate named argument",
            TypeError::UnsafeConversionNotAllowed { .. } => "Conversion not allowed in a safe context",
            TypeError::UnsafeAddressOfNotAllowed { .. } => "Address operator not allowed on this type in a safe context",

            TypeError::InvalidConstExpr { .. } => "Invalid constant expression",

            TypeError::InvalidCaseExprBlock { .. } => "Case block invalid as expression",

            TypeError::InvalidCast { .. } => "Invalid cast",
            TypeError::EmptyMatchBlock { .. } => "Empty match block",
            TypeError::MatchExprNotExhaustive { .. } => "Match expression is not exhaustive",
            TypeError::InvalidExitWithValue { .. } => "Invalid exit with value",
            TypeError::ConstDeclWithNoValue { .. } => "Constant declaration without a value",
            TypeError::InvalidLoopCounterType { .. } => "Invalid loop counter type",
            TypeError::InvalidLoopSeqType { .. } => "Invalid loop sequence type",
            TypeError::InvalidDeclWithTypeParams { .. } => "Invalid type declared with type params",
            TypeError::EnumValuesMustBeAscending { .. } => "Enumeration values must be ascending",
            TypeError::InvalidExplicitSpec { .. } => "Invalid explicit specialization",
            
            TypeError::InvalidTagItem { .. } => "Invalid tag type",
        })
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            _ => Some(DiagnosticLabel {
                text: Some(self.to_string()),
                span: self.span().clone(),
            }),
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            TypeError::NameError { err, .. } => match err {
                NameError::AlreadyDeclared { new, existing, existing_kind, .. } => {
                    if *existing.span().file.as_ref() == PathBuf::from("<builtin>") {
                        // don't show this message for conflicts with builtin identifiers
                        return Vec::new()
                    }

                    vec![DiagnosticMessage {
                        title: format!("{} `{}` previously declared here", existing_kind, new),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: existing.span().clone(),
                        }),
                        notes: Vec::new(),
                    }]
                }

                NameError::AlreadyDefined { ident, existing } => vec![DiagnosticMessage {
                    title: format!("`{}` previously defined here", ident),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: existing.span().clone(),
                    }),
                    notes: Vec::new(),
                }],

                NameError::AlreadyImplemented {
                    owning_ty: iface,
                    method,
                    existing,
                    ..
                } => vec![DiagnosticMessage {
                    title: format!("`{}.{}` previously implemented here", iface, method),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: existing.clone(),
                    }),
                    notes: Vec::new(),
                }],

                NameError::Ambiguous { ident, options } => {
                    let mut see_also: Vec<_> = options
                        .iter()
                        .map(|option| DiagnosticMessage {
                            title: format!("`{}` could refer to `{}`", ident, option.join(".")),
                            label: Some(DiagnosticLabel {
                                text: None,
                                span: option.last().span().clone(),
                            }),
                            notes: Vec::new(),
                        })
                        .collect();
                    see_also.sort();
                    see_also
                }

                NameError::DefDeclMismatch { def, decl, path: ident } => vec![
                    DiagnosticMessage {
                        title: format!("Previous declaration of `{}`", ident),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: decl.clone(),
                        }),
                        notes: Vec::new(),
                    },
                    DiagnosticMessage {
                        title: format!("Conflicting definition of `{}`", ident),
                        label: Some(DiagnosticLabel {
                            text: None,
                            span: def.clone(),
                        }),
                        notes: Vec::new(),
                    },
                ],

                _ => Vec::new(),
            },

            TypeError::UndefinedSymbols { undefined: syms, .. } => syms
                .iter()
                .map(|(path, span)| DiagnosticMessage {
                    title: format!("`{}` is declared but not defined", path),
                    label: Some(DiagnosticLabel {
                        text: Some("declared here".to_string()),
                        span: span.clone(),
                    }),
                    notes: Vec::new(),
                })
                .collect(),

            TypeError::NotInitialized { ident, .. } => vec![
                DiagnosticMessage::new(format!("uninitialized value `{}`", ident))
                    .with_label(DiagnosticLabel::new(ident.span().clone())
                        .with_text("declared here")),
            ],

            TypeError::NotMutable {
                decl: Some(decl),
                ..
            } => vec![DiagnosticMessage {
                title: "modifying immutable value".to_string(),
                label: Some(DiagnosticLabel {
                    text: Some("declared as immutable here".to_string()),
                    span: decl.span().clone(),
                }),
                notes: Vec::new(),
            }],
            
            TypeError::DuplicateDeclMod { existing, .. } => vec![DiagnosticMessage {
                title: "duplicate modifier".to_string(),
                label: Some(DiagnosticLabel {
                    text: Some("previously appeared here".to_string()),
                    span: existing.clone(),
                }),
                notes: Vec::new(),
            }],
            
            TypeError::TypeHasMultipleDtors { new_dtor, prev_dtor, .. } => vec![
                DiagnosticMessage::new("new destructor declaration")
                    .with_label(DiagnosticLabel::new(new_dtor.clone())),
                DiagnosticMessage::new("previous destructor declaration")
                    .with_label(DiagnosticLabel::new(prev_dtor.clone())
                        .with_text("previously declared here")),
            ],

            TypeError::IncompatibleDeclMod { first_span, first_keyword, .. } => vec![DiagnosticMessage {
                title: "incompatible modifier".to_string(),
                label: Some(DiagnosticLabel {
                    text: Some(format!("`{first_keyword}` previously appeared here")),
                    span: first_span.clone(),
                }),
                notes: Vec::new(),
            }],

            TypeError::DuplicateNamedArg { name, previous, .. } 
            | TypeError::DuplicateParamName { name, previous, .. } => 
                vec![DiagnosticMessage {
                    title: format!("previous occurrence of `{}`", name),
                    label: Some(DiagnosticLabel {
                        text: None,
                        span: previous.clone(),
                    }),
                    notes: Vec::new(),
                }],

            TypeError::AmbiguousFunction { candidates, .. } => {
                candidates
                    .iter()
                    .map(|c| {
                        DiagnosticMessage {
                            label: Some(DiagnosticLabel {
                                span: c.span().clone(),
                                text: None,
                            }),
                            title: format!("may refer to {}", c),
                            notes: Vec::new(),
                        }
                    })
                    .collect()
            }

            TypeError::InvalidMethodOverload { prev_decls, .. }
            | TypeError::InvalidFunctionOverload { prev_decls, .. } => {
                prev_decls
                    .iter()
                    .map(|ident| {
                        DiagnosticMessage::new(format!("{} previously declared here", ident))
                            .with_label(DiagnosticLabel::new(ident.span.clone()))
                    })
                    .collect()
            }
            
            TypeError::MethodDefMismatch { decls, .. } => {
                decls
                    .iter()
                    .map(|decl| {
                        DiagnosticMessage::new(format!("declaration of {} {} here", decl.kind, decl.name))
                            .with_label(DiagnosticLabel::new(decl.span.clone()))
                    })
                    .collect()
            }
            
            TypeError::InvalidImplementation { missing, mismatched, .. } => {
                let mut messages = Vec::new();

                messages.extend(mismatched
                    .iter()
                    .flat_map(|i| {
                        vec![
                            DiagnosticMessage {
                                title: format!(
                                    "{} declared here ({})", 
                                    i.iface_method_name, 
                                    i.expect_sig
                                ),
                                label: Some(DiagnosticLabel {
                                    text: None,
                                    span: i.iface_method_name.span.clone(),
                                }),
                                notes: Vec::new(),
                            },
                            DiagnosticMessage {
                              title: format!("mismatched implementation here {} ({})", i.impl_method_name, i.actual_sig),
                                label: Some(DiagnosticLabel {
                                    text: None,
                                    span: i.impl_method_name.span.clone(),
                                }),
                                notes: Vec::new(),
                            },
                        ]
                    }));
                    
                messages.extend(missing
                    .iter()
                    .map(|i| {
                        DiagnosticMessage {
                            title: format!("{} declared here", i.method_name),
                            label: Some(DiagnosticLabel {
                                text: None,
                                span: i.method_name.span.clone(),
                            }),
                            notes: Vec::new(),
                        }
                    }));
                messages
                            
            }

            _ => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        None
    }
}

fn write_args<'a>(f: &mut fmt::Formatter, args: impl IntoIterator<Item = &'a Type>) -> fmt::Result {
    for (i, arg) in args.into_iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{}", arg)?;
    }
    Ok(())
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::NameError { err, .. } => write!(f, "{}", err),
            TypeError::NotCallable(expr) => {
                write!(f, "{} `{}` of type `{}` is not a callable function", expr.name(), expr, expr.annotation().ty())
            }

            TypeError::InvalidArgs { expected, actual, .. } => {
                write!(f, "expected arguments (")?;
                write_args(f, expected.iter())?;
                write!(f, "), found (")?;
                write_args(f, actual.iter())?;
                write!(f, ")")
            }

            TypeError::InvalidCallInExpression(call) => {
                write!(f, "function call `{}` returns no value and cannot be used as part of an expr", call)
            }

            TypeError::InvalidIndexer { base, index_ty, .. } => {
                let base_ty = base.annotation().ty();
                write!(f, "`{}` cannot be used as an index for `{}` of type `{}`", index_ty, base, base_ty)
            }

            TypeError::IndexOutOfBounds { base_ty, index, .. } => {
                write!(f, "index value `{}` is out of range for type `{}`", index, base_ty)
            }

            TypeError::TypeMismatch { expected, actual, .. } => {
                write!(f, "type mismatch: expected {}, found {}", expected, actual)
            }

            TypeError::NotMutable { expr, .. } => {
                write!(f, "`{}` does not refer to a mutable value", expr)
            }

            TypeError::NotAddressable { ty, value_kind, .. } => {
                match value_kind {
                    Some(value_kind) => write!(f, "{} of type {} cannot have its address taken", value_kind, ty),
                    None => write!(f, "expr without a value cannot have its address taken"),
                }
            }

            TypeError::NotDerefable { ty, .. } => {
                write!(f, "value of type `{}` cannot be dereferenced", ty)
            }

            TypeError::NotMatchable { ty, .. } => {
                write!(f, "type `{}` cannot be used in matching constructs", ty)
            }
            
            TypeError::NotDefaultable { ty, .. } => {
                write!(f, "type `{}` does not have a default value in this context", ty)
            }
            
            TypeError::NotValueExpr { expected, actual, .. } => {
                write!(f, "expected value of type `{}`, found `{}`", expected, actual)
            }

            TypeError::InvalidBinOp { lhs, rhs, op, .. } => {
                match op {
                    Operator::Assignment => {
                        write!(f, "`{}` is not assignable to `{}`", rhs, lhs)
                    },
                    _ => {
                        write!(f, "operator {} cannot be applied to the operand types `{}` and `{}`", op, lhs, rhs)
                    }
                }
            }

            TypeError::InvalidUnaryOp { operand, op, .. } => {
                write!(f, "operator {} cannot be applied to an operand of type `{}`", op, operand)
            }

            TypeError::BlockOutputIsNotExpression { stmt, expected_expr_ty } => {
                write!(f, "expected expr of type `{}` but found stmt `{}`", expected_expr_ty, stmt)
            }

            TypeError::InvalidBlockOutput(expr) => {
                write!(f, "expr `{}` is not valid as the final expr in a block because it is not a complete stmt", expr)
            }

            TypeError::AmbiguousFunction { .. } => {
                write!(f, "call to function was ambiguous")
            }

            TypeError::AmbiguousSelfType { method, iface, .. } => {
                write!(f, "the type implementing `{}` could not be deduced for `{}` in this context", iface, method)
            }
            
            TypeError::InvalidFunctionOverload { ident, kind, .. } => {
                write!(f, "this overload of function `{}` is not valid: {}", ident, kind)
            }

            TypeError::InvalidMethodOverload { owning_type, method, kind, .. } => {
                write!(f, "this overload of method `{}.{}` is not valid: {}", owning_type, method, kind)
            }

            TypeError::ExternalGenericFunction { func, .. } => {
                write!(f, "`{}` is generic but is declared with the `{}` modifier", func, DeclMod::EXTERNAL_WORD)
            }
            
            TypeError::DuplicateDeclMod { keyword, .. } => {
                write!(f, "the modifier `{keyword}` cannot appear multiple times on this declaration")
            }
            
            TypeError::IncompatibleDeclMod { first_keyword, second_keyword, .. } => {
                write!(f, "the modifiers `{first_keyword}` and `{second_keyword}` cannot appear on the same declaration")
            }

            TypeError::InvalidCtorType { ty, .. } => {
                write!(f, "type `{}` cannot be created with a constructor expr", ty)
            }

            TypeError::CtorMissingMembers { ctor_ty, members, .. } => {
                write!(f, "the following members are missing from the constructor for type `{}`: ", ctor_ty)?;
                for (i, member) in members.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", member)?;
                }
                Ok(())
            }
            
            TypeError::TypeHasMultipleDtors { owning_type, .. } => {
                write!(f, "type `{}` has already declared a destructor", owning_type)
            }
            
            TypeError::DtorCannotHaveParams { .. } => {
                write!(f, "destructor must be declared without parameters")
            }
            
            TypeError::DtorCannotHaveTypeParams { .. } => {
                write!(f, "destructor must be declared without type parameters")
            }
            
            TypeError::InvalidDtorOwningType { ty, .. } => {
                write!(f, "{} `{}` cannot have a destructor", ty.kind_description(), ty)
            }

            TypeError::MethodDeclMissingType { kind, ident, .. } => {
                write!(f, "{} definition `{}` must belong to a type", kind, ident)
            }

            TypeError::UndefinedSymbols { unit, .. } => {
                write!(f, "unit `{}` contains undefined symbols", unit)
            }

            TypeError::UnableToInferType { expr } => {
                write!(f, "unable to infer the type of `{}`", expr)
            }

            TypeError::UnableToInferFunctionExprType { .. } => {
                write!(f, "unable to infer the type of this function expression from the usage")
            }

            TypeError::UnableToInferSpecialization { generic_ty, hint_ty, .. } => {
                write!(f, "unable to infer specialization of the generic type `{}` from expected type `{}`", generic_ty, hint_ty)
            }

            TypeError::UninitGlobalBinding { ident, ty } => {
                write!(f, "global variable `{}` of type `{}` does not have a default value and must be initialized", ident, ty)
            }

            TypeError::UninitBindingWithNoType { binding } => {
                write!(f, "the type of `{}` cannot be inferred because it has no initial value", binding.name)
            }

            TypeError::BindingWithNoType { binding_name, .. } => {
                write!(f, "the type of value bound to `{}` cannot be Nothing", binding_name)
            }

            TypeError::NotInitialized { ident, .. } => {
                write!(f, "`{}` is used before initialization", ident)
            }

            TypeError::InvalidRefExpression { expr } => {
                write!(f, "`{}` does not refer to a value that can be passed by reference", expr)
            }

            TypeError::InvalidStatement(invalid_stmt) => {
                write!(f, "{}", invalid_stmt)
            }
            
            TypeError::InvalidSetValueType { actual: actual_ty, .. } => {
                write!(f, "type `{}` is not an integer type or enum", actual_ty)
            }

            TypeError::SetValuesMustBeSequential { from, to, .. } => {
                write!(f, "{from} is greater than {to}")
            }

            TypeError::EmptySetDecl { name, .. } => {
                match name {
                    Some(path) => write!(f, "set declaration `{path}` contains no values"),
                    None => write!(f, "set declaration contains no values")
                }
            }
            
            TypeError::TooManySetValues { count, ..  } => {
                write!(f, "set type contains {count} values, which is more than the maximum number of flag bits ({MAX_FLAGS_BITS})")
            }

            TypeError::EmptyVariantDecl(variant) => {
                write!(f, "variant declaration `{}` has no cases", variant.name)
            }

            TypeError::EmptyVariantCaseBinding { variant, case_index, .. } => {
                let case_ident = &variant.cases[*case_index].ident;
                write!(f, "cannot bind value of empty variant case `{}.{}`", variant.name, case_ident)
            }

            TypeError::NoLoopContext { stmt } => {
                write!(f, "the stmt `{}` can only appear inside a loop", stmt)
            }

            TypeError::NoFunctionContext { stmt } => {
                write!(f, "the stmt `{}` can only appear inside a function", stmt)
            }

            TypeError::InvalidWeakType { ty, .. } => {
                write!(f, "the type `{}` cannot be used as a weak reference", ty)
            }

            TypeError::InvalidUnsizedType { ty, .. } => {
                write!(f, "the type `{}` cannot be used because its size is unknown in this context", ty)
            }

            TypeError::InvalidMethodModifiers { mods, .. } => {
                if mods.len() > 1 {
                    write!(f, "the following modifiers can not appear on a method declaration:")?;
                    write!(f, "{}", mods.iter()
                        .map(|m| format!("`{}`", m))
                        .collect::<Vec<_>>()
                        .join(", "))
                } else {
                    write!(f, "the modifier `{}` can not appear on a method declaration", mods[0])
                }
            }

            TypeError::InvalidMethodOwningType { method_ident, .. } => {
                write!(f, "method `{}` declared in this scope cannot explicitly implement an interface", method_ident)
            }
            
            TypeError::NoMethodContext { method_ident, .. } => {
                write!(f, "method `{}` is not declared within a type", method_ident)
            }

            TypeError::MethodDefMismatch { method_ident, owning_ty, actual_kind, .. } => {
                writeln!(f, "definition of {} `{}.{}` does not match previous declaration", actual_kind, owning_ty, method_ident)?;
                Ok(())
            }

            TypeError::InvalidMethodInstanceType { ty, .. } => {
                write!(f, "`{}` is not a type which supports methods", ty)
            }
            
            TypeError::InvalidImplementation { ty, missing, mismatched, .. } => {
                writeln!(f, "type `{}` is missing the following methods for interfaces it implements:", ty)?;
                let names = mismatched.iter()
                    .map(|item| &item.iface_method_name)
                    .chain(missing
                        .iter()
                        .map(|item| &item.method_name))
                    .enumerate();

                for (i, name) in names {
                    if i > 0 {
                        write!(f, "\n")?;
                    }
                    write!(f, "{}", name)?;
                }
                Ok(())
            }
            
            TypeError::InvalidBaseType { ty, invalid_base_ty, reason, .. } => {
                write!(f, "`{}` is not valid as a base type for `{}`: ", invalid_base_ty, ty)?;
                
                match reason {
                    InvalidBaseTypeReason::NotInterface => write!(f, "type is not an interface")?,
                    InvalidBaseTypeReason::Forward => write!(f, "base type cannot be a forward declaration")?,
                }
                
                writeln!(f)
            }
            
            TypeError::AbstractMethodDefinition { owning_ty, method, .. } => {
                writeln!(f, "method `{}.{}` is abstract and cannot be defined", owning_ty, method)
            }

            TypeError::NameNotVisible { name, .. } => {
                write!(f, "`{}` is not is not visible in the current context", name)
            }
            
            TypeError::TypeMemberInaccessible { ty, member, access, .. } => {
                write!(f, "{} member `{}.{}` is inaccessible in this context", access, ty, member)
            }

            TypeError::DuplicateNamedArg { name, .. } => {
                write!(f, "named argument `{}` already occurred in this argument list", name)
            }
            
            TypeError::DuplicateParamName { name, .. } => {
                write!(f, "parameter named `{}` was already declared previously", name)
            }

            TypeError::UnsafeConversionNotAllowed { from, to, .. } => {
                write!(f, "conversion from `{}` to `{}` is only allowed in an unsafe context", from, to)
            }

            TypeError::UnsafeAddressOfNotAllowed { ty, .. } => {
                write!(f, "value of type `{}` can only have its address taken in an unsafe context", ty)
            }

            TypeError::InvalidConstExpr { expr } => {
                write!(f, "expr `{}` is not a constant value", expr)
            }

            TypeError::InvalidCaseExprBlock { .. } => {
                write!(f, "case expr must contain at least one branch and an `else` branch")
            }

            TypeError::InvalidCast { from, to, .. } => {
                write!(f, "`{}` cannot be cast to `{}`", from, to)
            }

            TypeError::EmptyMatchBlock { .. } => {
                write!(f, "this match block must have at least one branch")
            }

            TypeError::MatchExprNotExhaustive { missing_cases, .. } => {
                write!(f, "this match expr must be exhaustive or have an `else` branch")?;
                if missing_cases.len() > 0 {
                    write!(f, " (unhandled cases: ")?;
                    for (i, missing_case) in missing_cases.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", missing_case)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }

            TypeError::InvalidExitWithValue { .. } => {
                write!(f, "cannot exit with a value in this context")
            }

            TypeError::ConstDeclWithNoValue { .. } => {
                write!(f, "constant declaration must have a value")
            }

            TypeError::InvalidLoopCounterType { counter_ty, .. } => {
                write!(f, "type `{}` cannot be used as a loop counter", counter_ty)
            }

            TypeError::InvalidLoopSeqType { seq_ty, .. } => {
                write!(f, "type `{}` cannot be used as a sequence", seq_ty)
            }

            TypeError::InvalidDeclWithTypeParams { kind, .. } => {
                write!(f, "{} types cannot have type parameters", kind)
            }

            TypeError::EnumValuesMustBeAscending { prev_ident, prev_val, next_ident, next_val, .. } => {
                write!(f, "item `{}` has lower value ({}) than previous item `{}` ({})", next_ident, next_val, prev_ident, prev_val)
            }
            
            TypeError::InvalidExplicitSpec { target, .. } => {
                write!(f, "{target} cannot be explicitly specialized with type arguments")
            }
            
            TypeError::InvalidTagItem { reason, .. } => {
                match reason {
                    InvalidTagReason::InvalidType(ty) =>  write!(f, "type {} is not valid for a tag item", ty),
                    InvalidTagReason::UnsizedType(ty) =>  write!(f, "size of type {} is not known in this context", ty),
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MissingImplementation {
    pub method_name: TypedFunctionName,
    pub sig: FunctionSig,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MismatchedImplementation {
    pub iface_method_name: TypedFunctionName,
    pub expect_sig: FunctionSig,

    pub impl_method_name: TypedFunctionName,
    pub actual_sig: FunctionSig,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidOverloadKind {
    MissingOverloadModifier,
    Duplicate(usize),
}

impl fmt::Display for InvalidOverloadKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InvalidOverloadKind::MissingOverloadModifier => {
                write!(f, "the declaration is missing the `{}` modifier", DeclMod::OVERLOAD_WORD)
            }
            InvalidOverloadKind::Duplicate(..) => {
                write!(f, "the declaration is a duplicate of a previous declaration")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidTypeParamsDeclKind {
    Enum,
    Set,
    Alias,
}

impl fmt::Display for InvalidTypeParamsDeclKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InvalidTypeParamsDeclKind::Enum => write!(f, "Enum"),
            InvalidTypeParamsDeclKind::Set => write!(f, "Set"),
            InvalidTypeParamsDeclKind::Alias => write!(f, "Alias"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MismatchedMethodDecl {
    pub name: Ident,
    pub span: Span,
    pub kind: FunctionDeclKind,
}

#[derive(Debug, Clone)]
pub enum InvalidBaseTypeReason {
    NotInterface,
    Forward,
}

#[derive(Debug, Clone)]
pub enum InvalidTagReason {
    InvalidType(Type),
    UnsizedType(Type),
}
