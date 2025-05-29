mod symbol;

pub use symbol::*;

use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Visibility;
use crate::ast::{Annotation, ConstExprValue};
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::Literal;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::OverloadCandidate;
use crate::typ::result::*;
use crate::typ::ty::*;
use crate::typ::Context;
use crate::typ::ValueKind;
use crate::IntConstant;
use derivative::*;
use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::sync::Arc;
use terapascal_common::span::*;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCaseValue {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    // variant ctors don't know the type args of their variant, it must be inferred from context
    pub variant_name: Arc<Symbol>,

    pub case: Ident,
}

impl From<VariantCaseValue> for Value {
    fn from(a: VariantCaseValue) -> Self {
        Value::VariantCase(Arc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct OverloadValue {
    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,

    pub candidates: Vec<OverloadCandidate>,
    pub sig: Option<Arc<FunctionSig>>,

    pub self_arg: Option<Box<Expr>>,
}

impl OverloadValue {
    pub fn method(
        self_ty: Type,
        iface_ty: Type,
        index: usize,
        self_arg: Expr,
        method: MethodDecl,
        span: Span
    ) -> Self {
        let sig = Arc::new(method.func_decl.sig());

        Self {
            span,
            self_arg: Some(Box::new(self_arg)),
            sig: Some(sig.clone()),
            candidates: vec![OverloadCandidate::Method {
                iface_ty,
                self_ty,
                index,
                decl: method.clone(),
            }],
        }
    }

    pub fn new(
        candidates: Vec<OverloadCandidate>,
        self_arg: Option<Box<Expr>>,
        span: Span,
    ) -> Self {
        let sig = if candidates.len() == 1 {
            Some(Arc::new(candidates[0].decl().sig()))
        } else {
            // undecided
            None
        };

        Self {
            candidates,
            sig,
            span,
            self_arg,
        }
    }

    pub fn func_ty(&self) -> Type {
        match &self.sig {
            Some(sig) => Type::Function(sig.clone()),
            None => Type::Nothing,
        }
    }
}

impl From<OverloadValue> for Value {
    fn from(a: OverloadValue) -> Self {
        Value::Overload(Arc::new(a))
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MethodValue {
    /// the type via which this method is being referred to. we don't distinguish here between
    /// an interface method (implemented on a type other than the self type) and a direct method
    /// call (known to be implemented on the self type used here)
    pub self_ty: Type,
    pub index: usize,
    
    // members below this point are just cached for convenience, all of these can be
    // fetched from the type by the index

    /// span of this reference to the method (not the method's own decl)
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub span: Span,

    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub decl: MethodDecl,
}

impl MethodValue {
    pub fn new(self_ty: Type, index: usize, decl: MethodDecl, span: Span) -> Self {
        Self {
            self_ty,
            index,
            span,
            decl,
        }
    }

    pub fn func_ty(&self) -> Type {
        Type::Function(Arc::new(self.decl.func_decl.sig()))
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type, self_arg: &Type) -> bool {
        self.decl.func_decl.sig().should_call_noargs_in_expr(expect_ty, self_arg)
    }
}

impl From<MethodValue> for Value {
    fn from(a: MethodValue) -> Self {
        Value::Method(Arc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct FunctionValue {
    pub name: Symbol,
    pub visibility: Visibility,

    pub decl: Arc<FunctionDecl>,
    pub sig: Arc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl FunctionValue {
    pub fn new(name: Symbol, visibility: Visibility, decl: Arc<FunctionDecl>, span: Span) -> Self {
        Self {
            name,
            visibility,
            sig: Arc::new(decl.sig()),
            decl,
            span,
        }
    }
    
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        self.sig.should_call_noargs_in_expr(expect_ty, &Type::Nothing)
    }
    
    pub fn check_visible(&self, at: &Span, ctx: &Context) -> TypeResult<()> {
        if self.visibility < Visibility::Interface
            && !ctx.is_current_namespace_child(&self.name.full_path) {
            return Err(TypeError::NameNotVisible {
                name: self.name.full_path.clone(),
                span: at.clone(),
            });
        }
        
        Ok(())
    }
}

impl From<FunctionValue> for Value {
    fn from(a: FunctionValue) -> Self {
        Value::Function(Arc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct TypedValue {
    pub ty: Type,
    pub value_kind: ValueKind,
    pub decl: Option<IdentPath>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl TypedValue {
    pub fn temp(ty: Type, span: Span) -> Self {
        TypedValue {
            ty,
            span,
            value_kind: ValueKind::Temporary,
            decl: None,
        }
    }

    pub fn unit_const(ty: Type, decl: IdentPath, span: Span) -> Self {
        TypedValue {
            ty,
            span,
            value_kind: ValueKind::Immutable,
            decl: Some(decl),
        }
    }
    
    pub fn local_const(ty: Type, name: Ident, span: Span) -> Self {
        TypedValue {
            ty,
            span,
            value_kind: ValueKind::Immutable,
            decl: Some(IdentPath::from(name)),
        }
    }

    pub fn unit_var(ty: Type, decl: IdentPath, span: Span) -> Self {
        TypedValue {
            ty,
            span,
            value_kind: ValueKind::Mutable,
            decl: Some(decl),
        }
    }
    
    pub fn local_var(ty: Type, name: Ident, span: Span) -> Self {
        TypedValue {
            ty,
            span,
            value_kind: ValueKind::Mutable,
            decl: Some(IdentPath::from(name)),
        }
    }
}

impl From<TypedValue> for Value {
    fn from(a: TypedValue) -> Self {
        Value::Typed(Arc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct ConstValue {
    pub decl: Option<IdentPath>,
    pub ty: Type,

    pub value: Literal,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl From<ConstValue> for Value {
    fn from(a: ConstValue) -> Self {
        Value::Const(Arc::new(a))
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(Hash, Debug, PartialEq)]
pub struct UfcsValue {
    pub function_name: Symbol,
    pub visibility: Visibility,
    
    pub self_arg: Box<Expr>,
    
    pub decl: Arc<FunctionDecl>,
    pub sig: Arc<FunctionSig>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl UfcsValue {
    pub fn new(
        function_name: Symbol,
        visibility: Visibility,
        self_arg: Expr,
        decl: Arc<FunctionDecl>,
        span: Span
    ) -> Self {
        Self {
            self_arg: Box::new(self_arg),
            function_name,
            sig: Arc::new(decl.sig()),
            decl,
            visibility,
            span,
        }
    }
    
    pub fn func_ty(&self) -> Type {
        Type::Function(self.sig.clone())
    }

    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type) -> bool {
        let self_arg_ty = self.self_arg.annotation().ty();

        self.decl.sig().should_call_noargs_in_expr(expect_ty, self_arg_ty.as_ref())
    }
}

impl From<UfcsValue> for Value {
    fn from(a: UfcsValue) -> Self {
        Value::UfcsFunction(Arc::new(a))
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub struct EvaluatedConstExpr<Val> {
    pub expr: Box<Expr>,
    pub value: Val,
}

impl<T> fmt::Display for EvaluatedConstExpr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

impl<T> Spanned for EvaluatedConstExpr<T> {
    fn span(&self) -> &Span {
        self.expr.span()
    }
}

impl<T> ConstExprValue<Value, T> for EvaluatedConstExpr<T> 
where
    T: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash
{
    fn as_expr(&self) -> &crate::ast::Expr<Value> {
        &self.expr
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub enum Value {
    Untyped(
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span
    ),
    Typed(Arc<TypedValue>),

    Function(Arc<FunctionValue>),
    UfcsFunction(Arc<UfcsValue>),

    // direct method reference e.g. `Interface.Method`
    Method(Arc<MethodValue>),
    Type(
        Type,
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span
    ),
    Namespace(
        IdentPath,
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span
    ),
    VariantCase(Arc<VariantCaseValue>),

    // as-yet unresolved function that may refer to 1+ functions (interface methods, ufcs functions,
    // or free functions)
    Overload(Arc<OverloadValue>),

    Const(Arc<ConstValue>),
}

impl Value {
    pub fn expect_any_value(&self) -> TypeResult<()> {
        self.expect_value(&Type::Nothing)
    }
    
    pub fn expect_value(&self, expect_ty: &Type) -> TypeResult<()> {
        let (actual_ty, span) = match self {
            Value::Method(method) => (method.func_ty(), &method.span),
            Value::Typed(val) => (val.ty.clone(), &val.span),
            Value::Const(const_val) => (const_val.ty.clone(), &const_val.span),
            Value::Function(func) => (func.func_ty(), &func.span),

            Value::Overload(..)
            | Value::UfcsFunction(..)
            | Value::Untyped(..)
            | Value::Namespace(..)
            | Value::Type(..)
            | Value::VariantCase(..) => {
                return Err(TypeError::NotValueExpr { 
                    expected: expect_ty.clone(),
                    actual: self.clone(),
                });
            }
        };

        if actual_ty == Type::Nothing {
            return Err(TypeError::NotValueExpr {
                expected: expect_ty.clone(),
                actual: self.clone(),
            });
        }
        
        if actual_ty != *expect_ty && *expect_ty != Type::Nothing {
            return Err(TypeError::TypeMismatch {
                span: span.clone(),
                expected: expect_ty.clone(),
                actual: actual_ty,
            });
        }
        
        Ok(())
    }
    
    pub fn expect_no_value(&self) -> TypeResult<()> {
        let actual_ty = self.ty();

        if *actual_ty != Type::Nothing {
            return Err(TypeError::TypeMismatch {
                span: self.span().clone(),
                expected: Type::Nothing,
                actual: actual_ty.into_owned(),
            });
        }
        
        Ok(())
    }
    
    pub fn as_const(&self) -> Option<&ConstValue> {
        match self {
            Value::Const(const_val) => Some(const_val.as_ref()),
            _ => None,
        }
    }

    pub fn new_temp_val(ty: Type, span: Span) -> Self {
        let typed_val = TypedValue {
            decl: None,
            value_kind: ValueKind::Temporary,
            ty,
            span,
        };
        typed_val.into()
    }

    pub fn ty(&self) -> Cow<Type> {
        match self {
            Value::Namespace(_, _)
            | Value::Untyped(_)
            | Value::Type(_, _)
            | Value::VariantCase(..) => Cow::Owned(Type::Nothing),

            Value::Function(func) => Cow::Owned(func.func_ty()),
            Value::UfcsFunction(call) => Cow::Owned(call.func_ty()),
            Value::Method(method) => Cow::Owned(method.func_ty()),
            Value::Overload(overload) => Cow::Owned(overload.func_ty()),

            Value::Const(const_val) => Cow::Borrowed(&const_val.ty),
            Value::Typed(val) => Cow::Borrowed(&val.ty),
        }
    }

    pub fn decl(&self) -> Option<Cow<IdentPath>> {
        match self {
            Value::Type(..) => None,
            Value::Function { .. } => None, // TODO
            Value::Method(..) => None, // TODO
            Value::UfcsFunction { .. } => None, // TODO
            Value::Overload { .. } => None, // TODO

            Value::Typed(val) => val.decl.as_ref().map(Cow::Borrowed),
            Value::Untyped(..) => None,
            Value::Namespace(path, ..) => Some(Cow::Borrowed(path)),

            Value::Const(const_val) => const_val.decl
                .as_ref()
                .map(Cow::Borrowed),

            Value::VariantCase(ctor) => {
                let case_path = ctor.variant_name
                    .full_path
                    .clone()
                    .child(ctor.case.clone());

                Some(Cow::Owned(case_path))
            },
        }
    }

    pub fn value_kind(&self) -> Option<ValueKind> {
        match self {
            Value::Typed(val) => Some(val.value_kind),
            Value::Const { .. } => Some(ValueKind::Immutable),
            _ => None,
        }
    }

    pub fn is_namespace(&self) -> bool {
        match self {
            Value::Namespace(_, _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Untyped(_) => { 
                write!(f, "untyped value") 
            },
            Value::Typed(val) => { 
                write!(f, "{} of type {}", val.value_kind, val.ty) 
            },
            Value::Function(func) => { 
                write!(f, "function {}", func.name) 
            },
            Value::UfcsFunction(func) => { 
                write!(f, "function {}", func.function_name) 
            },
            Value::Method(method) => { 
                write!(f, "method {}.{}", method.self_ty, method.decl.func_decl.ident()) 
            },
            Value::Type(ty, ..) => { 
                write!(f, "type {}", ty) 
            },
            Value::Namespace(ns, ..) => { 
                write!(f, "namespace {}", ns) 
            },
            Value::VariantCase(case) => { 
                write!(f, "variant case {}.{}", case.variant_name, case.case) 
            },
            Value::Overload(overload) => { 
                write!(f, "overloaded function")?;
                if let Some(sig) = &overload.sig {
                    write!(f, " with signature {}", sig)?;
                }
                Ok(())
            },
            Value::Const(const_val) => { 
                write!(f, "constant")?;
                if let Some(decl) = &const_val.decl {
                    write!(f, " {}", decl)?;
                }
                write!(f, "({})", const_val.value)
            },
        }
    }
}

impl Spanned for Value {
    fn span(&self) -> &Span {
        match self {
            Value::Method(method) => &method.span,
            Value::VariantCase(ctor) => &ctor.span,
            Value::Overload(overload) => &overload.span,
            Value::Typed(val) => &val.span,
            Value::Const(const_val) => &const_val.span,
            Value::Function(func) => &func.span,
            Value::UfcsFunction(call) => &call.span,

            Value::Untyped(span)
            | Value::Type(_, span)
            | Value::Namespace(_, span) => span,
        }
    }
}

impl Annotation for Value {
    type Type = Type;
    type DeclName = Symbol;
    type Pattern = TypePattern;
    type FunctionName = crate::typ::ast::FunctionName;

    type ConstStringExpr = EvaluatedConstExpr<String>;
    type ConstIntegerExpr = EvaluatedConstExpr<IntConstant>;
    type ConstValue = Literal;
}
