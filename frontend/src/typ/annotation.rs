mod invoke;
mod symbol;
pub mod overload;
pub mod method;
pub mod function;
mod ufcs;

use crate::ast;
use crate::ast::Annotation;
use crate::ast::ConstExprValue;
use crate::ast::Ident;
use crate::ast::IdentPath;
pub use crate::typ::annotation::invoke::Invocation;
use crate::typ::ast::evaluate_expr;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::specialize_call_args;
use crate::typ::ast::typecheck_type_args;
use crate::typ::ast::Expr;
use crate::typ::ast::Literal;
use crate::typ::function::FunctionValue;
use crate::typ::method::MethodValue;
use crate::typ::overload::OverloadValue;
use crate::typ::result::*;
use crate::typ::ty::*;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::ValueKind;
use crate::IntConstant;
use derivative::*;
use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::sync::Arc;
pub use symbol::*;
use terapascal_common::span::*;
pub use ufcs::*;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct VariantCaseValue {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,

    // variant ctors don't know the type args of their variant, it must be inferred from context
    pub variant_name: Arc<Symbol>,

    // a subrange of `span`, of the reference to the variant type name. `variant_name`'s span
    // will be the type declaration's span, not the reference to it at this value's location
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub variant_name_span: Span,

    pub case: Ident,
}

impl VariantCaseValue {
    pub fn typecheck_invocation(
        &self,
        args: &[ast::Expr],
        type_args: Option<&ast::TypeArgList<Span>>,
        expect_ty: &Type,
        span: &Span,
        ctx: &mut Context,
    ) -> TypeResult<Invocation> {
        let variant_sym = match type_args {
            Some(type_name_list) => {
                let type_list = typecheck_type_args(type_name_list, ctx)?;

                self.variant_name
                    .specialize(&type_list, ctx)
                    .map_err(|err| TypeError::from_generic_err(err, span.clone()))?
                    .into_owned()
            },

            None => {
                // infer the specialized generic type if the written one is generic and the hint is a specialized
                // version of that same generic variant
                match expect_ty {
                    Type::Variant(expect_variant)
                        if expect_variant.full_path == self.variant_name.full_path =>
                    {
                        (**expect_variant).clone()
                    },

                    _ => (*self.variant_name).clone(),
                }
            },
        };

        if variant_sym.is_unspecialized_generic() {
            return Err(TypeError::from_generic_err(
                GenericError::CannotInferArgs {
                    target: GenericTarget::Name(variant_sym.full_path.clone()),
                    hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                },
                span.clone(),
            ));
        }

        let variant_def = ctx
            .instantiate_variant_def(&variant_sym)
            .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

        let case_data = variant_def
            .case_position(&self.case)
            .and_then(|case_index| variant_def.cases[case_index].data.as_ref());

        let mut arg_exprs = Vec::new();
        for i in 0..args.len() {
            let expect_ty = match (i, case_data) {
                (0, Some(data)) => &data.ty,
                _ => &Type::Nothing,
            };

            let arg = evaluate_expr(&args[i], expect_ty, ctx)?;
            arg_exprs.push(arg);
        }

        self.create_ctor_invocation(
            &arg_exprs,
            variant_sym.type_args.as_ref(),
            expect_ty,
            span,
            ctx,
        )
    }

    pub fn create_ctor_invocation(
        &self,
        args: &[Expr],
        type_args: Option<&TypeArgList>,
        expect_ty: &Type,
        span: &Span,
        ctx: &mut Context,
    ) -> TypeResult<Invocation> {
        // validate visibility
        if !ctx.is_visible(&self.variant_name.full_path) {
            return Err(TypeError::NameNotVisible {
                name: self.variant_name.full_path.clone(),
                span: span.clone(),
            });
        }

        let variant_sym = match (type_args, &self.variant_name.type_params) {
            (Some(type_args), ..) => self
                .variant_name
                .specialize(type_args, ctx)
                .map_err(|err| TypeError::from_generic_err(err, span.clone()))?,

            (None, Some(..)) => {
                let inferred_name = match expect_ty {
                    Type::Variant(expect_name) => {
                        self.variant_name.infer_specialized_from_hint(expect_name).cloned()
                    },

                    _ => None,
                };

                inferred_name.map(Cow::Owned).ok_or_else(|| {
                    let err = GenericError::CannotInferArgs {
                        target: GenericTarget::Name(self.variant_name.full_path.clone()),
                        hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                    };
                    TypeError::from_generic_err(err, span.clone())
                })?
            },

            (None, None) => Cow::Borrowed(self.variant_name.as_ref()),
        };

        let variant_def = ctx
            .instantiate_variant_def(&variant_sym)
            .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

        // validate case name exists
        let case_index = match variant_def.case_position(&self.case) {
            Some(index) => index,

            None => {
                return Err(TypeError::from_name_err(
                    NameError::MemberNotFound {
                        member: self.case.clone(),
                        base: NameContainer::Type(Type::variant(variant_sym.into_owned())),
                    },
                    span.clone(),
                ));
            },
        };

        // validate arg count matches (0 or 1)
        let arg = match &variant_def.cases[case_index].data {
            None => {
                if !args.is_empty() {
                    return Err(TypeError::invalid_variant_ctor_args(
                        None,
                        args,
                        span.clone(),
                    ));
                }

                None
            },

            Some(data) => {
                if args.len() != 1 {
                    return Err(TypeError::invalid_variant_ctor_args(
                        Some(data.ty.clone()),
                        args,
                        span.clone(),
                    ));
                }

                let arg = args.iter().cloned().next().unwrap();
                // eprintln!("arg: {}, ty: {}", arg.annotation(), arg.annotation().ty());

                let data_val = implicit_conversion(arg, &data.ty, ctx)?;
                
                Some(data_val)
            },
        };

        Ok(Invocation::VariantCtor {
            variant_type: TypeName::named(
                Type::variant(variant_sym.into_owned()),
                self.variant_name_span.clone(),
            ),
            case: self.case.clone(),
            arg,
            span: self.span.clone(),
        })
    }
}

impl From<VariantCaseValue> for Value {
    fn from(a: VariantCaseValue) -> Self {
        Value::VariantCase(Arc::new(a))
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
    T: fmt::Debug + fmt::Display + Clone + PartialEq + Eq + Hash,
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
        Span,
    ),
    Typed(Arc<TypedValue>),

    Function(Arc<FunctionValue>),
    UfcsFunction(Arc<UfcsValue>),

    Invocation(Arc<Invocation>),

    // direct method reference e.g. `Interface.Method`
    Method(Arc<MethodValue>),
    Type(
        Type,
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span,
    ),
    Namespace(
        IdentPath,
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        #[derivative(Hash = "ignore")]
        Span,
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
            Value::Typed(val) => {
                (val.ty.clone(), &val.span)
            },
            Value::Const(const_val) => {
                (const_val.ty.clone(), &const_val.span)
            },
            Value::Invocation(val) => {
                (val.result_type().clone(), val.span())
            },
            Value::Function(func_val) => {
                (Type::Function(func_val.sig.clone()), &func_val.span)
            },

            Value::Method(..)
            | Value::Overload(..)
            | Value::UfcsFunction(..)
            | Value::Untyped(..)
            | Value::Namespace(..)
            | Value::Type(..)
            | Value::VariantCase(..) => {
                return Err(TypeError::NotValueExpr {
                    expected: expect_ty.clone(),
                    actual: self.clone(),
                });
            },
        };

        if actual_ty == Type::Nothing {
            return Err(TypeError::NotValueExpr {
                expected: expect_ty.clone(),
                actual: self.clone(),
            });
        }

        if actual_ty != *expect_ty && *expect_ty != Type::Nothing {
            return Err(TypeError::type_mismatch(expect_ty.clone(), actual_ty, span.clone()));
        }

        Ok(())
    }

    pub fn expect_no_value(&self) -> TypeResult<()> {
        let actual_ty = self.ty();

        if *actual_ty != Type::Nothing {
            return Err(TypeError::type_mismatch(Type::Nothing, actual_ty.into_owned(), self.span().clone()));
        }

        Ok(())
    }

    pub fn as_invocation(&self) -> Option<&Invocation> {
        match self {
            Value::Invocation(invocation) => Some(invocation.as_ref()),
            _ => None,
        }
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
            | Value::UfcsFunction(_)
            | Value::Method(_)
            | Value::Overload(_)
            | Value::Type(_, _)
            | Value::VariantCase(..) => {
                Cow::Owned(Type::Nothing)
            },

            Value::Function(func_val) => {
                Cow::Owned(Type::Function(func_val.sig.clone()))
            },

            Value::Invocation(invocation) => {
                Cow::Borrowed(invocation.result_type())
            },

            Value::Const(const_val) => {
                Cow::Borrowed(&const_val.ty)
            },
            Value::Typed(val) => {
                Cow::Borrowed(&val.ty)
            },
        }
    }

    pub fn decl(&self) -> Option<Cow<IdentPath>> {
        match self {
            Value::Type(..) => None,
            Value::Function { .. } => None,     // TODO
            Value::Method(..) => None,          // TODO
            Value::UfcsFunction { .. } => None, // TODO
            Value::Overload { .. } => None,     // TODO

            Value::Invocation(..) => None,

            Value::Typed(val) => val.decl.as_ref().map(Cow::Borrowed),
            Value::Untyped(..) => None,
            Value::Namespace(path, ..) => Some(Cow::Borrowed(path)),

            Value::Const(const_val) => const_val.decl.as_ref().map(Cow::Borrowed),

            Value::VariantCase(ctor) => {
                let case_path = ctor.variant_name.full_path.clone().child(ctor.case.clone());

                Some(Cow::Owned(case_path))
            },
        }
    }

    pub fn value_kind(&self) -> Option<ValueKind> {
        match self {
            Value::Typed(val) => Some(val.value_kind),
            Value::Const { .. } => Some(ValueKind::Immutable),
            Value::Invocation(..) => Some(ValueKind::Temporary),
            Value::Function(..) => Some(ValueKind::Immutable),
            _ => None,
        }
    }

    pub fn is_namespace(&self) -> bool {
        match self {
            Value::Namespace(_, _) => true,
            _ => false,
        }
    }
    
    /// A node can be an unevaluated reference like a function name, which is valid in some contexts
    /// e.g. as the target of a function call argument list or to have its address taken. In contexts
    /// where only the value is legal, this operation forces the reference to be evaluated e.g.
    /// turns a function reference into a function invocation. This is the logic behind no-args
    /// function calls.
    /// 
    /// This operation may fail with an error if the *type* of value can be evaluated, but this
    /// particular value is invalid: a function reference that is evaluated could be a valid
    /// no-args call, but if it requires more parameters, it should be treated like any invalid
    /// invocation and raise an InvalidArgs error.
    /// 
    /// This will *not* fail with an error for node values that can't be evaluated, and will instead
    /// return itself. For example, a typed Integer value node is already a value so evaluating it
    /// does nothing, and a Type value node has no possible value, so we should leave it alone
    /// and allow the expression to fail typechecking.  
    pub fn evaluate(&mut self, expect_ty: &Type, ctx: &mut Context) -> TypeResult<()> {
        match self {
            // can't be evaluated
            Value::Untyped(..)
            | Value::Type(_, _)
            | Value::Namespace(_, _)

            // already a value
            | Value::Invocation(_)
            | Value::Typed(_)
            | Value::Const(_) => Ok(()),

            // references to functions become no-args invocations when evaluated
            Value::Function(func) => {
                let args = specialize_call_args(
                    &func.decl,
                    &[],
                    None,
                    None,
                    &func.span,
                    ctx
                )?;

                let invocation = func.create_invocation(
                    &args.actual_args,
                    None,
                    args.type_args.as_ref(),
                    expect_ty,
                    &func.span,
                    ctx
                )?;
                
                *self = Value::from(invocation);
                Ok(())
            }

            Value::UfcsFunction(ufcs) => {
                let invocation = ufcs.create_zero_args_invocation(expect_ty, &ufcs.span, ctx)?;
                *self = Value::from(invocation);
                Ok(())
            }

            Value::Method(method) => {
                let args = specialize_call_args(
                    &method.decl.func_decl,
                    &[],
                    method.self_arg.as_ref().map(Box::as_ref),
                    None,
                    &method.span,
                    ctx
                )?;

                *self = Value::from(Invocation::Method {
                    method: method.clone(),
                    self_ty: method.self_ty.ty().clone(),
                    args: args.actual_args,
                    args_span: None,
                    span: method.span.clone(),
                    type_args: args.type_args,
                });
                Ok(())
            }

            Value::VariantCase(case_val) => {
                let invocation = case_val.create_ctor_invocation(
                    &[],
                    None,
                    expect_ty,
                    &case_val.span,
                    ctx
                )?;

                *self = Value::from(invocation);
                Ok(())
            }

            Value::Overload(overload_val) => {
                let invocation = overload_val.create_invocation(&[], None, None, &overload_val.span, ctx)?;
                *self = Value::from(invocation);
                Ok(())
            }
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
                write!(f, "function (UFCS) {}", func.function_name)
            },
            Value::Invocation(invoked) => {
                write!(f, "{invoked}")
            },
            Value::Method(method) => {
                write!(
                    f,
                    "method {}.{}",
                    method.self_ty,
                    method.decl.func_decl.ident()
                )
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

            Value::Invocation(invocation) => invocation.span(),

            Value::Untyped(span) | Value::Type(_, span) | Value::Namespace(_, span) => span,
        }
    }
}

impl Annotation for Value {
    type Type = Type;
    type TypeName = TypeName;
    type DeclName = Symbol;
    type Pattern = TypePattern;
    type FunctionName = crate::typ::ast::FunctionName;

    type ConstStringExpr = EvaluatedConstExpr<String>;
    type ConstIntegerExpr = EvaluatedConstExpr<IntConstant>;
    type ConstValue = Literal;
}
