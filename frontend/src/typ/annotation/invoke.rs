use crate::typ::ast::Expr;
use crate::typ::ast::ObjectCtorMember;
use crate::typ::function::FunctionValue;
use crate::typ::method::MethodValue;
use crate::typ::FunctionSig;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeName;
use crate::typ::Value;
use crate::Ident;
use derivative::Derivative;
use std::fmt;
use std::iter;
use std::sync::Arc;
use terapascal_common::span::MaybeSpanned;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub enum InvocationValue {
    Function {
        function: Arc<FunctionValue>,
        type_args: Option<TypeArgList>,

        args: Vec<Expr>,
        args_span: Option<Span>,

        span: Span,
    },
    Method {
        method: Arc<MethodValue>,
        type_args: Option<TypeArgList>,

        /// this list should include the self-arg, which may or may not also be present in the method
        /// value depending on how the method was invoked.
        /// i.e. the length of this vec should match the method's parameter count
        args: Vec<Expr>,
        args_span: Option<Span>,

        span: Span,
    },
    ObjectCtor {
        object_type: TypeName,

        members: Vec<ObjectCtorMember>,
        type_args: Option<TypeArgList>,

        span: Span,
    },
    VariantCtor {
        variant_type: TypeName,
        case: Ident,

        arg: Option<Expr>,

        span: Span,
    },
    FunctionValue {
        value: Expr,

        args: Vec<Expr>,
        args_span: Option<Span>,

        sig: Arc<FunctionSig>,
    },
}

impl InvocationValue {
    pub fn result_type(&self) -> &Type {
        match self {
            InvocationValue::Function { function, .. } => &function.sig.result_ty,
            InvocationValue::Method { method, .. } => &method.sig.result_ty,
            InvocationValue::ObjectCtor { object_type, .. } => object_type.ty(),
            InvocationValue::VariantCtor { variant_type, .. } => variant_type.ty(),
            InvocationValue::FunctionValue { sig, .. } => &sig.result_ty,
        }
    }

    pub fn target_span(&self) -> Option<&Span> {
        match self {
            InvocationValue::Function { function, .. } => Some(&function.span),
            InvocationValue::Method { method, .. } => Some(&method.span),
            InvocationValue::ObjectCtor { object_type, .. } => object_type.get_span(),
            InvocationValue::VariantCtor { variant_type, .. } => variant_type.get_span(),
            InvocationValue::FunctionValue { value, .. } => Some(value.span()),
        }
    }

    // constructors don't have function signatures
    pub fn sig(&self) -> Option<&Arc<FunctionSig>> {
        match self {
            InvocationValue::Function { function, .. } => Some(&function.sig),

            InvocationValue::FunctionValue { sig, .. } => Some(sig),

            InvocationValue::Method { method, .. } => Some(&method.sig),

            InvocationValue::ObjectCtor { .. } | InvocationValue::VariantCtor { .. } => None,
        }
    }

    pub fn args(&self) -> Box<dyn Iterator<Item = &Expr> + '_> {
        match self {
            InvocationValue::Function { args, .. } => Box::new(args.iter()),

            InvocationValue::Method { args, .. } => {
                Box::new(args.iter())
            },

            InvocationValue::ObjectCtor { members, .. } => {
                Box::new(members.iter().map(|mem| &mem.value))
            },
            InvocationValue::VariantCtor { arg, .. } => match arg {
                Some(arg) => Box::new(iter::once(arg)),
                None => Box::new(iter::empty()),
            },
            InvocationValue::FunctionValue { args, .. } => Box::new(args.iter()),
        }
    }

    pub fn args_span(&self) -> Option<&Span> {
        match self {
            InvocationValue::Function { args_span, .. } => args_span.as_ref(),
            InvocationValue::Method { args_span, .. } => args_span.as_ref(),
            InvocationValue::ObjectCtor { .. } => None,
            InvocationValue::VariantCtor { .. } => None,
            InvocationValue::FunctionValue { args_span, .. } => args_span.as_ref(),
        }
    }

    pub fn type_args(&self) -> Option<&TypeArgList> {
        match self {
            InvocationValue::Function { type_args, .. }
            | InvocationValue::Method { type_args, .. }
            | InvocationValue::ObjectCtor { type_args, .. } => type_args.as_ref(),

            InvocationValue::VariantCtor { .. }
            | InvocationValue::FunctionValue { .. } => None,
        }
    }
}

impl Spanned for InvocationValue {
    fn span(&self) -> &Span {
        match self {
            InvocationValue::Function { span, .. } => span,
            InvocationValue::Method { span, .. } => span,
            InvocationValue::ObjectCtor { span, .. } => span,
            InvocationValue::VariantCtor { span, .. } => span,
            InvocationValue::FunctionValue { value, .. } => value.span(),
        }
    }
}

impl From<InvocationValue> for Value {
    fn from(value: InvocationValue) -> Self {
        Value::Invocation(Arc::new(value))
    }
}

impl fmt::Display for InvocationValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InvocationValue::Function { function, .. } => {
                write!(f, "invocation of function {}", function.name)
            },
            InvocationValue::Method { method, .. } => {
                write!(
                    f,
                    "invocation of method {}.{}",
                    method.self_ty, method.decl.func_decl.name.ident
                )
            },
            InvocationValue::ObjectCtor { object_type, .. } => {
                write!(f, "object ctor invocation of type {}", object_type)
            },
            InvocationValue::VariantCtor { variant_type, .. } => {
                write!(f, "variant ctor invocation of type {}", variant_type)
            },
            InvocationValue::FunctionValue { .. } => {
                write!(f, "function value invocation")
            },
        }
    }
}
