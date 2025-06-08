use crate::typ::FunctionValue;
use crate::typ::MethodValue;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::Value;
use derivative::Derivative;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub enum InvocationValue {
    Function {
        function: Arc<FunctionValue>,
        type_args: Option<TypeArgList>,
    },
    Method {
        method: Arc<MethodValue>,
        type_args: Option<TypeArgList>,
    },
    VirtualMethod {
        method: Arc<MethodValue>,

        iface_ty: Type,
        iface_method_index: usize,
    },
}

impl InvocationValue {
    pub fn result_type(&self) -> &Type {
        match self {
            InvocationValue::Function { function, .. } => &function.decl.result_ty,
            InvocationValue::Method { method, .. } => &method.decl.func_decl.result_ty,
            InvocationValue::VirtualMethod { method, .. } => &method.decl.func_decl.result_ty,
        }
    }

    pub fn target_span(&self) -> &Span {
        match self {
            InvocationValue::Function { function, .. } => &function.span,
            InvocationValue::Method { method, .. } => &method.span,
            InvocationValue::VirtualMethod { method, .. } => &method.span,
        }
    }

    pub fn function(
        function: impl Into<Arc<FunctionValue>>,
        type_args: Option<TypeArgList>,
    ) -> Self {
        InvocationValue::Function {
            function: function.into(),
            type_args,
        }
    }

    pub fn method(method: impl Into<Arc<MethodValue>>, type_args: Option<TypeArgList>) -> Self {
        InvocationValue::Method {
            method: method.into(),
            type_args,
        }
    }

    pub fn virtual_method(
        method: impl Into<Arc<MethodValue>>,
        iface_ty: impl Into<Type>,
        iface_method_index: usize,
    ) -> Self {
        InvocationValue::VirtualMethod {
            method: method.into(),
            iface_ty: iface_ty.into(),
            iface_method_index,
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
            InvocationValue::VirtualMethod {
                method, iface_ty, ..
            } => {
                write!(
                    f,
                    "virtual invocation of method {}.{} ({})",
                    method.self_ty, method.decl.func_decl.name.ident, iface_ty
                )
            },
        }
    }
}
