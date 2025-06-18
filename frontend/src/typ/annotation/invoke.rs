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
use crate::ast::SemanticHint;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub enum Invocation {
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
        
        /// it's possible for this to be different to the method's own self-type in cases where
        /// an interface method is being invoked for a currently unknown (e.g. generic) self type.
        /// in that case, we need to know both types (the real self-type and the type the abstract
        /// method is declared in), and the call will be de-virtualized during codegen 
        self_ty: Type,

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

impl Invocation {
    pub fn result_type(&self) -> &Type {
        match self {
            Invocation::Function { function, .. } => &function.sig.result_ty,
            Invocation::Method { method, .. } => &method.sig.result_ty,
            Invocation::ObjectCtor { object_type, .. } => object_type.ty(),
            Invocation::VariantCtor { variant_type, .. } => variant_type.ty(),
            Invocation::FunctionValue { sig, .. } => &sig.result_ty,
        }
    }

    pub fn target_span(&self) -> Option<&Span> {
        match self {
            Invocation::Function { function, .. } => Some(&function.span),
            Invocation::Method { method, .. } => Some(&method.span),
            Invocation::ObjectCtor { object_type, .. } => object_type.get_span(),
            Invocation::VariantCtor { variant_type, .. } => variant_type.get_span(),
            Invocation::FunctionValue { value, .. } => Some(value.span()),
        }
    }

    // constructors don't have function signatures
    pub fn sig(&self) -> Option<&Arc<FunctionSig>> {
        match self {
            Invocation::Function { function, .. } => Some(&function.sig),

            Invocation::FunctionValue { sig, .. } => Some(sig),

            Invocation::Method { method, .. } => Some(&method.sig),

            Invocation::ObjectCtor { .. } | Invocation::VariantCtor { .. } => None,
        }
    }

    pub fn args(&self) -> Box<dyn Iterator<Item = &Expr> + '_> {
        match self {
            Invocation::Function { args, .. } => Box::new(args.iter()),

            Invocation::Method { args, .. } => {
                Box::new(args.iter())
            },

            Invocation::ObjectCtor { members, .. } => {
                Box::new(members.iter().map(|mem| &mem.value))
            },
            Invocation::VariantCtor { arg, .. } => match arg {
                Some(arg) => Box::new(iter::once(arg)),
                None => Box::new(iter::empty()),
            },
            Invocation::FunctionValue { args, .. } => Box::new(args.iter()),
        }
    }

    pub fn args_span(&self) -> Option<&Span> {
        match self {
            Invocation::Function { args_span, .. } => args_span.as_ref(),
            Invocation::Method { args_span, .. } => args_span.as_ref(),
            Invocation::ObjectCtor { .. } => None,
            Invocation::VariantCtor { .. } => None,
            Invocation::FunctionValue { args_span, .. } => args_span.as_ref(),
        }
    }

    pub fn type_args(&self) -> Option<&TypeArgList> {
        match self {
            Invocation::Function { type_args, .. }
            | Invocation::Method { type_args, .. }
            | Invocation::ObjectCtor { type_args, .. } => type_args.as_ref(),

            Invocation::VariantCtor { .. }
            | Invocation::FunctionValue { .. } => None,
        }
    }
    
    pub fn semantic_hint(&self) -> SemanticHint {
        match self {
            Invocation::Function { .. } => SemanticHint::Function,
            Invocation::Method { .. } => SemanticHint::Method,
            Invocation::ObjectCtor { .. } => SemanticHint::Type,
            Invocation::VariantCtor { .. } => SemanticHint::VariantCase,
            Invocation::FunctionValue { .. } => SemanticHint::Variable,
        }
    }
}

impl Spanned for Invocation {
    fn span(&self) -> &Span {
        match self {
            Invocation::Function { span, .. } => span,
            Invocation::Method { span, .. } => span,
            Invocation::ObjectCtor { span, .. } => span,
            Invocation::VariantCtor { span, .. } => span,
            Invocation::FunctionValue { value, .. } => value.span(),
        }
    }
}

impl From<Invocation> for Value {
    fn from(value: Invocation) -> Self {
        Value::Invocation(Arc::new(value))
    }
}

impl fmt::Display for Invocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Invocation::Function { function, .. } => {
                write!(f, "invocation of function {}", function.name)
            },
            Invocation::Method { method, .. } => {
                write!(
                    f,
                    "invocation of method {}.{}",
                    method.self_ty, method.decl.func_decl.name.ident
                )
            },
            Invocation::ObjectCtor { object_type, .. } => {
                write!(f, "object ctor invocation of type {}", object_type)
            },
            Invocation::VariantCtor { variant_type, .. } => {
                write!(f, "variant ctor invocation of type {}", variant_type)
            },
            Invocation::FunctionValue { .. } => {
                write!(f, "function value invocation")
            },
        }
    }
}
