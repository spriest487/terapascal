use crate::typ::ast::Expr;
use crate::typ::ast::MethodDecl;
use crate::typ::FunctionSig;
use crate::typ::Type;
use crate::typ::TypeName;
use crate::typ::Value;
use derivative::Derivative;
use std::sync::Arc;
use terapascal_common::span::Span;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MethodValue {
    /// the type via which this method is being referred to. we don't distinguish here between
    /// an interface method (implemented on a type other than the self type) and a direct method
    /// call (known to be implemented on the self type used here)
    pub self_ty: TypeName,
    pub index: usize,

    /// None for class methods
    pub self_arg: Option<Box<Expr>>,

    // members below this point are just cached for convenience, all of these can be
    // fetched from the type by the index
    /// span of this reference to the method (not the method's own decl)
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub span: Span,

    /// original decl
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub decl: MethodDecl,

    /// sig of this usage, with any type args/self types applied
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub sig: Arc<FunctionSig>,
}

impl MethodValue {
    pub fn new(
        self_ty: TypeName,
        self_arg: Option<Expr>,
        index: usize,
        decl: MethodDecl,
        span: Span,
    ) -> Self {
        Self {
            self_ty,
            self_arg: self_arg.map(|arg| Box::new(arg)),
            index,
            span,
            sig: Arc::new(decl.func_decl.sig()),
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
