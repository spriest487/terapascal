use crate::ast::Expr;
use crate::ast::TypeName;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::typecheck_expr;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Spanned;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ArrayType {
    pub element_ty: Type,
    pub dim: usize,
}

impl ArrayType {
    pub fn new(element_ty: Type, dim: usize) -> Self {
        Self { element_ty, dim }
    }
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "array[{}] of {}", self.dim, self.element_ty)
    }
}

pub fn typecheck_array_type(element: &Box<TypeName>, dim: &Option<Box<Expr>>, ctx: &mut Context) -> TypeResult<Type> {
    let element_ty = typecheck_type(element.as_ref(), ctx)?;

    match dim {
        Some(dim_expr) => {
            let dim_expr =
                typecheck_expr(dim_expr, &Type::Primitive(Primitive::Int32), ctx)?;
            let dim_val = const_eval_integer(&dim_expr, ctx)?;

            let dim = dim_val
                .as_usize()
                .ok_or_else(|| TypeError::TypeMismatch {
                    span: dim_expr.span().clone(),
                    actual: dim_expr.annotation().ty().into_owned(),
                    expected: Type::Primitive(Primitive::Int32),
                })?;

            Ok(ArrayType { element_ty, dim }.into())
        },

        None => Ok(Type::DynArray {
            element: Arc::new(element_ty),
        }),
    }
}
