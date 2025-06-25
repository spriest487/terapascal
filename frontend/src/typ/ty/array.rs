use crate::ast;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::typecheck_expr;
use crate::typ::typecheck_typename;
use crate::typ::Context;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Spanned;

pub type ArrayTypeName = ast::ArrayTypeName<Value>;

const ARRAY_DIM_TY: Type = Type::Primitive(Primitive::Int32);

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

pub fn typecheck_array_type(array_type_name: &ast::ArrayTypeName, ctx: &mut Context) -> TypeResult<ArrayTypeName> {
    let element_ty = typecheck_typename(array_type_name.element.as_ref(), ctx)?;

    let (array_ty, array_dim) = match &array_type_name.dim {
        Some(dim) => {
            let dim_expr = typecheck_expr(&dim.dim_expr, &ARRAY_DIM_TY, ctx)?;
            let dim_val = const_eval_integer(&dim_expr, ctx)?;

            let dim_size = dim_val.value
                .as_i32()
                .ok_or_else(|| TypeError::TypeMismatch {
                    span: dim_expr.span().clone(),
                    actual: dim_expr.annotation().ty().into_owned(),
                    expected: ARRAY_DIM_TY,
                })?;
            
            let Ok(dim_size) = usize::try_from(dim_size) else {
                // use the dynarray type for the error because we don't have a valid static dimension
                return Err(TypeError::IndexOutOfBounds {
                    index: dim_val.value,
                    span: dim_expr.span().clone(),
                    base_ty: Box::new(Type::dyn_array(element_ty.ty().clone())),
                });
            };

            let array_ty = Type::from(ArrayType {
                element_ty: element_ty.ty().clone(),
                dim: dim_size,
            }).indirect_by(array_type_name.indirection);

            (array_ty, Some(Box::new(ast::ArrayTypeNameDim {
                open_bracket: dim.open_bracket.clone(),
                dim_expr,
                close_bracket: dim.close_bracket.clone(),
            })))
        }

        None => {
            let dyn_array_ty = Type::DynArray {
                element: Arc::new(element_ty.ty().clone()),
            };

            (dyn_array_ty, None)
        }
    };

    Ok(ArrayTypeName {
        element: Box::new(element_ty),
        indirection: array_type_name.indirection,
        span: array_type_name.span.clone(),
        ty: array_ty,
        array_kw: array_type_name.array_kw.clone(),
        of_kw: array_type_name.of_kw.clone(),
        dim: array_dim,
    })
}
