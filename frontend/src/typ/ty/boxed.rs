use crate::ast;
use crate::typ::typecheck_typename;
use crate::typ::BoxTypeName;
use crate::typ::Context;
use crate::typ::Type;
use crate::typ::TypeResult;
use std::sync::Arc;

pub fn typecheck_box_type(
    box_type_name: &ast::BoxTypeName,
    ctx: &mut Context,
) -> TypeResult<BoxTypeName> {
    let value_ty = typecheck_typename(box_type_name.value.as_ref(), ctx)?;

    let box_ty = Type::Box(Arc::new(value_ty.ty().clone()));

    Ok(BoxTypeName {
        box_kw: box_type_name.box_kw.clone(),
        value: Box::new(value_ty),
        indirection: box_type_name.indirection,
        span: box_type_name.span.clone(),
        ty: box_ty,
        of_kw: box_type_name.of_kw.clone(),
    })
}
