use crate::ast;
use crate::ast::TypeAnnotation;
use crate::typ::ast::Expr;
use crate::typ::builtin_typeinfo_name;
use crate::typ::string_to_char_lit;
use crate::typ::string_type;
use crate::typ::typecheck_typename;
use crate::typ::Context;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeName;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::ValueKind;
use crate::typ::STRING_CHAR_TYPE;
use crate::IntConstant;
use crate::RealConstant;
use std::sync::Arc;
use terapascal_common::span::Span;

pub type Literal = ast::Literal<TypeName>;

impl Literal {
    pub fn as_string(&self) -> Option<&Arc<String>> {
        match self {
            Literal::String(value) => Some(value),
            _ => None,
        }
    }
    
    pub fn cast_to_primitive(&self, to_primitive: Primitive) -> Option<Literal> {
        match self {
            Literal::Nil if to_primitive.is_pointer() => Some(Literal::Nil),

            Literal::Integer(int_val) => match to_primitive {
                p if p.is_integer() && !p.is_pointer() => {
                    Some(self.clone())
                }

                p if p.is_real() => {
                    let real_val = int_val.as_f64()?;
                    Some(Literal::Real(RealConstant::from(real_val)))
                }

                _ => None,
            }

            Literal::Real(real_val) => match to_primitive {
                p if p.is_real() => {
                    Some(self.clone())
                }

                p if p.is_integer() && !p.is_pointer() => {
                    let int_val = real_val.as_f64()?.round() as i128;
                    Some(Literal::Integer(IntConstant::from(int_val)))
                }

                _ => None,
            }

            Literal::Boolean(bool_val) => match to_primitive {
                Primitive::Boolean => Some(self.clone()),

                p if p.is_integer() && !p.is_pointer() => {
                    let int_val = if *bool_val { 1 } else { 0 };
                    Some(Literal::Integer(IntConstant::from(int_val)))
                }

                p if p.is_real() => {
                    let real_val = if *bool_val { 1.0 } else { 0.0 };
                    Some(Literal::Real(RealConstant::from(real_val)))
                }

                _ => None,
            }

            _ => None,
        }
    }

    pub fn try_bitwise_not(self) -> Option<Self> {
        match self {
            Literal::Integer(i) => {
                let operand_val = i.as_u64()?;
                Some(Literal::Integer(IntConstant::from(!operand_val)))
            }

            _ => None
        }
    }

    pub fn try_negate(self) -> Option<Self> {
        match self {
            Literal::Boolean(b) => Some(Literal::Boolean(!b)),

            Literal::Integer(i) => {
                let operand_val = i.as_i128();
                Some(Literal::Integer(IntConstant::from(-operand_val)))
            }

            Literal::Real(r) => {
                let operand_val = r.0;
                Some(Literal::Real(RealConstant(-operand_val)))
            }

            _ => None
        }
    }

    pub fn try_eq(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::String(a), Literal::String(b)) => {
                Some(Literal::Boolean(a == b))
            }

            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Boolean(a == b))
            }
            (Literal::Integer(a), Literal::Integer(b)) => {
                Some(Literal::Boolean(a == b))
            }
            (Literal::Nil, Literal::Nil) => {
                Some(Literal::Boolean(true))
            }
            (Literal::Boolean(a), Literal::Boolean(b)) => {
                Some(Literal::Boolean(a == b))
            }
            _ => None,
        }
    }

    pub fn try_add(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::String(a), Literal::String(b)) => {
                let s = (*a).clone() + b.as_str();
                Some(Literal::String(s.into()))
            }
            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Real(a + b))
            }
            (Literal::Integer(a), Literal::Integer(b)) => {
                Some(Literal::Integer(a + b))
            }
            _ => None,
        }
    }

    pub fn try_sub(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Real(a - b))
            }
            (Literal::Integer(a), Literal::Integer(b)) => {
                Some(Literal::Integer(a - b))
            }
            _ => None,
        }
    }

    pub fn try_mul(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Real(a * b))
            }
            (Literal::Integer(a), Literal::Integer(b)) => {
                Some(Literal::Integer(a * b))
            }
            _ => None,
        }
    }

    pub fn try_div(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Real((a / b).round()))
            }
            (Literal::Integer(a), Literal::Integer(b)) => {
                Some(Literal::Integer(a / b))
            }
            _ => None,
        }
    }

    pub fn try_fdiv(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Real(a / b))
            }
            _ => None,
        }
    }

    pub fn try_and(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Boolean(a), Literal::Boolean(b)) => {
                Some(Literal::Boolean(a && b))
            }
            _ => None,
        }
    }

    pub fn try_or(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Boolean(a), Literal::Boolean(b)) => {
                Some(Literal::Boolean(a || b))
            }
            _ => None,
        }
    }

    pub fn try_gt(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Boolean(a > b))
            }
            (Literal::Integer(a), Literal::Integer(b)) => {
                Some(Literal::Boolean(a > b))
            }
            _ => None,
        }
    }

    pub fn try_lt(self, b: Literal) -> Option<Literal> {
        match (self, b) {
            (Literal::Real(a), Literal::Real(b)) => {
                Some(Literal::Boolean(a < b))
            }
            (Literal::Integer(a), Literal::Integer(b)) => {
                Some(Literal::Boolean(a < b))
            }
            _ => None,
        }
    }

    pub fn try_bitwise<Op>(self, b: Literal, op: Op) -> Option<Literal>
    where
        Op: Fn(u64, u64) -> u64,
    {
        match (self, b) {
            (Literal::Integer(a), Literal::Integer(b)) => {
                let val = op(a.as_u64()?, b.as_u64()?);
                Some(Literal::Integer(IntConstant::from(val)))
            }

            _ => None
        }
    }
    
    pub fn try_into_int(self) -> Option<IntConstant> {
        match self {
            Literal::Integer(int) => Some(int),
            _ => None,
        }
    }
}

pub fn typecheck_literal(
    lit: &ast::Literal<ast::TypeName>,
    expect_ty: &Type,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    match lit {
        ast::Literal::String(s) => {
            // if we're expecting a char, and the literal is one char long, we can do that instead
            let char_ty = Type::from(STRING_CHAR_TYPE);
            if *expect_ty == char_ty {
                if let Some(char_lit) = string_to_char_lit(s.as_str()) {
                    let val = TypedValue::temp(char_ty, span.clone());
                    return Ok(Expr::literal(char_lit, Value::from(val)));
                }
            }

            let binding = ValueKind::Immutable;
            let annotation = TypedValue {
                ty: string_type(ctx)?,
                value_kind: binding,
                span: span.clone(),
                decl: None,
            }
                .into();

            Ok(Expr::literal(Literal::String(s.clone()), annotation))
        }

        ast::Literal::Boolean(b) => {
            let annotation = TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Immutable,
                span: span.clone(),
                decl: None,
            }
                .into();

            Ok(Expr::literal(Literal::Boolean(*b), annotation))
        }

        ast::Literal::Integer(i) => typecheck_literal_int(i, expect_ty, span.clone()),

        ast::Literal::Real(x) => {
            let ty = if x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypedValue {
                ty,
                value_kind: ValueKind::Immutable,
                span: span.clone(),
                decl: None,
            }
                .into();

            Ok(ast::Expr::literal(
                ast::Literal::Real(x.clone()),
                annotation,
            ))
        }

        ast::Literal::Nil => {
            let ty = match expect_ty {
                ptr @ Type::Pointer(..) => ptr.clone(),
                _ => Type::Nil,
            };

            let annotation = TypedValue {
                ty,
                value_kind: ValueKind::Temporary,
                span: span.clone(),
                decl: None,
            };

            Ok(Expr::literal(Literal::Nil, annotation.into()))
        }

        ast::Literal::SizeOf(size_of_ty) => {
            let ty = typecheck_typename(&size_of_ty, ctx)?;
            let annotation = TypedValue {
                ty: Type::Primitive(Primitive::Int32),
                span: span.clone(),
                decl: None,
                value_kind: ValueKind::Temporary,
            };

            Ok(Expr::literal(
                Literal::SizeOf(Box::new(ty)),
                annotation.into(),
            ))
        }

        ast::Literal::DefaultValue(default_of_ty) => {
            let ty = if !default_of_ty.is_known() {
                if *expect_ty == Type::Nothing {
                    return Err(TypeError::UnableToInferType {
                        expr: Box::new(ast::Expr::literal(lit.clone(), span.clone())),
                    });
                } else {
                    TypeName::inferred(expect_ty.clone())
                }
            } else {
                typecheck_typename(default_of_ty, ctx)?
            };

            ty.expect_sized(ctx, span)?;

            let has_default = ty
                .has_default(ctx)
                .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

            if !has_default {
                return Err(TypeError::NotDefaultable {
                    ty: Type::from(ty),
                    span: span.clone(),
                });
            }

            Ok(create_default_literal(ty, span.clone()))
        }

        ast::Literal::TypeInfo(typename) => {
            let ty = typecheck_typename(typename, ctx)?; 
            
            let typeinfo_type = Type::Class(Arc::new(builtin_typeinfo_name()));
            let val = TypedValue::temp(typeinfo_type, span.clone());
            
            Ok(Expr::literal(Literal::TypeInfo(Box::new(ty)), Value::from(val)))
        }
    }
}

pub fn create_default_literal(typename: TypeName, span: Span) -> Expr {
    let annotation = TypedValue {
        ty: typename.ty().clone(),
        decl: None,
        span: span.clone(),
        value_kind: ValueKind::Temporary,
    };

    Expr::literal(
        Literal::DefaultValue(Box::new(typename)),
        annotation.into(),
    )
}

fn typecheck_literal_int(i: &IntConstant, expect_ty: &Type, span: Span) -> TypeResult<Expr> {
    let ty = match expect_ty {
        Type::Primitive(Primitive::UInt8) => {
            try_map_primitive_int(i, Primitive::UInt8, IntConstant::as_u8)
        }
        Type::Primitive(Primitive::Int8) => {
            try_map_primitive_int(i, Primitive::Int8, IntConstant::as_i8)
        }
        Type::Primitive(Primitive::Int16) => {
            try_map_primitive_int(i, Primitive::Int16, IntConstant::as_i16)
        }
        Type::Primitive(Primitive::UInt16) => {
            try_map_primitive_int(i, Primitive::UInt16, IntConstant::as_u16)
        }
        Type::Primitive(Primitive::Int32) => {
            try_map_primitive_int(i, Primitive::Int32, IntConstant::as_i32)
        }
        Type::Primitive(Primitive::UInt32) => {
            try_map_primitive_int(i, Primitive::UInt32, IntConstant::as_u32)
        }
        Type::Primitive(Primitive::Int64) => {
            try_map_primitive_int(i, Primitive::Int64, IntConstant::as_i64)
        }
        Type::Primitive(Primitive::UInt64) => {
            try_map_primitive_int(i, Primitive::UInt64, IntConstant::as_u64)
        }
        Type::Primitive(Primitive::NativeInt) => {
            try_map_primitive_int(i, Primitive::NativeInt, IntConstant::as_isize)
        }
        Type::Primitive(Primitive::NativeUInt) => {
            try_map_primitive_int(i, Primitive::NativeUInt, IntConstant::as_usize)
        }

        Type::Primitive(Primitive::Real32) => {
            try_map_primitive_int(i, Primitive::Real32, IntConstant::as_f32)
        }

        _ => match i.as_i32() {
            Some(_) => Type::from(Primitive::Int32),
            None => unimplemented!("integer literals outside range of i32"),
        },
    };

    let annotation = TypedValue {
        ty,
        value_kind: ValueKind::Immutable,
        span,
        decl: None,
    }
        .into();

    Ok(ast::Expr::literal(ast::Literal::Integer(*i), annotation))
}

fn try_map_primitive_int<F, T>(i: &IntConstant, primitive_ty: Primitive, f: F) -> Type
where
    F: Fn(&IntConstant) -> Option<T>,
{
    match f(&i) {
        Some(..) => Type::Primitive(primitive_ty),
        None => Type::Primitive(Primitive::Int32),
    }
}
