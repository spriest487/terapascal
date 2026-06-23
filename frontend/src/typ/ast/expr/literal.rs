use crate::ast;
use crate::typ::ast::Expr;
use crate::typ::builtin_typeinfo_name;
use crate::typ::string_to_char_lit;
use crate::typ::typecheck_typename;
use crate::typ::ConstValue;
use crate::typ::Context;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeName;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::IntConstant;
use crate::RealConstant;
use std::sync::Arc;
use terapascal_common::span::Span;

pub type Literal = ast::Literal<Value>;
pub type LiteralItem = ast::LiteralItem<Value>;

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
    lit: &ast::Literal,
    expect_ty: &Type,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    match lit {
        ast::Literal::String(s) => {
            // if we're expecting a number, and the literal is one char long, we treat the string
            // as a character literal of its first character
            if expect_ty.is_integer() {
                if let Some(char_lit) = string_to_char_lit(s.as_str()) {
                    let val = TypedValue::literal(expect_ty.clone(), span.clone());
                    return Ok(Expr::literal(char_lit, Value::from(val)));
                }
            }

            let const_val = ConstValue::string_literal(s.clone(), span.clone());

            Ok(Expr::literal(Literal::String(s.clone()), const_val))
        }

        ast::Literal::Boolean(b) => {
            let bool_val = Literal::Boolean(*b);
            let const_val = ConstValue::literal(bool_val.clone(), Primitive::Boolean, span.clone());

            Ok(Expr::literal(bool_val, const_val))
        }

        ast::Literal::Integer(i) => {
            typecheck_literal_int(i, expect_ty, span.clone())
        },

        ast::Literal::Real(x) => {
            let ty = if *expect_ty == Type::Primitive(Primitive::Real32) && x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else if x.as_f64().is_some() {
                Type::from(Primitive::Real64)
            } else {
                unimplemented!("real literal outside range of f64");
            };

            let real_value = ast::Literal::Real(x.clone());
            let const_value = ConstValue::literal(real_value.clone(), ty, span.clone());

            Ok(ast::Expr::literal(
                real_value,
                const_value,
            ))
        }

        ast::Literal::Nil => {
            let has_nil_ty = if ctx.allow_unsafe() {
                // in unsafe contexts, nil literals can be assigned/compared directly to object
                // pointers, and should act like a nil pointer of that type
                expect_ty.is_typed_pointer() || expect_ty.is_object()
            } else {
                // otherwise, nil can only have a pointer type when treated as a raw pointer
                expect_ty.is_typed_pointer()
            };
            
            let nil_ty = if has_nil_ty {
               expect_ty.clone() 
            } else {
                Type::Nothing.ptr()
            };
            
            let value = ConstValue {
                decl: None,
                ty: nil_ty,
                
                value: Literal::Nil,
                
                span: span.clone(),
            };

            Ok(Expr::literal(Literal::Nil, value))
        }

        ast::Literal::SizeOf(size_of_ty) => {
            let ty = typecheck_typename(&size_of_ty, ctx)?;
            let size_value = Literal::SizeOf(Box::new(ty));
            let value = ConstValue::literal(size_value.clone(), Primitive::Int32, span.clone());

            Ok(Expr::literal(size_value, value))
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
            if !ctx.opts().rtti {
                return Err(TypeError::RTTIDisabled { 
                    span: span.clone(),
                });
            }
            
            let ty = typecheck_typename(typename, ctx)?; 
            
            let typeinfo_type = Type::Class(Arc::new(builtin_typeinfo_name()));
            let val = TypedValue::temp(typeinfo_type, span.clone());
            
            Ok(Expr::literal(Literal::TypeInfo(Box::new(ty)), Value::from(val)))
        }
    }
}

pub fn create_default_literal(typename: TypeName, span: Span) -> Expr {
    let ty = typename.ty().clone();
    let default_lit = Literal::DefaultValue(Box::new(typename.clone()));

    let value = ConstValue::literal(default_lit, ty, span);

    Expr::literal(
        Literal::DefaultValue(Box::new(typename)),
        value,
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
        Type::Primitive(Primitive::Real64) => {
            try_map_primitive_int(i, Primitive::Real32, IntConstant::as_f64)
        }

        _ => match i.as_i32() {
            Some(_) => Type::from(Primitive::Int32),
            None => unimplemented!("integer literals outside range of i32"),
        },
    };

    let int_value = ast::Literal::Integer(*i);
    let value = ConstValue::literal(int_value.clone(), ty, span);
    
    Ok(ast::Expr::literal(int_value, value))
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
