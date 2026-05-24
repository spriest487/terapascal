use crate::c::type_map::TypeID;
use crate::c::CBuilder;
use crate::c::BuiltinName;
use crate::c::DynArrayTypeID;
use crate::c::FieldName;
use crate::c::FunctionName;
use crate::c::GlobalName;
use crate::c::Statement;
use crate::c::Type;
use crate::c::Unit;
use crate::c::VariableID;
use crate::ir;
use ir::MetadataSource as _;
use std::fmt;

#[allow(unused)]
#[derive(Clone, PartialEq, Debug)]
pub enum InfixOp {
    Eq,
    Neq,
    Assign,
    Add,
    Sub,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Assign => write!(f, "="),
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Gte => write!(f, ">="),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Lte => write!(f, "<="),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Rem => write!(f, "%"),
            InfixOp::Shl => write!(f, "<<"),
            InfixOp::Shr => write!(f, ">>"),
            InfixOp::BitAnd => write!(f, "&"),
            InfixOp::BitOr => write!(f, "|"),
            InfixOp::BitXor => write!(f, "^"),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum PrefixOp {
    Not,
    BitNot,
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrefixOp::Not => write!(f, "!"),
            PrefixOp::BitNot => write!(f, "~"),
        }
    }
}

#[allow(unused)]
#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Variable(VariableID),
    Function(FunctionName),
    Deref(Box<Expr>),
    Global(GlobalName), // global value
    LitCString(String), // C string literal
    LitBool(bool),
    LitInt(i128),
    LitFloat(f64),
    Null,
    Index {
        lhs: Box<Expr>,
        index: Box<Expr>,
    },
    InfixOp {
        lhs: Box<Expr>,
        op: InfixOp,
        rhs: Box<Expr>,
    },
    PrefixOp {
        op: PrefixOp,
        operand: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    AddrOf(Box<Expr>),
    Field {
        base: Box<Expr>,
        field: FieldName,
    },
    Arrow {
        base: Box<Expr>,
        field: FieldName,
    },
    Cast(Box<Expr>, Type),
    SizeOf(Type),
}

impl Expr {
    pub fn translate_val(v: &ir::Value, builder: &mut CBuilder) -> Self {
        match v {
            ir::Value::LiteralNil => Expr::Null,
            ir::Value::LiteralBool(b) => Expr::LitBool(*b),
            ir::Value::LiteralI8(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralU8(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralI16(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralU16(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralI32(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralU32(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralI64(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralU64(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralISize(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralUSize(i) => Expr::LitInt(*i as i128),
            ir::Value::LiteralF32(f) => Expr::LitFloat(f64::from(*f)),
            ir::Value::LiteralF64(f) => Expr::LitFloat(f64::from(*f)),
            
            ir::Value::Ref(r) => {
                Expr::translate_ref(r, builder)
            },

            ir::Value::SizeOf(ty) => {
                let ty = builder.translate_type(ty);
                Expr::SizeOf(ty)
            }
            
            ir::Value::Default(ty) => {
                match ty {
                    ir::Type::Generic(..) => {
                        panic!("code output should never contain generic placeholders")
                    },

                    ir::Type::Function(_)
                    | ir::Type::TempRef(_) 
                    | ir::Type::Pointer(_)
                    | ir::Type::Nothing
                    | ir::Type::Object(_)
                    | ir::Type::WeakObject(_) => Expr::Null,
                    
                    ir::Type::Bool => Expr::LitBool(false),
                    ir::Type::U8 
                    | ir::Type::I8 
                    | ir::Type::I16 
                    | ir::Type::U16
                    | ir::Type::I32
                    | ir::Type::U32
                    | ir::Type::I64
                    | ir::Type::U64
                    | ir::Type::USize
                    | ir::Type::ISize
                    | ir::Type::F32
                    | ir::Type::F64 => Expr::LitInt(0),

                    ir::Type::Struct { .. }
                    | ir::Type::Variant { .. }
                    | ir::Type::Flags(_)
                    | ir::Type::Array { .. } => {
                        let ty = builder.translate_type(ty);
                        let ty_size = Expr::SizeOf(ty.clone());
                        
                        let val_var = builder.new_temp_var(ty, false);
                        builder.stmts.push(Statement::Expr(
                            Expr::Function(FunctionName::Builtin(BuiltinName::ZeroMemory)).call([
                                Expr::Variable(val_var.clone())
                                    .addr_of()
                                    .cast(Type::UChar.ptr()),
                                ty_size,
                            ])
                        ));
                        
                        Expr::Variable(val_var)
                    }
                }
            }
        }
    }

    pub fn translate_ref(r: &ir::Ref, builder: &mut CBuilder) -> Self {
        match r {
            ir::Ref::Discard => {
                panic!("can't translate a discard ref, it should only be used in assignments")
            },
            ir::Ref::Result => {
                VariableID::Result.to_expr()
            }
            ir::Ref::Arg(id) => {
                VariableID::Arg(*id).to_expr()
            }
            ir::Ref::Local(local_id) => {
                VariableID::local(*local_id).to_expr()
            },
            ir::Ref::Deref(inner) => {
                Expr::translate_val(inner.as_ref(), builder).deref()
            },
            ir::Ref::Global(ir::GlobalRef::Function(key)) => {
                let instance = builder.translate_function_ref(key);
                Expr::Function(instance.name)
            },
            ir::Ref::Global(ir::GlobalRef::StringLiteral(id)) => {
                let name = GlobalName::StringLiteral(*id);
                Expr::Global(name).addr_of()
            },
            ir::Ref::Global(ir::GlobalRef::Variable(id)) => {
                let name = GlobalName::Variable(*id);
                Expr::Global(name)
            }
            ir::Ref::Global(ir::GlobalRef::StaticTagArray(id)) => {
                let name = GlobalName::StaticTagArray(*id);
                Expr::Global(name).addr_of()
            }
            ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(ty)) => {
                if builder.opts().debug {
                    let comment = format!("typeinfo ref: {}", ty.to_pretty_string(builder.metadata()));
                    builder.stmts.push(Statement::Comment(comment));
                }

                if builder.metadata().get_type_info(ty).is_some() {
                    let type_id = builder.create_type_id(ty);
                    let name = GlobalName::StaticTypeInfo(type_id);
                    Expr::Global(name).addr_of()
                } else {
                    Expr::Null
                }
            }
            ir::Ref::Global(ir::GlobalRef::StaticFuncInfo(id)) => {
                let name = GlobalName::StaticFuncInfo(*id);
                Expr::Global(name).addr_of()
            }
            ir::Ref::Field(field_ref) => {
                Self::translate_field(&field_ref.instance, &field_ref.instance_type, field_ref.field, builder)
            }
            ir::Ref::Element(el_ref) => {
                Self::translate_element(&el_ref.instance, &el_ref.index, &el_ref.instance_type, builder)
            }
            ir::Ref::VariantTag(tag_ref) => {
                Self::translate_variant_tag(&tag_ref.instance, builder)
            }
            ir::Ref::VariantData(data_ref) => {
                Self::translate_variant_data(&data_ref.instance, data_ref.case_index, builder)
            }
        }
    }

    pub fn result_var() -> Self {
        Expr::Variable(VariableID::Result)
    }
    
    pub fn local_var(id: ir::LocalID) -> Self {
        Expr::Variable(VariableID::Local(id))
    }
    
    pub fn arg_var(id: ir::ArgID) -> Self {
        Expr::Variable(VariableID::Arg(id))
    }
    
    pub fn named_var(name: impl Into<String>) -> Self {
        Expr::Variable(VariableID::Named(Box::new(name.into())))
    }
    
    pub fn call(self, args: impl IntoIterator<Item=Self>) -> Self {
        Expr::Call {
            func: Box::new(self),
            args: args.into_iter().collect(),
        }
    }

    pub fn call_new(class_type: &ir::Type, immortal: bool, unit: &mut Unit) -> Self {
        let new = Expr::Function(FunctionName::Builtin(BuiltinName::RcNew));

        let class_c_type = unit.translate_type(class_type);
        let class_type_index = unit.get_type_id(class_type);

        let class_ptr = Expr::class_ptr(class_type_index);

        let instance = new.call([
            class_ptr, 
            Expr::LitBool(immortal),
        ]);

        instance.cast(class_c_type)
    }

    pub fn call_newarray(array_id: DynArrayTypeID, len: impl Into<Expr>, immortal: bool) -> Self {
        let new = Expr::Function(FunctionName::Builtin(BuiltinName::RcNewArray));
        let array_class_ptr = Expr::dyn_array_class_ptr(array_id);

        let instance = new.call([
            array_class_ptr, 
            len.into(), 
            Expr::LitBool(immortal),
        ]);
        
        instance.cast(Type::dyn_array_ptr(array_id))
    }

    pub fn deref(self) -> Self {
        Expr::Deref(Box::new(self))
    }
    
    pub fn index(self, index: Self) -> Self {
        Expr::Index {
            lhs: Box::new(self),
            index: Box::new(index),
        }
    }

    pub fn addr_of(self) -> Self {
        Expr::AddrOf(Box::new(self))
    }

    pub fn arrow(self, field: FieldName) -> Self {
        Expr::Arrow {
            base: Box::new(self),
            field,
        }
    }

    pub fn infix_op(self, op: InfixOp, rhs: Self) -> Self {
        Expr::InfixOp {
            lhs: Box::new(self),
            op,
            rhs: Box::new(rhs),
        }
    }

    pub fn cast(self, ty: Type) -> Self {
        Expr::Cast(Box::new(self), ty)
    }
    
    pub fn field(self, field: FieldName) -> Self {
        Expr::Field {
            base: Box::new(self),
            field,
        }
    }

    pub fn assign(lhs: impl Into<Box<Self>>, rhs: impl Into<Box<Self>>) -> Self {
        Expr::InfixOp {
            lhs: lhs.into(),
            op: InfixOp::Assign,
            rhs: rhs.into(),
        }
    }

    pub fn assign_from(self, rhs: impl Into<Box<Self>>) -> Self {
        Expr::assign(self, rhs)
    }
    
    pub fn not(self) -> Self {
        Expr::PrefixOp {
            op: PrefixOp::Not,
            operand: Box::new(self),
        }
    }

    pub(crate) fn translate_infix_op(
        lhs: &ir::Value,
        op: InfixOp,
        rhs: &ir::Value,
        builder: &mut CBuilder,
    ) -> Self {
        let lhs_expr = Expr::translate_val(lhs, builder);
        let rhs_expr = Expr::translate_val(rhs, builder);

        Self::infix_op(lhs_expr, op, rhs_expr)
    }

    pub fn translate_assign(out: &ir::Ref, val: Self, builder: &mut CBuilder) -> Self {
        match out {
            ir::Ref::Discard => val,
            _ => {
                let out_ref = Expr::translate_ref(out, builder);
                Self::infix_op(out_ref, InfixOp::Assign, val)
            },
        }
    }
    
    pub fn array_class_ptr(arr_obj: &Expr, v_id: &ir::ObjectID, unit: &mut Unit) -> Expr {
        match v_id {
            ir::ObjectID::Array(element_ty) => {
                let (_, dyn_array_id) = unit.translate_dyn_array_type(element_ty);
                dyn_array_id.class_ptr()
            }

            _ => {
                let obj_class_ptr = arr_obj.clone().arrow(FieldName::ObjectClass);
                obj_class_ptr.cast(Type::DynArrayClass.ptr())
            },
        }
    }

    pub fn translate_element(
        arr: &ir::Ref,
        index: &ir::Value,
        of_type: &ir::Type,
        builder: &mut CBuilder,
    ) -> Self {
        let base_expr = Expr::translate_ref(arr, builder);
        let index_expr = Expr::translate_val(index, builder);
        
        match of_type {
            ir::Type::Object(ir::ObjectID::Array(element_type)) => {
                let arr_id = builder.get_dyn_array_type(element_type);
                let arr_as_obj = base_expr.cast(Type::object_ptr());
                
                let element_ptr_type = builder.translate_type(&element_type.as_ref().clone().ptr());

                arr_id.class_ptr()
                    .arrow(FieldName::DynArrayClassElement)
                    .call([arr_as_obj, index_expr])
                    .cast(element_ptr_type)
            }

            ir::Type::Object(ir::ObjectID::Box(value_type)) => {
                let box_id = builder.get_box_type(value_type);

                base_expr.cast(box_id.ptr_type())
                    .arrow(FieldName::BoxValue)
                    .addr_of()
            }
            
            ir::Type::Array { .. } => {
                // static array
                let elements_expr = base_expr.field(FieldName::StaticArrayElements);

                // the field is already an array, so the size info is encoded in the type
                elements_expr
                    .index(index_expr)
                    .addr_of()
            }
            
            _ => {
                panic!("translate_element: invalid base type for element instruction ({of_type})")
            }
        }
    }

    pub fn translate_length(
        arr: &ir::Ref,
        of_type: &ir::Type,
        builder: &mut CBuilder,
    ) -> Self {
        let array_expr = Expr::translate_ref(arr, builder);

        match of_type {
            ir::Type::Object(type_id) => {
                let arr_obj = array_expr.cast(Type::object_ptr());
                let class_ptr = builder.array_class_ptr(&arr_obj, type_id);

                class_ptr
                    .arrow(FieldName::DynArrayClassLength)
                    .call([arr_obj])
            }

            ir::Type::Array { dim, .. } => {
                i128::try_from(*dim)
                    .map(Expr::LitInt)
                    .expect("array size couldn't be converted to literal")
            }
            
            _ => {
                Expr::LitInt(1)
            }
        }
    }

    pub fn translate_field(
        a: &ir::Ref,
        of_ty: &ir::Type,
        field_id: ir::FieldID,
        builder: &mut CBuilder,
    ) -> Self {
        let a_expr = Expr::translate_ref(a, builder);

        match of_ty {
            ir::Type::Object(class_id) => {
                // pointer to RC containing pointer to class resource
                match class_id {
                    // cast closures are of unknown type, but we don't need a real vtable to call
                    // them. cast to the AnonymousClosure struct which is guaranteed to have the
                    // same layout as the first two fields of any closure struct
                    ir::ObjectID::AnyClosure(func_ty_id) if field_id == ir::CLOSURE_PTR_FIELD => {
                        let cast_to_anon_closure = a_expr.cast(Type::AnonymousClosure.ptr());
                        let func_ptr = cast_to_anon_closure.arrow(FieldName::ClosureFunctionPointer).addr_of();

                        let func_ptr_ty = builder.translate_type(&func_ty_id.to_function_type());

                        func_ptr.cast(func_ptr_ty.ptr())
                    },

                    // normal class: it's just a field accessed through this pointer
                    ir::ObjectID::Class(..) => {
                        a_expr.arrow(FieldName::ID(field_id)).addr_of()
                    },

                    _ => panic!(
                        "bad resource type {:?} in Field instruction target",
                        class_id
                    ),
                }
            },

            _ => {
                // local struct
                let field = Expr::Field {
                    base: Box::new(a_expr),
                    field: FieldName::ID(field_id),
                };

                field.addr_of()
            },
        }
    }

    pub fn translate_variant_tag(
        instance: &ir::Ref,
        builder: &mut CBuilder
    ) -> Expr {
        let instance_expr = Expr::translate_ref(instance, builder);

        let tag_field_expr = Expr::Field {
            base: Box::new(instance_expr),
            field: FieldName::VariantTag,
        };

        tag_field_expr.addr_of()
    }

    pub fn translate_variant_data(
        instance: &ir::Ref,
        case_index: usize,
        builder: &mut CBuilder
    ) -> Expr {
        let data_field = Expr::Field {
            base: Box::new(Expr::translate_ref(instance, builder)),
            field: FieldName::VariantData,
        };

        let case_field = Expr::Field {
            base: Box::new(data_field),
            field: FieldName::VariantDataCase(case_index),
        };

        case_field.addr_of()
    }

    pub fn class_ptr(type_index: TypeID) -> Self {
        // dyn arrays use a different class type, but it's castable to the normal Class struct
        Expr::class(type_index).addr_of().cast(Type::Class.ptr())
    }
    
    pub fn class(type_index: TypeID) -> Self {
        Expr::Global(GlobalName::ClassInstance(type_index))
    }

    pub fn dyn_array_class(array_id: DynArrayTypeID) -> Self {
        Expr::Global(GlobalName::DynArrayClassInstance(array_id))
    }

    pub fn dyn_array_class_ptr(array_id: DynArrayTypeID) -> Self {
        Expr::Global(GlobalName::DynArrayClassInstance(array_id)).addr_of()
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Global(name) => write!(f, "{}", name),
            Expr::LitCString(s) => write!(f, "\"{}\"", s.escape_default()),
            Expr::LitFloat(x) => write!(f, "{}", x),
            Expr::LitInt(i) => write!(f, "{}", i),
            Expr::LitBool(b) => write!(f, "{}", b),
            Expr::Deref(inner) => write!(f, "(*({}))", inner),
            Expr::AddrOf(inner) => write!(f, "&({})", inner),
            Expr::Index { lhs, index } => write!(f, "({}[{}])", lhs, index),
            Expr::InfixOp { lhs, op, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::PrefixOp { op, operand } => write!(f, "({}({}))", op, operand),
            Expr::Null => write!(f, "NULL"),
            Expr::Variable(id) => write!(f, "{}", id),
            Expr::Function(name) => write!(f, "{}", name),
            Expr::Call { func, args } => {
                write!(f, "{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            },
            Expr::Field { base, field } => write!(f, "({}).{}", base, field),
            Expr::Arrow { base, field } => write!(f, "({})->{}", base, field),
            Expr::Cast(value, ty) => write!(f, "(({}){})", ty.typename(), value),
            Expr::SizeOf(ty) => write!(f, "sizeof({})", ty.typename()),
        }
    }
}
