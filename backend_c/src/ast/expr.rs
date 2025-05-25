use crate::ast::FieldName;
use crate::ast::FunctionName;
use crate::ast::GlobalName;
use crate::ast::Unit;
use crate::ast::Type;
use crate::ast::TypeDefName;
use crate::ir;
use std::fmt;
use ir_lang::LocalID;
use crate::c::VariableID;

#[allow(unused)]
#[derive(Clone, PartialEq, Debug)]
pub enum InfixOp {
    Eq,
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
    Class(ir::TypeDefID),
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
    Element {
        base: Box<Expr>,
        index: Box<Expr>,
    },
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
    pub fn translate_val(v: &ir::Value, module: &mut Unit) -> Self {
        match v {
            ir::Value::LiteralBool(b) => Expr::LitBool(*b),
            ir::Value::LiteralNull => Expr::Null,
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
            ir::Value::Ref(r) => Expr::translate_ref(r, module),
            ir::Value::SizeOf(ty) => {
                let ty = Type::from_metadata(ty, module);
                Expr::SizeOf(ty)
            }
        }
    }

    pub fn translate_ref(r: &ir::Ref, module: &mut Unit) -> Self {
        match r {
            ir::Ref::Discard => {
                panic!("can't translate a discard ref, it should only be used in assignments")
            },
            ir::Ref::Local(local_id) => Expr::Variable(VariableID::Local(*local_id)),
            ir::Ref::Deref(inner) => Expr::translate_val(inner.as_ref(), module).deref(),
            ir::Ref::Global(ir::GlobalRef::Function(id)) => {
                let name = module.function_name(*id);
                Expr::Function(name)
            },
            ir::Ref::Global(ir::GlobalRef::StringLiteral(id)) => {
                let name = GlobalName::StringLiteral(*id);
                Expr::Global(name).addr_of()
            },
            ir::Ref::Global(ir::GlobalRef::StaticClosure(id)) => {
                let name = GlobalName::StaticClosure(*id);
                Expr::Global(name)
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
                let name = GlobalName::StaticTypeInfo(ty.clone());
                Expr::Global(name).addr_of()
            }
            ir::Ref::Global(ir::GlobalRef::StaticFuncInfo(id)) => {
                let name = GlobalName::StaticFuncInfo(*id);
                Expr::Global(name).addr_of()
            }
        }
    }
    
    pub fn local_var(id: LocalID) -> Self {
        Expr::Variable(VariableID::Local(id))
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

    pub fn infix_op(lhs: Self, op: InfixOp, rhs: Self) -> Self {
        Expr::InfixOp {
            lhs: Box::new(lhs),
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
        module: &mut Unit,
    ) -> Self {
        let lhs_expr = Expr::translate_val(lhs, module);
        let rhs_expr = Expr::translate_val(rhs, module);

        Self::infix_op(lhs_expr, op, rhs_expr)
    }

    pub fn translate_assign(out: &ir::Ref, val: Self, module: &mut Unit) -> Self {
        match out {
            ir::Ref::Discard => val,
            _ => {
                let out_ref = Expr::translate_ref(out, module);
                Self::infix_op(out_ref, InfixOp::Assign, val)
            },
        }
    }

    pub fn translate_element(arr: &ir::Ref, index: &ir::Value, module: &mut Unit) -> Self {
        let array_expr = Expr::translate_ref(arr, module);
        let elements_expr = Expr::Field {
            base: Box::new(array_expr),
            field: FieldName::StaticArrayElements,
        };

        let index_expr = Expr::translate_val(index, module);
        let element = Expr::Element {
            base: Box::new(elements_expr),
            index: Box::new(index_expr),
        };

        element.addr_of()
    }

    pub fn translate_field(
        a: &ir::Ref,
        of_ty: &ir::Type,
        field_id: ir::FieldID,
        module: &mut Unit,
    ) -> Self {
        let a_expr = Expr::translate_ref(a, module);

        match of_ty {
            ir::Type::RcPointer(class_id) => {
                // pointer to RC containing pointer to class resource
                match class_id {
                    // HACK: closures are of unknown type, but we don't need a real vtable to call
                    // them. every closure struct starts with a Rc struct followed by a pointer
                    // to the closure's function, so we can get the function pointer by applying
                    // the offset manually
                    ir::VirtualTypeID::Closure(func_ty_id) if field_id == ir::CLOSURE_PTR_FIELD => {
                        let size_of_rc = Expr::SizeOf(Type::Rc);
                        let offset_ptr = Expr::infix_op(a_expr, InfixOp::Add, size_of_rc);

                        let func_ptr_ty = Type::DefinedType(TypeDefName::Alias(*func_ty_id));
                        offset_ptr.cast(func_ptr_ty.ptr())
                    },

                    // normal class: it's just a field accessed through this pointer
                    ir::VirtualTypeID::Class(..) => {
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

    pub fn class_ptr(struct_id: ir::TypeDefID) -> Self {
        Expr::Class(struct_id).addr_of().cast(Type::Class.ptr())
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
            Expr::Element { base, index } => write!(f, "({})[{}]", base, index),
            Expr::Field { base, field } => write!(f, "({}).{}", base, field),
            Expr::Arrow { base, field } => write!(f, "({})->{}", base, field),
            Expr::Cast(value, ty) => write!(f, "(({}){})", ty.typename(), value),
            Expr::SizeOf(ty) => write!(f, "sizeof({})", ty.typename()),
            Expr::Class(struct_id) => write!(f, "{}", GlobalName::ClassInstance(*struct_id)),
        }
    }
}
