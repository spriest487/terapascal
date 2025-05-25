use crate::ast::expr::Expr;
use crate::ast::expr::InfixOp;
use crate::ast::expr::PrefixOp;
use crate::ast::ty_def::FieldName;
use crate::ast::FunctionName;
use crate::ast::Unit;
use crate::ast::Type;
use ir_lang as ir;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum GlobalName {
    ClassInstance(ir::TypeDefID),

    StringLiteral(ir::StringID),

    StaticClosure(ir::StaticClosureID),
    
    TypeInfoList,
    TypeInfoCount,
    StaticTypeInfo(Rc<ir::Type>),

    FuncInfoList,
    FuncInfoCount,
    
    StaticTagArray(ir::TagLocation),
    
    Variable(ir::VariableID),
}

impl fmt::Display for GlobalName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalName::ClassInstance(id) => write!(f, "Class_{}", id.0),
            GlobalName::StringLiteral(id) => write!(f, "String_{}", id.0),
            GlobalName::StaticClosure(id) => write!(f, "StaticClosure_{}", id.0),
            GlobalName::StaticTagArray(loc) => {
                write!(f, "StaticTagArray_")?;
                match loc {
                    ir::TagLocation::TypeDef(id) => write!(f, "Type_{}", id.0),
                    ir::TagLocation::Interface(id) => write!(f, "Iface_{}", id.0),
                    ir::TagLocation::Method { type_id, method_index} => write!(f, "Method_{}_{}", type_id, method_index),
                    ir::TagLocation::InterfaceMethod { iface_id, method_index} => write!(f, "IfaceMethod_{}_{}", iface_id, method_index),
                    ir::TagLocation::Function(id) => write!(f, "Function_{}", id.0),
                }
            }
            GlobalName::Variable(id) => write!(f, "Variable_{}", id.0),

            GlobalName::TypeInfoList => write!(f, "typeinfo_list"),
            GlobalName::TypeInfoCount => write!(f, "typeinfo_count"),

            GlobalName::FuncInfoList => write!(f, "funcinfo_list"),
            GlobalName::FuncInfoCount => write!(f, "funcinfo_count"),

            GlobalName::StaticTypeInfo(ty) => {
                write_global_typeinfo_decl_name(f, ty)
            }
        }
    }
}

pub fn write_global_typeinfo_decl_name(f: &mut fmt::Formatter, ty: &ir::Type) -> fmt::Result {
    write!(f, "TypeInfo_")?;
    write_global_typeinfo_decl_name_type(f, ty)
}

fn write_global_typeinfo_decl_name_type(f: &mut fmt::Formatter, ty: &ir::Type) -> fmt::Result {
    match ty {
        // primitives
        ir::Type::Bool => write!(f, "Bool"),
        ir::Type::U8 => write!(f, "U8"),
        ir::Type::I8 => write!(f, "I8"),
        ir::Type::I16 => write!(f, "I16"),
        ir::Type::U16 => write!(f, "U16"),
        ir::Type::I32 => write!(f, "I32"),
        ir::Type::U32 => write!(f, "U32"),
        ir::Type::I64 => write!(f, "I64"),
        ir::Type::U64 => write!(f, "U64"),
        ir::Type::USize => write!(f, "USize"),
        ir::Type::ISize => write!(f, "ISize"),
        ir::Type::F32 => write!(f, "F32"),

        // aggregates
        ir::Type::Struct(id) => write!(f, "Struct_{id}"),
        ir::Type::Variant(id) => write!(f, "Variant_{id}"),
        ir::Type::Flags(_repr_id, set_id) => write!(f, "Flags_{set_id}"),

        // reference types
        ir::Type::RcPointer(id) | ir::Type::RcWeakPointer(id) => {
            write!(f, "VType_")?;
            match id {
                ir::VirtualTypeID::Any => write!(f, "Any"),
                ir::VirtualTypeID::Class(id) => write!(f, "Class_{id}"),
                ir::VirtualTypeID::Interface(id) => write!(f, "Interface_{id}"),
                ir::VirtualTypeID::Closure(id) => write!(f, "Closure_{id}"),
            }
        },

        ir::Type::Function(closure_id) => write!(f, "Closure_{closure_id}"),

        // ???
        ir::Type::Nothing => write!(f, "Nothing"),
        
        // recursive types
        ir::Type::Pointer(ty) => {
            write!(f, "Ptr_")?;
            write_global_typeinfo_decl_name_type(f, ty)
        },
        ir::Type::Array { element, dim } => {
            write!(f, "Array{}_", dim)?;
            write_global_typeinfo_decl_name_type(f, element)
        },
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum VariableID {
    Local(ir::LocalID),
    Named(Box<String>),
}

impl fmt::Display for VariableID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L")?;
        match self {
            VariableID::Local(id) => write!(f, "{}", id.0),
            VariableID::Named(name) => write!(f, "_{}", name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    VariableDecl {
        ty: Type,
        id: VariableID,
        null_init: bool,
    },
    Expr(Expr),
    BeginBlock,
    EndBlock,
    Label(ir::Label),
    Goto(ir::Label),
    Comment(String),
    IfCond {
        cond: Expr,
        then_branch: Vec<Statement>,
        else_branch: Vec<Statement>,
    },
    Return,
    ReturnValue(Expr),
}

impl Statement {
    pub fn if_then(
        cond: impl Into<Expr>,
        then_branch: impl IntoIterator<Item=Statement>
    ) -> Statement {
        Statement::IfCond {
            cond: cond.into(),
            then_branch: then_branch.into_iter().collect(),
            else_branch: vec![],
        }
    }

    pub fn if_then_else(
        cond: impl Into<Expr>,
        then_branch: impl IntoIterator<Item=Statement>,
        else_branch: impl IntoIterator<Item=Statement>
    ) -> Statement {
        Statement::IfCond {
            cond: cond.into(),
            then_branch: then_branch.into_iter().collect(),
            else_branch: else_branch.into_iter().collect(),
        }
    }
    
    pub fn assign(lhs: impl Into<Expr>, rhs: impl Into<Expr>) -> Self {
        Statement::Expr(Expr::assign(lhs.into(), rhs.into()))
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::VariableDecl { ty, id, null_init } => {
                let name = format!("{}", id);
                write!(f, "{}", ty.to_decl_string(&name))?;

                if *null_init {
                    write!(f, " = NULL")?;
                }

                write!(f, ";")
            },

            Statement::Expr(expr) => write!(f, "{};", expr),

            Statement::BeginBlock => write!(f, "{{"),
            Statement::EndBlock => write!(f, "}}"),

            Statement::Label(label) => write!(f, "J{}:", label.0),
            Statement::Goto(label) => write!(f, "goto J{};", label.0),

            Statement::Comment(text) => write!(f, "/* {} */", text),

            Statement::IfCond { cond, then_branch, else_branch } => {
                writeln!(f, "if ({}) {{", cond)?;
                for then_stmt in then_branch {
                    writeln!(f, "  {}", then_stmt)?;
                }
                write!(f, "}}")?;

                if !else_branch.is_empty() {
                    writeln!(f, " else {{")?;
                    for else_stmt in else_branch {
                        writeln!(f, "  {}", else_stmt)?;
                    }
                    write!(f, "}}")?;
                }
                
                writeln!(f)
            }

            Statement::Return => write!(f, "return;"),
            Statement::ReturnValue(expr) => write!(f, "return {};", expr),
        }
    }
}

pub struct Builder<'a> {
    module: &'a mut Unit,
    pub stmts: Vec<Statement>,
}

impl<'a> Builder<'a> {
    pub fn new(module: &'a mut Unit) -> Self {
        Self {
            module,
            stmts: Vec::new(),
        }
    }
    
    pub fn translate_type(&mut self, ir_ty: &ir::Type) -> Type {
        Type::from_metadata(ir_ty, self.module)
    }

    pub fn translate_instructions(&mut self, instructions: &[ir::Instruction]) {
        for instruction in instructions {
            self.translate_instruction(instruction);
        }
    }

    fn translate_instruction(&mut self, instruction: &ir::Instruction) {
        if self.module.opts.trace_ir {
            self.stmts.push(Statement::Comment(instruction.to_string()));
        }

        match instruction {
            ir::Instruction::LocalAlloc(id, ty) => {
                let null_init = ty.is_rc();
                let ty = Type::from_metadata(ty, self.module);
                self.stmts.push(Statement::VariableDecl {
                    ty,
                    id: VariableID::Local(*id),
                    null_init,
                });
            },

            ir::Instruction::DebugPush(ctx) => self
                .stmts
                .push(Statement::Comment(format!("context: {}", ctx))),
            ir::Instruction::DebugPop => {
                // no-op
            },

            ir::Instruction::LocalBegin => self.stmts.push(Statement::BeginBlock),
            ir::Instruction::LocalEnd => self.stmts.push(Statement::EndBlock),

            ir::Instruction::Label(label) => {
                self.stmts.push(Statement::Label(*label));
                // this might be at end end of a block, which C doesn't allow,
                // so insert an empty block too so there's something to label
                self.stmts.push(Statement::BeginBlock);
                self.stmts.push(Statement::EndBlock);
            },

            ir::Instruction::Jump { dest } => self.stmts.push(Statement::Goto(*dest)),
            ir::Instruction::JumpIf { dest, test } => {
                let cond_expr = Expr::translate_val(test, self.module);
                
                self.stmts.push(Statement::if_then(cond_expr, [
                    Statement::Goto(*dest)
                ]));
            },
            ir::Instruction::Comment(text) => {
                let safe_text = text.replace("/*", "").replace("*/", "");
                self.stmts.push(Statement::Comment(safe_text));
            },

            ir::Instruction::AddrOf { out, a } => {
                let addr = Expr::translate_ref(a, self.module).addr_of();
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    addr,
                    self.module,
                )));
            },

            ir::Instruction::Move { out, new_val } => {
                let val = Expr::translate_val(new_val, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    val,
                    self.module,
                )));
            },

            ir::Instruction::Eq(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Eq, b);
            },

            ir::Instruction::Add(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Add, b);
            },

            ir::Instruction::Sub(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Sub, b);
            },

            ir::Instruction::Mul(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Mul, b);
            },

            ir::Instruction::Mod(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Rem, b);
            },

            ir::Instruction::IDiv(ir::BinOpInstruction { out, a, b }) => {
                // TODO: make sure integer divisions with 2 floats returns an int
                self.write_infix_op(out, a, InfixOp::Div, b);
            },

            ir::Instruction::FDiv(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Div, b);
            },

            ir::Instruction::Shl(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Shl, b);
            },

            ir::Instruction::Shr(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Shr, b);
            },

            ir::Instruction::BitAnd(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::BitAnd, b);
            },

            ir::Instruction::BitOr(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::BitOr, b);
            },

            ir::Instruction::BitXor(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::BitXor, b);
            },

            ir::Instruction::BitNot(ir::UnaryOpInstruction { out, a }) => {
                let val = Expr::PrefixOp {
                    op: PrefixOp::BitNot,
                    operand: Box::new(Expr::translate_val(a, self.module)),
                };
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    val,
                    self.module,
                )))
            },

            ir::Instruction::Element { out, a, index, .. } => {
                let element = Expr::translate_element(a, index, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    element,
                    self.module,
                )));
            },

            ir::Instruction::Field {
                out,
                a,
                of_ty,
                field,
            } => {
                let field = Expr::translate_field(a, of_ty, *field, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    field,
                    self.module,
                )))
            },

            ir::Instruction::VariantTag { out, a, .. } => {
                let tag_field = Expr::Field {
                    base: Box::new(Expr::translate_ref(a, self.module)),
                    field: FieldName::VariantTag,
                };

                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    tag_field.addr_of(),
                    self.module,
                )));
            },

            ir::Instruction::VariantData { out, a, tag, .. } => {
                let data_field = Expr::Field {
                    base: Box::new(Expr::translate_ref(a, self.module)),
                    field: FieldName::VariantData,
                };

                let case_field = Expr::Field {
                    base: Box::new(data_field),
                    field: FieldName::VariantDataCase(*tag),
                };

                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    case_field.addr_of(),
                    self.module,
                )));
            },

            ir::Instruction::Call {
                out,
                function,
                args,
            } => {
                self.translate_call(out.as_ref(), function, args);
            },

            ir::Instruction::RcNew { out, type_id, immortal } => {
                self.translate_rc_new(out, *type_id, *immortal);
            }

            ir::Instruction::Gt(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gt, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },

            ir::Instruction::Gte(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gte, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },

            ir::Instruction::Lt(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Lt, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },
            ir::Instruction::Lte(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Lte, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },
            

            ir::Instruction::And(ir::BinOpInstruction { out, a, b }) => {
                let and = Expr::translate_infix_op(a, InfixOp::And, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    and,
                    self.module,
                )))
            },

            ir::Instruction::Or(ir::BinOpInstruction { out, a, b }) => {
                let or = Expr::translate_infix_op(a, InfixOp::Or, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    or,
                    self.module,
                )))
            },

            ir::Instruction::Not(ir::UnaryOpInstruction { out, a }) => {
                let a_expr = Expr::translate_val(a, self.module);
                let not = Expr::PrefixOp {
                    op: PrefixOp::Not,
                    operand: Box::new(a_expr),
                };
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    not,
                    self.module,
                )))
            },

            ir::Instruction::Retain { at, weak } => {                
                let retain = Expr::Function(FunctionName::RcRetain);

                let rc_ptr = Expr::translate_ref(at, self.module);
                let call_retain = retain.call([rc_ptr, Expr::LitBool(*weak)]);

                self.stmts.push(Statement::Expr(call_retain));
            },

            ir::Instruction::Release { at, weak } => {
                let release = Expr::Function(FunctionName::RcRelease);

                let rc_ptr = Expr::translate_ref(at, self.module);
                let call_release = release.call([rc_ptr, Expr::LitBool(*weak)]);

                self.stmts.push(Statement::Expr(call_release));
            },

            ir::Instruction::VirtualCall {
                out,
                iface_id,
                method,
                self_arg,
                rest_args,
            } => {
                let method_func = Expr::Function(FunctionName::Method(*iface_id, *method));

                let mut args = vec![Expr::translate_val(self_arg, self.module)];
                args.extend(
                    rest_args
                        .iter()
                        .map(|arg| Expr::translate_val(arg, self.module)),
                );

                let call = method_func.call(args);

                self.stmts.push(Statement::Expr(match out {
                    Some(out) => Expr::translate_assign(out, call, self.module),
                    None => call,
                }));
            },

            ir::Instruction::ClassIs { out, a, class_id } => {
                self.translate_class_is(out, a, *class_id);
            },

            ir::Instruction::Raise { val } => {
                let raise_func = Expr::Function(FunctionName::Raise);
                let val_expr = Expr::translate_ref(val, self.module);

                self.stmts.push(Statement::Expr(raise_func.call([val_expr])));
            },

            ir::Instruction::Cast { ty, out, a } => {
                let ty = Type::from_metadata(ty, self.module);
                let expr = Expr::translate_val(a, self.module);

                // TODO: just assume any valid pascal cast is a valid C cast for now...
                let cast_result = Expr::Cast(Box::new(expr), ty);

                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    cast_result,
                    self.module,
                )));
            },
        }
    }
    
    pub fn assign(&mut self, lhs: impl Into<Expr>, rhs: impl Into<Expr>) {
        self.stmts.push(Statement::Expr(Expr::assign(lhs.into(), rhs.into())));
    }

    fn write_infix_op(&mut self, out: &ir::Ref, lhs: &ir::Value, op: InfixOp, rhs: &ir::Value) {
        let and = Expr::translate_infix_op(lhs, op, rhs, self.module);
        self.stmts.push(Statement::Expr(Expr::translate_assign(
            out,
            and,
            self.module,
        )))
    }

    fn translate_class_is(&mut self, out: &ir::Ref, a: &ir::Value, class_id: ir::VirtualTypeID) {
        let actual_expr = Expr::translate_val(a, self.module);

        // get class ptr from rc
        let rc_ptr = actual_expr.cast(Type::Rc.ptr());
        let actual_class_ptr = rc_ptr.clone().arrow(FieldName::RcClass);
        
        let is_null = rc_ptr.clone().not();

        // zombie refs don't count as any type
        let is_zombie = rc_ptr
            .arrow(FieldName::RcStrongCount)
            .not();

        let is_dead = Expr::infix_op(is_null, InfixOp::Or, is_zombie);

        self.stmts.push(Statement::if_then_else(is_dead, [
            Statement::Expr(Expr::translate_assign(out, Expr::LitBool(false), self.module)),
        ], [{
            let is = match class_id {
                ir::VirtualTypeID::Class(struct_id) => {
                    let is_class_ptr = Expr::class_ptr(struct_id);
                    Expr::infix_op(actual_class_ptr, InfixOp::Eq, is_class_ptr)
                },

                ir::VirtualTypeID::Any => {
                    // `is Any` is true for anything!
                    Expr::LitBool(true)
                },

                ir::VirtualTypeID::Closure(_func_ty_id) => {
                    // TODO - can you use `is` on a function type?
                    Expr::LitBool(false)
                },

                ir::VirtualTypeID::Interface(iface_id) => {
                    let is_impl_func = Expr::Function(FunctionName::IsImpl);

                    Expr::call(
                        is_impl_func,
                        vec![actual_class_ptr, Expr::LitInt(iface_id.0 as i128)],
                    )
                },
            };

            Statement::Expr(Expr::translate_assign(
                out,
                is,
                self.module,
            ))
        }]));
    }

    fn translate_call(&mut self, out: Option<&ir::Ref>, function: &ir::Value, args: &[ir::Value]) {
        let func_expr = Expr::translate_val(function, self.module);
        let args = args
            .iter()
            .map(|arg_val| Expr::translate_val(arg_val, self.module));

        let call = func_expr.call(args);

        self.stmts.push(Statement::Expr(match out {
            Some(out) => Expr::translate_assign(out, call, self.module),

            None => call,
        }));
    }

    fn translate_rc_new(&mut self, out: &ir::Ref, struct_id: ir::TypeDefID, immortal: bool) {
        let ty_class_ptr = Expr::class_ptr(struct_id);

        let new_rc = Expr::Call {
            func: Box::new(Expr::Function(FunctionName::RcAlloc)),
            args: vec![
                ty_class_ptr,
                Expr::LitBool(immortal),
            ],
        };

        self.stmts.push(Statement::Expr(Expr::translate_assign(
            out,
            new_rc,
            self.module,
        )))
    }
}
