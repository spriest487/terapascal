use crate::ast::boxed::BoxTypeID;
use crate::ast::expr::Expr;
use crate::ast::expr::InfixOp;
use crate::ast::expr::PrefixOp;
use crate::ast::ty_def::FieldName;
use crate::ast::BuiltinName;
use crate::ast::DynArrayTypeID;
use crate::ast::FunctionName;
use crate::ast::Type;
use crate::ast::TypeDefName;
use crate::ast::Unit;
use crate::ir;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum GlobalName {
    ClassType,
    DynArrayClassType,
    BoxClassType,
    
    ClassInstance(ir::TypeDefID),
    DynArrayClassInstance(DynArrayTypeID),
    BoxClassInstance(BoxTypeID),

    StringLiteral(ir::StringID),

    StaticClosure(ir::StaticClosureID),
    
    TypeInfoList,
    TypeInfoCount,
    StaticTypeInfo(Rc<ir::Type>),
    
    FuncInfoList,
    FuncInfoCount,
    StaticFuncInfo(ir::FunctionID),

    // TODO
    // this error message should be moved directly into the C impl files once FunctionInfo provides
    // enough info for the common code to do the arg validation itself rather than doing it per
    // invoker func
    InvokeArgsError,
    
    StaticTagArray(ir::TagLocation),
    
    Variable(ir::VariableID),
}

impl fmt::Display for GlobalName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalName::ClassType => write!(f, "Class"),
            GlobalName::DynArrayClassType => write!(f, "DynArrayClass"),
            GlobalName::BoxClassType => write!(f, "BoxClass"),
            
            GlobalName::ClassInstance(id) => write!(f, "Class_{}", id.0),
            GlobalName::DynArrayClassInstance(id) => write!(f, "DynArrayClass_{}", id.0),
            GlobalName::BoxClassInstance(id) => write!(f, "BoxClass_{}", id.0),

            GlobalName::StringLiteral(id) => write!(f, "String_{}", id.0),
            GlobalName::StaticClosure(id) => write!(f, "StaticClosure_{}", id.0),
            GlobalName::StaticTagArray(loc) => {
                write!(f, "StaticTagArray_")?;
                match loc {
                    ir::TagLocation::TypeDef(id) => {
                        write!(f, "Type_{}", id.0)
                    },
                    ir::TagLocation::Interface(id) => {
                        write!(f, "Iface_{}", id.0)
                    },
                    ir::TagLocation::Method { type_id, method_index} => {
                        write!(f, "Method_{}_{}", type_id, method_index)
                    },
                    ir::TagLocation::InterfaceMethod { iface_id, method_index} => {
                        write!(f, "IfaceMethod_{}_{}", iface_id, method_index)
                    },
                    ir::TagLocation::Function(id) => {
                        write!(f, "Function_{}", id.0)
                    },
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
            
            GlobalName::StaticFuncInfo(id) => write!(f, "FuncInfo_{}", id.0),
            
            GlobalName::InvokeArgsError => {
                write!(f, "Error_InvokeArgs")
            }
        }
    }
}

pub fn write_global_typeinfo_decl_name(f: &mut fmt::Formatter, ty: &ir::Type) -> fmt::Result {
    write!(f, "TypeInfo_{}", global_typeinfo_decl_name_type(ty))
}

pub fn global_typeinfo_decl_name(metadata: &ir::Metadata, ty: &ir::Type) -> Option<String> {
    if metadata.get_type_info(ty).is_some() {
        Some(format!("TypeInfo_{}", global_typeinfo_decl_name_type(ty)))
    } else {
        None
    }
}

fn global_typeinfo_decl_name_type(ty: &ir::Type) -> String {
    match ty {
        // primitives
        ir::Type::Bool => String::from("Bool"),
        ir::Type::U8 => String::from("U8"),
        ir::Type::I8 => String::from("I8"),
        ir::Type::I16 => String::from("I16"),
        ir::Type::U16 => String::from("U16"),
        ir::Type::I32 => String::from("I32"),
        ir::Type::U32 => String::from("U32"),
        ir::Type::I64 => String::from("I64"),
        ir::Type::U64 => String::from("U64"),
        ir::Type::USize => String::from("USize"),
        ir::Type::ISize => String::from("ISize"),
        ir::Type::F32 => String::from("F32"),
        ir::Type::F64 => String::from("F64"),

        // aggregates
        ir::Type::Struct(id) => format!("Struct_{id}"),
        ir::Type::Variant(id) => format!("Variant_{id}"),
        ir::Type::Flags(_repr_id, set_id) => format!("Flags_{set_id}"),

        // reference types
        ir::Type::Object(id) => {
            vtype_typeinfo_name(id)
        },
        
        ir::Type::WeakObject(id) => {
            format!("{}_Weak", vtype_typeinfo_name(id))
        }

        ir::Type::Function(closure_id) => format!("FunctionType_{closure_id}"),

        // ???
        ir::Type::Nothing => String::from("Nothing"),

        // recursive types
        ir::Type::Pointer(ty) => {
            format!("Ptr_{}", global_typeinfo_decl_name_type(ty))
        },
        ir::Type::TempRef(ty) => {
            format!("Ref_{}", global_typeinfo_decl_name_type(ty))
        },
        ir::Type::Array { element, dim } => { 
            format!("Array{}_{}", dim, global_typeinfo_decl_name_type(element))
        },
    }
}

fn vtype_typeinfo_name(id: &ir::ObjectID) -> String {
    match id {
        ir::ObjectID::Any => String::from("VType_Any"),
        ir::ObjectID::Class(id) => format!("VType_Class_{id}"),
        ir::ObjectID::Interface(id) => format!("VType_Interface_{id}"),
        ir::ObjectID::Closure(id) => format!("VType_Closure_{id}"),
        ir::ObjectID::Array(element) => {
            format!("DynArray_{}", global_typeinfo_decl_name_type(&element))
        }
        ir::ObjectID::Box(element) => {
            format!("Box_{}", global_typeinfo_decl_name_type(&element))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariableID {
    Local(ir::LocalID),
    Named(Box<String>),
    Temp(usize),
}

impl VariableID {
    pub fn local(id: ir::LocalID) -> Self {
        Self::Local(id)
    }
    
    pub fn named(name: impl Into<String>) -> Self {
        Self::Named(Box::new(name.into()))
    }
    
    pub fn to_expr(&self) -> Expr {
        Expr::Variable(self.clone())
    }
}

impl fmt::Display for VariableID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableID::Local(id) => write!(f, "L{}", id.0),
            VariableID::Named(name) => write!(f, "V_{name}"),
            VariableID::Temp(id) => write!(f, "V{id}"),
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

#[derive(Default)]
struct LocalScope {
    variable_types: BTreeMap<ir::LocalID, ir::Type>,
    
    next_temp_var: usize,
}

pub struct Builder<'a, 'b> {
    module: &'a mut Unit<'b>,
    pub stmts: Vec<Statement>,
    
    local_stack: Vec<LocalScope>,
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(module: &'a mut Unit<'b>) -> Self {
        Self {
            module,
            stmts: Vec::new(),
            local_stack: vec![LocalScope::default()],
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
    
    pub fn new_temp_var(&mut self, var_type: Type, null_init: bool) -> VariableID {
        let current_scope = self.local_stack.last_mut().unwrap();
        let id = VariableID::Temp(current_scope.next_temp_var);
        current_scope.next_temp_var += 1;

        self.stmts.push(Statement::VariableDecl {
            ty: var_type,
            id: id.clone(),
            null_init,
        });
        
        id
    }

    fn translate_instruction(&mut self, instruction: &ir::Instruction) {
        if self.module.opts.trace_ir {
            self.stmts.push(Statement::Comment(instruction.to_string()));
        }

        match instruction {
            ir::Instruction::LocalAlloc(id, ty) => {
                let null_init = ty.is_object();
                let c_type = Type::from_metadata(ty, self.module);

                self.stmts.push(Statement::VariableDecl {
                    ty: c_type,
                    id: VariableID::local(*id),
                    null_init,
                });
                
                let current_scope = self.local_stack.last_mut().unwrap();
                current_scope.variable_types.insert(*id, ty.clone());
            },

            ir::Instruction::DebugPush(ctx) => self
                .stmts
                .push(Statement::Comment(format!("context: {}", ctx))),
            ir::Instruction::DebugPop => {
                // no-op
            },

            ir::Instruction::LocalBegin => {
                self.stmts.push(Statement::BeginBlock);
                self.local_stack.push(LocalScope::default());
            },
            ir::Instruction::LocalEnd => {
                self.local_stack.pop();
                self.stmts.push(Statement::EndBlock)
            },

            ir::Instruction::Label(label) => {
                self.stmts.push(Statement::Label(*label));
                // this might be at end of a block, which C doesn't allow,
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

            ir::Instruction::AddrOf { out, a } | ir::Instruction::MakeRef { out, a } => {
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

            ir::Instruction::Element { out, a, index, of_type: of_ty, .. } => {
                let element = Expr::translate_element(a, index, of_ty, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    element,
                    self.module,
                )));
            },

            ir::Instruction::Length { out, a, of_type: of_ty, .. } => {
                let element = Expr::translate_length(a, of_ty, self.module);
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

            ir::Instruction::NewObject { out, type_id, immortal } => {
                self.translate_new_object(out, *type_id, *immortal);
            }

            ir::Instruction::NewArray { out, element_type, count, immortal } => {
                self.translate_new_array(out, element_type, count, *immortal);
            }

            ir::Instruction::NewBox { out, value_type, immortal } => {
                self.translate_new_box(out, value_type, *immortal);
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
                let retain = Expr::Function(FunctionName::Builtin(BuiltinName::RcRetain));

                let rc_ptr = Expr::translate_ref(at, self.module).cast(Type::object_ptr());
                let call_retain = retain.call([rc_ptr, Expr::LitBool(*weak)]);

                self.stmts.push(Statement::Expr(call_retain));
            },

            ir::Instruction::Release { at, weak, released_out } => {
                let release = Expr::Function(FunctionName::Builtin(BuiltinName::RcRelease));

                let rc_ptr = Expr::translate_ref(at, self.module).cast(Type::object_ptr());
                let call_release = release.call([rc_ptr, Expr::LitBool(*weak)]);
                
                if *released_out != ir::Ref::Discard {
                    let lhs = Expr::translate_ref(released_out, self.module);
                    self.stmts.push(Statement::assign(lhs, call_release));
                } else {
                    self.stmts.push(Statement::Expr(call_release));
                }
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
                self.translate_class_is(out, a, class_id);
            },

            ir::Instruction::Raise { val } => {
                let value_expr = Expr::translate_ref(val, self.module);
                self.raise(value_expr);
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
    
    pub fn retain(&mut self, at: Expr, weak: bool) {
        let retain = Expr::Function(FunctionName::Builtin(BuiltinName::RcRetain));

        self.stmts.push(Statement::Expr(retain.call([
            at.cast(Type::Rc.ptr()),
            Expr::LitBool(weak),
        ])));
    }
    
    pub fn release(&mut self, at: Expr, weak: bool) {
        let release = Expr::Function(FunctionName::Builtin(BuiltinName::RcRelease));

        self.stmts.push(Statement::Expr(release.call([
            at.cast(Type::Rc.ptr()),
            Expr::LitBool(weak),
        ])));
    }
    
    pub fn raise(&mut self, message_str: Expr) {
        let raise_func = Expr::Function(FunctionName::Builtin(BuiltinName::Raise));
        self.stmts.push(Statement::Expr(raise_func.call([message_str])));
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

    fn translate_class_is(&mut self, out: &ir::Ref, a: &ir::Value, class_id: &ir::ObjectID) {
        let actual_expr = Expr::translate_val(a, self.module);

        // get class ptr from rc
        let rc_ptr = actual_expr.cast(Type::Rc.ptr());
        let actual_class_ptr = rc_ptr.clone().arrow(FieldName::ObjectClass);
        
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
                ir::ObjectID::Class(struct_id) => {
                    let is_class_ptr = Expr::class_ptr(*struct_id);
                    
                    Expr::infix_op(actual_class_ptr, InfixOp::Eq, is_class_ptr)
                },

                ir::ObjectID::Array(element) => {
                    let array_id = self.module.get_dyn_array_type(element);
                    let array_class_ptr = Expr::dyn_array_class_ptr(array_id)
                        .cast(Type::Class.ptr());
                    
                    Expr::infix_op(actual_class_ptr, InfixOp::Eq, array_class_ptr)
                }

                ir::ObjectID::Box(element) => {
                    let box_id = self.module.get_box_type(element);
                    let box_class_ptr = box_id.class_ptr().cast(Type::Class.ptr());

                    Expr::infix_op(actual_class_ptr, InfixOp::Eq, box_class_ptr)
                }

                ir::ObjectID::Any => {
                    // `is Any` is true for anything!
                    Expr::LitBool(true)
                },

                ir::ObjectID::Closure(_func_ty_id) => {
                    // TODO - can you use `is` on a function type?
                    Expr::LitBool(false)
                },

                ir::ObjectID::Interface(iface_id) => {
                    let is_impl_func = Expr::Function(FunctionName::Builtin(BuiltinName::IsImpl));

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
    
    #[allow(unused)]
    fn find_ref_type(&self, at: &ir::Ref) -> Option<ir::Type> {
        match at {
            ir::Ref::Local(id) => {
                let mut local_type = None;
                for scope in self.local_stack.iter().rev() {
                    if let Some(ty) = scope.variable_types.get(id) {
                        local_type = Some(ty.clone());
                        break;
                    }
                }
                
                local_type
            }

            ir::Ref::Global(ir::GlobalRef::Variable(var_id)) => {
                let var_type = self.module.metadata.get_variable_type(*var_id)?;
                
                Some(var_type.clone())
            }

            ir::Ref::Global(ir::GlobalRef::StaticClosure(static_closure_id)) => {
                self.module.static_closures.iter()
                    .find_map(|closure| {
                        (closure.id == *static_closure_id)
                            .then(|| closure.func_ty_id)
                            .map(ir::Type::Function)
                    })
            }
            
            ir::Ref::Global(ir::GlobalRef::StaticTagArray(..)) => {
                Some(ir::Type::any().dyn_array())
            }

            ir::Ref::Global(ir::GlobalRef::Function(func_id)) => {
                let func_ty = self.module.function_types.get(func_id)?;
                Some(ir::Type::Function(*func_ty))
            }

            ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(..)) => {
                Some(ir::TYPEINFO_TYPE)
            }

            ir::Ref::Global(ir::GlobalRef::StaticFuncInfo(..)) => {
                Some(ir::FUNCINFO_TYPE)
            }

            ir::Ref::Global(ir::GlobalRef::StringLiteral(..)) => {
                Some(ir::STRING_TYPE)
            }

            ir::Ref::Deref(deref_val) => {
                if let ir::Value::Ref(deref) = deref_val.as_ref() {
                    self.find_ref_type(deref)
                } else {
                    None
                }
            }

            _ => None,
        }
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

    fn translate_new_object(&mut self, out: &ir::Ref, struct_id: ir::TypeDefID, immortal: bool) {
        let ty_class_ptr = Expr::class_ptr(struct_id);
        
        let new_function = Expr::Function(FunctionName::Builtin(BuiltinName::RcNew));

        let new_object = new_function.call([
            ty_class_ptr,
            Expr::LitBool(immortal),
        ]);

        self.stmts.push(Statement::Expr(Expr::translate_assign(
            out,
            new_object.cast(Type::from_ir_struct(struct_id).ptr()),
            self.module,
        )))
    }

    fn translate_new_array(
        &mut self,
        out: &ir::Ref,
        element_type: &ir::Type,
        count: &ir::Value,
        immortal: bool,
    ) {
        let array_id = self.module.get_dyn_array_type(element_type);
        let count_val = Expr::translate_val(count, self.module);

        let array_class_ptr = Expr::dyn_array_class(array_id).addr_of();

        let new_function = Expr::Function(FunctionName::Builtin(BuiltinName::RcNewArray));

        let object_ptr = new_function.call([
            array_class_ptr,
            count_val,
            Expr::LitBool(immortal),
        ]);
        
        let array_ptr = object_ptr.cast(Type::dyn_array_ptr(array_id));

        self.stmts.push(Statement::Expr(Expr::translate_assign(
            out,
            array_ptr,
            self.module,
        )))
    }

    fn translate_new_box(
        &mut self,
        out_ref: &ir::Ref,
        element_type: &ir::Type,
        immortal: bool,
    ) {
        let out = Expr::translate_ref(out_ref, self.module);
        
        self.new_box(out, element_type, immortal);
    }
    
    pub fn new_box(&mut self, out: Expr, value_type: &ir::Type, immortal: bool) {
        let box_type_id = self.module.get_box_type(value_type);

        let box_type = Type::DefinedType(TypeDefName::Box(box_type_id));
        let box_ptr_type = box_type.clone().ptr();

        let new_function = Expr::Function(FunctionName::Builtin(BuiltinName::RcNew));

        let box_class = box_type_id
            .class_ptr()
            .cast(Type::Class.ptr());

        self.stmts.push(Statement::assign(
            out,
            new_function
                .call([box_class, Expr::LitBool(immortal)])
                .cast(box_ptr_type)
        ));
    }
    
    pub fn box_value(&mut self, value: Expr, value_type: &ir::Type) -> Expr {
        let box_ptr_type = self.module.get_box_type(value_type).ptr_type();
        let box_ptr = Expr::Variable(self.new_temp_var(box_ptr_type, false));
        
        let box_value_field = box_ptr.clone().arrow(FieldName::BoxValue);

        self.new_box(box_ptr.clone(), value_type, false);
        self.assign(box_value_field.clone(), value);

        if value_type.is_object() {
            // TODO: deep retain
            self.retain(box_value_field.cast(Type::Rc.ptr()), false);
        }

        box_ptr
    }
    
    pub fn unbox_value(&mut self, box_ptr: Expr, value_type: &ir::Type) -> Expr {
        let arg_box_id = self.module.get_box_type(value_type);
        let arg_box = box_ptr.cast(arg_box_id.ptr_type());

        arg_box.arrow(FieldName::BoxValue)
    }
}
