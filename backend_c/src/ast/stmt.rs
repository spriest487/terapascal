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
use std::iter;
use std::rc::Rc;
use terapascal_ir::MetadataSource;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum GlobalName {
    ClassType,
    DynArrayClassType,
    BoxClassType,
    
    ClassInstance(ir::TypeDefID),
    DynArrayClassInstance(DynArrayTypeID),
    BoxClassInstance(BoxTypeID),

    StringLiteral(ir::StringID),

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
        ir::Type::Generic(..) => {
            panic!("code output should never contain generic placeholders")
        },

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
        ir::Type::Struct(id) => {
            if !id.args.is_empty() {
                todo!("C backend type args")
            }
            format!("Struct_{}", id.def_id)
        },
        ir::Type::Variant(id) => {
            if !id.args.is_empty() {
                todo!("C backend type args")
            }

            format!("Variant_{}", id.def_id)
        },
        ir::Type::Flags(repr_id) => format!("Flags_{repr_id}"),

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
        ir::ObjectID::AnyClosure(id) => format!("VType_Closure_{id}"),
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
    Result,
    Arg(ir::ArgID),
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
            VariableID::Result => write!(f, "Result"),
            VariableID::Arg(id) => write!(f, "Arg{}", id.0),
            VariableID::Local(id) => write!(f, "L{}", id.0),
            VariableID::Temp(id) => write!(f, "V{id}"),
            VariableID::Named(name) => write!(f, "V_{name}"),
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
    pub fn var(ty: Type, id: VariableID, null_init: bool) -> Statement {
        Statement::VariableDecl { ty, id, null_init }
    }
    
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

pub struct Builder<'a, 'b> {
    module: &'a mut Unit<'b>,
    pub stmts: Vec<Statement>,

    variable_types: BTreeMap<ir::LocalID, ir::Type>,
    next_temp_var: usize,
}

impl<'a, 'b> Builder<'a, 'b> {
    pub fn new(module: &'a mut Unit<'b>) -> Self {
        Self {
            module,
            stmts: Vec::new(),

            variable_types: BTreeMap::new(),
            next_temp_var: 0,
        }
    }
    
    pub fn translate_type(&mut self, ir_ty: &ir::Type) -> Type {
        Type::from_metadata(ir_ty, self.module)
    }

    pub fn translate_instructions(&mut self, instruction_list: &ir::InstructionList) {
        let mut last_source_span = None;

        for (instruction, source_span) in instruction_list.instructions
            .iter()
            .zip(instruction_list.sources.iter()
                .map(|span| span.as_ref())
                .chain(iter::repeat(None)))
        {
            // print the source location each time it changes
            if source_span != last_source_span {
                if let Some(span) = source_span {
                    self.stmts.push(Statement::Comment(span.to_string()));
                }

                last_source_span = source_span;
            }

            self.translate_instruction(instruction);
        }
    }
    
    pub fn new_temp_var(&mut self, var_type: Type, null_init: bool) -> VariableID {
        let id = VariableID::Temp(self.next_temp_var);
        self.next_temp_var += 1;

        self.stmts.push(Statement::var(var_type, id.clone(), null_init));
        
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
                
                if self.variable_types.insert(*id, ty.clone()).is_some() {
                    panic!("redeclaration of variable {} ({})", *id, ty.to_pretty_string(self.module.metadata.as_formatter()));
                }
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
                let cond_expr = Expr::translate_val(test, self);
                
                self.stmts.push(Statement::if_then(cond_expr, [
                    Statement::Goto(*dest)
                ]));
            },
            ir::Instruction::Comment(text) => {
                let safe_text = text.replace("/*", "").replace("*/", "");
                self.stmts.push(Statement::Comment(safe_text));
            },

            ir::Instruction::AddrOf { out, a } | ir::Instruction::MakeRef { out, a } => {
                let addr = Expr::translate_ref(a, self).addr_of();
                self.assign_ref(out, addr);
            },

            ir::Instruction::Move { out, new_val } => {
                let val = Expr::translate_val(new_val, self);
                self.assign_ref(out, val);
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
                    operand: Box::new(Expr::translate_val(a, self)),
                };
                self.assign_ref(out, val);
            },

            ir::Instruction::Length { out, a, of_type: of_ty, .. } => {
                let element = Expr::translate_length(a, of_ty, self);
                self.assign_ref(out, element);
            },

            ir::Instruction::Call { out, function, args, type_args } => {
                if !type_args.is_empty() {
                    todo!("generic method translation")
                }

                self.translate_call(out.as_ref(), function, args);
            }

            ir::Instruction::NewObject { out, type_id, type_args, immortal } => {
                self.translate_new_object(out, *type_id, type_args, *immortal);
            }

            ir::Instruction::NewArray { out, element_type, count, immortal } => {
                self.translate_new_array(out, element_type, count, *immortal);
            }

            ir::Instruction::NewBox { out, value_type, immortal } => {
                self.translate_new_box(out, value_type, *immortal);
            }

            ir::Instruction::Gt(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gt, b, self);
                let assign_result = Expr::translate_assign(
                    out,
                    gt,
                    self,
                );
                self.stmts.push(Statement::Expr(assign_result))
            },

            ir::Instruction::Gte(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gte, b, self);
                let assign_result = Expr::translate_assign(
                    out,
                    gt,
                    self,
                );
                self.stmts.push(Statement::Expr(assign_result))
            },

            ir::Instruction::Lt(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Lt, b, self);
                let assign_result = Expr::translate_assign(
                    out,
                    gt,
                    self,
                );
                self.stmts.push(Statement::Expr(assign_result))
            },
            ir::Instruction::Lte(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Lte, b, self);
                let assign_result = Expr::translate_assign(
                    out,
                    gt,
                    self,
                );
                self.stmts.push(Statement::Expr(assign_result))
            },
            

            ir::Instruction::And(ir::BinOpInstruction { out, a, b }) => {
                let and = Expr::translate_infix_op(a, InfixOp::And, b, self);
                let assign_result = Expr::translate_assign(
                    out,
                    and,
                    self,
                );
                self.stmts.push(Statement::Expr(assign_result))
            },

            ir::Instruction::Or(ir::BinOpInstruction { out, a, b }) => {
                let or = Expr::translate_infix_op(a, InfixOp::Or, b, self);
                let assign_result = Expr::translate_assign(
                    out,
                    or,
                    self,
                );
                self.stmts.push(Statement::Expr(assign_result))
            },

            ir::Instruction::Not(ir::UnaryOpInstruction { out, a }) => {
                let a_expr = Expr::translate_val(a, self);
                let not = Expr::PrefixOp {
                    op: PrefixOp::Not,
                    operand: Box::new(a_expr),
                };
                let assign_result = Expr::translate_assign(
                    out,
                    not,
                    self,
                );
                self.stmts.push(Statement::Expr(assign_result))
            },

            ir::Instruction::Retain { at, value_type } => {
                // TODO deep release/retain
                let retain = Expr::Function(FunctionName::Builtin(BuiltinName::RcRetain));

                let rc_ptr = Expr::translate_ref(at, self).cast(Type::object_ptr());
                let call_retain = retain.call([rc_ptr, Expr::LitBool(value_type.is_weak())]);

                self.stmts.push(Statement::Expr(call_retain));
            },

            ir::Instruction::Release { at, value_type, released_out } => {
                // TODO deep release/retain
                let release = Expr::Function(FunctionName::Builtin(BuiltinName::RcRelease));

                let rc_ptr = Expr::translate_ref(at, self).cast(Type::object_ptr());
                let call_release = release.call([rc_ptr, Expr::LitBool(value_type.is_weak())]);
                
                if *released_out != ir::Ref::Discard {
                    let lhs = Expr::translate_ref(released_out, self);
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

                let mut args = vec![Expr::translate_val(self_arg, self)];
                args.extend(
                    rest_args
                        .iter()
                        .map(|arg| Expr::translate_val(arg, self)),
                );

                let call = method_func.call(args);

                let result_expr = match out {
                    Some(out) => Expr::translate_assign(out, call, self),
                    None => call,
                };

                self.stmts.push(Statement::Expr(result_expr));
            },

            ir::Instruction::IsType { out, a, value_type, is_type } => {
                self.translate_is_type(out, a, value_type, is_type);
            },

            ir::Instruction::Raise { val } => {
                let value_expr = Expr::translate_ref(val, self);
                self.raise(value_expr);
            },

            ir::Instruction::Cast { ty, out, a } => {
                let ty = Type::from_metadata(ty, self.module);
                let expr = Expr::translate_val(a, self);

                // TODO: just assume any valid pascal cast is a valid C cast for now...
                let cast_result = Expr::Cast(Box::new(expr), ty);

                self.assign_ref(out, cast_result);
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

    pub fn assign_ref(&mut self, out_ref: &ir::Ref, rhs: impl Into<Expr>) {
        let out_expr = Expr::translate_ref(out_ref, self);
        self.assign(out_expr, rhs.into())
    }

    fn write_infix_op(&mut self, out: &ir::Ref, lhs: &ir::Value, op: InfixOp, rhs: &ir::Value) {
        let and = Expr::translate_infix_op(lhs, op, rhs, self);
        let assign_result = Expr::translate_assign(out, and, self);

        self.stmts.push(Statement::Expr(assign_result))
    }

    fn translate_is_type(&mut self, out: &ir::Ref, a: &ir::Value, value_type: &ir::Type, is_type: &ir::Type) {
        let is_object_id = if value_type.is_object()
            && let ir::Type::Object(object_id) = is_type
        {
            object_id
        } else {
            let result = Expr::translate_assign(out, Expr::LitBool(value_type == is_type), self);
            self.stmts.push(Statement::Expr(result));
            return;
        };

        let actual_expr = Expr::translate_val(a, self);

        // get class ptr from rc
        let rc_ptr = actual_expr.cast(Type::Rc.ptr());
        let actual_class_ptr = rc_ptr.clone().arrow(FieldName::ObjectClass);
        
        let is_null = rc_ptr.clone().not();

        // zombie refs don't count as any type
        let is_zombie = rc_ptr
            .arrow(FieldName::RcStrongCount)
            .not();

        let is_dead = Expr::infix_op(is_null, InfixOp::Or, is_zombie);

        let stmt = Statement::if_then_else(is_dead, [
            Statement::Expr(Expr::translate_assign(out, Expr::LitBool(false), self)),
        ], [{
            let is = match is_object_id {
                ir::ObjectID::Class(id) => {
                    if !id.args.is_empty() {
                        todo!("C backend type args")
                    }
                    let is_class_ptr = Expr::class_ptr(id.def_id);
                    
                    Expr::infix_op(actual_class_ptr, InfixOp::Eq, is_class_ptr)
                }

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

                ir::ObjectID::AnyClosure(_func_ty_id) => {
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

            Statement::Expr(Expr::translate_assign(out, is, self))
        }]);

        self.stmts.push(stmt);
    }

    pub fn get_dyn_array_type(&mut self, element_type: &ir::Type) -> DynArrayTypeID {
        self.module.get_dyn_array_type(element_type)
    }

    pub fn array_class_ptr(&mut self, arr_obj: &Expr, id: &ir::ObjectID) -> Expr {
        Expr::array_class_ptr(arr_obj, id, self.module)
    }

    pub fn function_name(&self, id: ir::FunctionID) -> FunctionName {
        self.module.function_name(id)
    }

    pub fn get_box_type(&mut self, element_type: &ir::Type) -> BoxTypeID {
        self.module.get_box_type(element_type)
    }

    #[allow(unused)]
    fn find_ref_type(&self, at: &ir::Ref) -> Option<ir::Type> {
        match at {
            ir::Ref::Local(id) => {
                self.variable_types.get(id).cloned()
            }

            ir::Ref::Global(ir::GlobalRef::Variable(var_id)) => {
                let var = self.module.metadata.get_variable(*var_id)?;
                
                Some(var.r#type.clone())
            }
            
            ir::Ref::Global(ir::GlobalRef::StaticTagArray(..)) => {
                Some(ir::Type::any().dyn_array())
            }

            ir::Ref::Global(ir::GlobalRef::Function(func_id)) => {
                let func_ty = self.module.function_types.get(func_id)?;
                Some(ir::Type::Function(*func_ty))
            }

            ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(..)) => {
                Some(ir::Type::type_info())
            }

            ir::Ref::Global(ir::GlobalRef::StaticFuncInfo(..)) => {
                Some(ir::Type::func_info())
            }

            ir::Ref::Global(ir::GlobalRef::StringLiteral(..)) => {
                Some(ir::Type::string())
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
        let func_expr = Expr::translate_val(function, self);
        let args = args
            .iter()
            .map(|arg_val| Expr::translate_val(arg_val, self));

        let call = func_expr.call(args);

        let result_expr = match out {
            Some(out) => Expr::translate_assign(out, call, self),

            None => call,
        };

        self.stmts.push(Statement::Expr(result_expr));
    }

    fn translate_new_object(
        &mut self,
        out: &ir::Ref,
        struct_id: ir::TypeDefID,
        type_args: &[ir::Type],
        immortal: bool,
    ) {
        if !type_args.is_empty() {
            todo!("C backend type args")
        }

        let ty_class_ptr = Expr::class_ptr(struct_id);
        
        let new_function = Expr::Function(FunctionName::Builtin(BuiltinName::RcNew));

        let new_object = new_function.call([
            ty_class_ptr,
            Expr::LitBool(immortal),
        ]);

        let struct_type = Type::from_ir_struct(struct_id, type_args);

        self.assign_ref(out, new_object.cast(struct_type.ptr()));
    }

    fn translate_new_array(
        &mut self,
        out: &ir::Ref,
        element_type: &ir::Type,
        count: &ir::Value,
        immortal: bool,
    ) {
        let array_id = self.module.get_dyn_array_type(element_type);
        let count_val = Expr::translate_val(count, self);

        let array_class_ptr = Expr::dyn_array_class(array_id).addr_of();

        let new_function = Expr::Function(FunctionName::Builtin(BuiltinName::RcNewArray));

        let object_ptr = new_function.call([
            array_class_ptr,
            count_val,
            Expr::LitBool(immortal),
        ]);
        
        let array_ptr = object_ptr.cast(Type::dyn_array_ptr(array_id));

        self.assign_ref(out, array_ptr);
    }

    fn translate_new_box(
        &mut self,
        out_ref: &ir::Ref,
        element_type: &ir::Type,
        immortal: bool,
    ) {
        let out = Expr::translate_ref(out_ref, self);
        
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
