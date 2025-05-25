pub mod scope;

#[cfg(test)]
mod test;

use self::scope::*;
use crate::ast as ast;
use crate::ast::IdentPath;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::library_builder::FunctionDefKey;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::metadata::*;
use crate::codegen::FunctionInstance;
use crate::codegen::IROptions;
use crate::codegen::SetFlagsType;
use crate::typ as typ;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::Symbol;
use ast::Ident;
use common::span::Span;
use ir_lang::*;
use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub struct Builder<'m> {
    library: &'m mut LibraryBuilder,

    // positional list of type args that can be used to reify types in the current context
    // during this stage we only need to be able to substitute args, we don't need to validate
    // anything, so combining them into a single list and ignoring positions is OK
    generic_context: typ::GenericContext,

    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_label: Label,

    loop_stack: Vec<LoopScope>,
}

impl<'m> Builder<'m> {
    pub fn new(lib: &'m mut LibraryBuilder) -> Self {
        let module_span = lib.module_span().clone();

        let mut instructions = Vec::new();
        instructions.push(Instruction::LocalBegin);

        if lib.opts().debug {
            instructions.push(Instruction::DebugPush(module_span.clone()));
        }

        Self {
            library: lib,
            
            instructions,

            // the EXIT label is always reserved, so start one after that
            next_label: Label(EXIT_LABEL.0 + 1),
            scopes: vec![Scope::new()],

            loop_stack: Vec::new(),

            generic_context: typ::GenericContext::empty(),
        }
    }

    pub fn opts(&self) -> &IROptions {
        &self.library.opts()
    }
    
    pub fn with_generic_ctx(mut self, ctx: typ::GenericContext) -> Self {
        self.generic_context = ctx;
        self
    }
    
    pub fn generic_context(&self) -> &typ::GenericContext {
        &self.generic_context
    }

    pub fn translate_variant_case<'ty>(
        &'ty mut self,
        variant: &typ::Symbol,
        case: &Ident,
    ) -> (TypeDefID, usize, Option<&'ty Type>) {
        let name_path = self.translate_name(variant);

        let (id, variant_struct) = match self.library.metadata().find_variant_def(&name_path) {
            Some((id, variant_struct)) => (id, variant_struct),
            None => panic!("missing IR metadata definition for variant {}", variant),
        };

        let case_index = variant_struct
            .cases
            .iter()
            .position(|c| c.name == *case.name);

        match case_index {
            Some(index) => (id, index, variant_struct.cases[index].ty.as_ref()),
            None => panic!("missing case {} for {} in IR metadata", case, variant),
        }
    }

    pub fn translate_name(&mut self, name: &Symbol) -> NamePath {
        translate_name(name, &self.generic_context, self.library)
    }

    pub fn translate_iface(&mut self, iface_def: &typ::ast::InterfaceDecl) -> Interface {
        translate_iface(iface_def, &self.generic_context, self.library)
    }

    pub fn translate_type(&mut self, src_ty: &typ::Type) -> Type {
        self.library.translate_type(src_ty, &self.generic_context)
    }

    pub fn translate_dyn_array_struct(&mut self, element_ty: &typ::Type) -> TypeDefID {
        self.library.translate_dyn_array_struct(element_ty, &self.generic_context)
    }

    pub fn translate_method(
        &mut self,
        self_ty: typ::Type,
        self_ty_method_index: usize,
        mut call_ty_args: Option<typ::TypeArgList>
    ) -> FunctionInstance {
        if let Some(args_list) = &mut call_ty_args {
            *args_list = args_list
                .clone()
                .apply_type_args(&self.generic_context, &self.generic_context);
        }

        self.library.translate_method_impl(self_ty, self_ty_method_index, call_ty_args)
    }

    pub fn translate_func(
        &mut self,
        func_name: &Symbol,
        func_sig: &Rc<typ::FunctionSig>,
        mut call_ty_args: Option<typ::TypeArgList>,
    ) -> FunctionInstance {
        if let Some(args_list) = &mut call_ty_args {
            *args_list = args_list
                .clone()
                .apply_type_args(&self.generic_context, &self.generic_context);
        }

        let mut key = FunctionDefKey {
            type_args: call_ty_args,
            decl_key: FunctionDeclKey::Function { 
                name: func_name.full_path.clone(),
                sig: func_sig.clone(),
            },
        };

        self.library.instantiate_func(&mut key)
    }

    pub fn translate_func_ty(&mut self, func_sig: &typ::FunctionSig) -> TypeDefID {
        self.library
            .translate_func_ty(func_sig, &self.generic_context)
    }

    pub fn build_closure_expr(&mut self, func: &typ::ast::AnonymousFunctionDef) -> Ref {
        let closure = self
            .library
            .build_closure_instance(func, &self.generic_context);

        if func.captures.len() == 0 {
            let static_closure = self.library.build_static_closure_instance(closure);

            Ref::Global(GlobalRef::StaticClosure(static_closure.id))
        } else {
            self.build_closure_instance(closure)
        }
    }
    
    pub fn build_function_closure(&mut self, func: &FunctionInstance) -> Ref {
        let static_closure = self
            .library
            .build_func_static_closure_instance(func, &self.generic_context);

        Ref::Global(GlobalRef::StaticClosure(static_closure.id))
    }

    pub fn build_closure_instance(&mut self, closure: ClosureInstance) -> Ref {
        let closure_def = self
            .library
            .metadata()
            .get_struct_def(closure.closure_id)
            .cloned()
            .unwrap();

        let closure_ptr_ty = closure.closure_ptr_ty();

        // virtual pointer to the closure
        let closure_ref = self.local_new(closure_ptr_ty.clone(), None);

        self.scope(|builder| {
            builder.rc_new(closure_ref.clone(), closure.closure_id, false);

            // the closure pointer type (a virtual pointer to any closure of this function type)
            // and the closure structure pointer type are different - we need to use the closure
            // *structure* pointer type to set members of this specific closure instance!
            let closure_struct_ty = Type::Struct(closure.closure_id);
            let closure_struct_ptr_ty = closure_struct_ty.clone().ptr();

            // downcast virtual closure ptr to the concrete closure struct
            let closure_struct_ref = builder.local_temp(closure_struct_ptr_ty.clone());
            builder.cast(closure_struct_ref.clone(), closure_ref.clone(), closure_struct_ptr_ty.clone());

            let func_ptr_ty = closure_def.fields[&CLOSURE_PTR_FIELD].ty.clone();

            let func_field_ptr = builder.local_new(func_ptr_ty.clone().ptr(), None);
            builder.field(
                func_field_ptr.clone(),
                closure_struct_ref.clone().to_deref(),
                closure_struct_ty.clone(),
                CLOSURE_PTR_FIELD,
            );

            // initialize closure reference to function
            let func_ref = Ref::Global(GlobalRef::Function(closure.func_instance.id));
            // builder.cast(func_field_ptr.clone().to_deref(), func_ref, func_ptr_ty.clone());
            builder.mov(func_field_ptr.clone().to_deref(), func_ref);
            
            // initialize closure capture fields - copy from local scope into closure object
            for (field_id, field_def) in closure_def.fields.iter() {
                // skip the closure pointer field
                if *field_id == CLOSURE_PTR_FIELD {
                    continue;
                }

                // skip unnamed (padding) fields
                let field_name = match &field_def.name {
                    None => continue,
                    Some(name) => name,
                };

                let capture_field_ptr = builder.local_temp(field_def.ty.clone().ptr());
                builder.field(
                    capture_field_ptr.clone(),
                    closure_struct_ref.clone().to_deref(),
                    closure_struct_ty.clone(),
                    *field_id,
                );

                let capture_field = capture_field_ptr.to_deref();

                let captured_local_id = builder.find_local(field_name).unwrap().id();
                builder.mov(capture_field.clone(), captured_local_id);

                builder.retain(capture_field.clone(), &field_def.ty);
            }
        });

        closure_ref
    }
    
    pub fn get_method(&self, self_ty: &typ::Type, index: usize) -> typ::ast::MethodDecl {
        self.library.get_method(self_ty, index)
    }

    // for a given interface type and method index, get the method index from the self-type that
    // implements that method. panics on any error retrieving the methods, if the method index is
    // not valid for the interface type, or if the self-type is not an implementor of the interface
    pub fn get_impl_method_index(
        &self,
        self_ty: &typ::Type,
        iface_ty: &typ::Type,
        iface_method_index: usize,
    ) -> usize {
        if self_ty == iface_ty {
            return iface_method_index;
        }
        
        let iface_method = self.get_method(&iface_ty, iface_method_index);

        let method_name = iface_method.func_decl.ident();

        let impl_sig = iface_method.func_decl.sig().with_self(&self_ty);
        
        self.library.find_method_index(self_ty, method_name, &impl_sig)
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<str> {
        self.library.metadata().pretty_ty_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        self.library.metadata_mut().find_or_insert_string(s)
    }

    pub fn get_struct(&self, id: TypeDefID) -> Option<&Struct> {
        self.library.metadata().get_struct_def(id)
    }

    #[allow(unused)]
    pub fn get_iface(&self, id: InterfaceID) -> Option<&Interface> {
        self.library.metadata().get_iface_def(id)
    }
    
    pub fn find_type_seq_support(&self, src_ty: &typ::Type) -> Option<TypeSequenceSupport> {
        self.library.find_type_seq_support(src_ty)
    }
    
    fn remove_empty_blocks(&mut self) {
        let mut pc = 0;

        // stack of instruction indices at which the current empty scope begins
        let mut empty = vec![Some(pc)];

        while pc < self.instructions.len() {
            let block_empty = empty.last_mut().unwrap();

            match &self.instructions[pc] {
                Instruction::LocalBegin => {
                    empty.push(Some(pc));
                    pc += 1;
                }

                // end the scope
                Instruction::LocalEnd => {
                    if let Some(empty_start) = *block_empty {
                        // it's still empty, remove all the empty statements
                        let remove_count = (pc + 1) - empty_start;
                        pc = empty_start;
                        for _ in 0..remove_count {
                            self.instructions.remove(pc);
                        }

                        empty.pop();
                    } else {
                        empty.pop();
                        
                        // containing scope is no longer empty
                        *empty.last_mut().unwrap() = None;

                        pc += 1;
                    }
                }
                
                Instruction::DebugPop
                | Instruction::DebugPush(..)
                | Instruction::Comment(..) => {
                    pc += 1;
                }
                
                _ => {
                    *block_empty = None;
                    pc += 1;
                }
            }
        }
    }

    pub fn finish(mut self) -> Vec<Instruction> {
        if self.opts().debug {
            self.append(Instruction::DebugPop);
        }
        
        while !self.scopes.is_empty() {
            self.end_scope();
        }

        self.remove_empty_blocks();

        if matches!(self.instructions.as_slice(), [
            Instruction::DebugPush(..),
            Instruction::DebugPop,
        ]) {
            self.instructions.pop();
        }

        self.instructions
    }

    pub fn push_debug_context(&mut self, ctx: Span) {
        if !self.opts().debug {
            return;
        }

        self.current_scope_mut().inc_debug_ctx_count();

        self.append(Instruction::DebugPush(ctx));
    }

    pub fn pop_debug_context(&mut self) {
        if !self.opts().debug {
            return;
        }

        self.current_scope_mut().dec_debug_ctx_count();

        self.append(Instruction::DebugPop);
    }

    pub fn append(&mut self, instruction: Instruction) {
        if instruction.should_discard() {
            return;
        }

        self.instructions.push(instruction);
    }

    pub fn comment(&mut self, content: &(impl fmt::Display + ?Sized)) {
        if !self.opts().debug {
            return;
        }

        self.append(Instruction::Comment(content.to_string()));
    }

    pub fn mov(&mut self, out: impl Into<Ref>, val: impl Into<Value>) {
        self.append(Instruction::Move {
            out: out.into(),
            new_val: val.into(),
        });
    }
    
    pub fn rc_new(&mut self, out: impl Into<Ref>, type_id: TypeDefID, immortal: bool) {
        self.append(Instruction::RcNew { 
            out: out.into(),
            type_id,
            immortal,
        });
    }

    pub fn class_is(&mut self, out: impl Into<Ref>, a: impl Into<Value>, type_id: VirtualTypeID) {
        self.append(Instruction::ClassIs {
            out: out.into(),
            a: a.into(),
            class_id: type_id,
        })
    }
    
    pub fn add(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Add(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn sub(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Sub(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn sub_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>, as_type: &Type) -> Value {
        let a = a.into();
        let b = b.into();
        
        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            if let Some(result) = Value::from_literal_val(a_val - b_val, &as_type) {
                return result;
            }
        }

        let out = self.local_temp(as_type.clone());
        self.sub(out.clone(), a, b);       
        
        Value::Ref(out)
    }

    pub fn mul(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Mul(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn idiv(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::IDiv(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn fdiv(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::FDiv(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn modulo(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Mod(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn shl(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Shl(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn shr(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Shr(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    pub fn and(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::And(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    pub fn and_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        match (a.into(), b.into()) {
            (Value::LiteralBool(a), Value::LiteralBool(b)) => Value::LiteralBool(a && b),

            (a, b) => {
                let result = self.local_temp(Type::Bool);
                self.and(result.clone(), a, b);
                Value::Ref(result)
            },
        }
    }

    pub fn not_to_val(&mut self, bool_val: impl Into<Value>) -> Value {
        match bool_val.into() {
            Value::LiteralBool(b) => Value::LiteralBool(!b),

            other_val => {
                let result = self.local_temp(Type::Bool);
                self.not(result.clone(), other_val);
                Value::Ref(result)
            },
        }
    }

    pub fn not(&mut self, out: impl Into<Ref>, bool_val: impl Into<Value>) {
        self.append(Instruction::Not(UnaryOpInstruction {
            a: bool_val.into(),
            out: out.into(),
        }));
    }

    pub fn or(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Or(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    pub fn or_to_value(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_bool(), b.to_literal_bool()) {
            Value::LiteralBool(a_val || b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.or(result.clone(), a, b);
            Value::Ref(result)
        }
    }

    pub fn bit_and(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::BitAnd(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    pub fn bit_not(&mut self, out: impl Into<Ref>, a: impl Into<Value>) {
        self.append(Instruction::BitNot(UnaryOpInstruction {
            a: a.into(),
            out: out.into(),
        }));
    }

    pub fn bit_or(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::BitOr(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    pub fn bit_xor(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::BitXor(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    pub fn eq(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Eq(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }
    
    pub fn eq_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val == b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.eq(result.clone(), a, b);
            Value::Ref(result)
        }
    }

    pub fn gt(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Gt(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    pub fn gt_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val > b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.gt(result.clone(), a, b);
            Value::Ref(result)
        }
    }

    pub fn gte(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Gte(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    pub fn gte_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val >= b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.gte(result.clone(), a, b);
            Value::Ref(result)
        }
    }

    pub fn lt(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Lt(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    pub fn lte(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Lte(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    pub fn lt_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val < b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.lt(result.clone(), a, b);
            Value::Ref(result)
        }
    }

    pub fn size_of(&mut self, out: impl Into<Ref>, ty: Type) {
        self.mov(out, Value::SizeOf(ty));
    }

    pub fn field(
        &mut self,
        out: impl Into<Ref>,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
    ) {
        self.append(Instruction::Field {
            out: out.into(),
            a: base.into(),
            of_ty: base_ty.into(),
            field,
        })
    }

    pub fn field_val(
        &mut self,
        out: impl Into<Ref>,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type,
    ) {
        let field_ptr = self.local_temp(field_ty.ptr());
        self.field(field_ptr.clone(), base, base_ty, field);

        self.mov(out, field_ptr.to_deref());
    }
    
    pub fn field_to_val(
        &mut self,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type
    ) -> Ref {
        let result = self.local_temp(field_ty.clone());
        self.field_val(result.clone(), base, base_ty, field, field_ty);
        
        result
    }

    pub fn assign_field(
        &mut self,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type,
        val: impl Into<Value>,
    ) {
        let field_ptr = self.local_temp(field_ty.ptr());
        self.field(field_ptr.clone(), base, base_ty, field);

        self.mov(field_ptr.to_deref(), val);
    }
    
    pub fn element(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        element_ty: impl Into<Type>,
    ) {
        self.append(Instruction::Element {
            element: element_ty.into(),
            out: out.into(),
            a: a.into(),
            index: index.into(),
        });
    }

    pub fn element_val(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        element_ty: impl Into<Type>,
    ) {
        let element_ty = element_ty.into();
        let element_ptr = self.local_temp(element_ty.clone().ptr());

        self.element(element_ptr.clone(), a, index, element_ty);
        self.mov(out, element_ptr.to_deref());
    }
    
    #[allow(unused)]
    pub fn element_to_val(
        &mut self,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        element_ty: impl Into<Type>,
    ) -> Ref {
        let element_ty = element_ty.into();
        let result = self.local_temp(element_ty.clone());
        self.element_val(result.clone(), a, index, element_ty);

        result
    }
    
    pub fn set_include(&mut self, set_ref: impl Into<Ref>, bit_val: impl Into<Value>, set_type: &typ::SetType) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);
        
        let flags_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(flags_ptr.clone(), set_ref);

        self.call(flags_type_info.include_func, [
            Value::from(flags_ptr),
            bit_val.into(),
        ], None);
    }

    // todo: what is this for
    #[allow(unused)]
    pub fn set_exclude(&mut self, set_ref: impl Into<Ref>, bit_val: impl Into<Value>, set_type: &typ::SetType) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let flags_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(flags_ptr.clone(), set_ref);

        self.call(flags_type_info.exclude_func, [
            Value::from(flags_ptr),
            bit_val.into(),
        ], None);
    }

    pub fn set_contains(&mut self,
        out: impl Into<Ref>,
        set_ref: impl Into<Ref>,
        bit_val: impl Into<Value>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let flags_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(flags_ptr.clone(), set_ref);

        self.call(flags_type_info.contains_func, [
            Value::from(flags_ptr),
            bit_val.into(),
        ], Some(out.into()));
    }
    
    pub fn set_eq(&mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.clone().ptr());
        let b_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(a_ptr.clone(), a);
        self.addr_of(b_ptr.clone(), b);

        self.call(flags_type_info.eq_func, [
            Value::from(a_ptr),
            Value::from(b_ptr),
        ], Some(out.into()));
    }

    pub fn set_bit_not(&mut self,
        a: impl Into<Ref>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.clone().ptr());
        self.addr_of(a_ptr.clone(), a);

        self.call(flags_type_info.bit_not_func, [
            Value::from(a_ptr),
        ], None);
    }

    fn set_bitwise_op(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
        get_func_id: fn(&SetFlagsType) -> FunctionID,
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.clone().ptr());
        let b_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(a_ptr.clone(), a);
        self.addr_of(b_ptr.clone(), b);

        self.call(get_func_id(&flags_type_info), [
            Value::from(a_ptr),
            Value::from(b_ptr),
        ], None);
    }

    pub fn set_bit_and(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.bit_and_func)
    }

    pub fn set_bit_or(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.bit_or_func)
    }

    pub fn set_bit_xor(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.bit_xor_func)
    }

    pub fn label(&mut self, label: Label) {
        self.append(Instruction::Label(label))
    }

    pub fn jmp(&mut self, dest: Label) {
        self.append(Instruction::Jump { dest })
    }

    pub fn jmpif(&mut self, dest: Label, cond: impl Into<Value>) {
        self.append(Instruction::JumpIf {
            dest,
            test: cond.into(),
        })
    }

    pub fn call(
        &mut self,
        function: impl Into<Value>,
        args: impl IntoIterator<Item = Value>,
        out: Option<Ref>,
    ) {
        self.append(Instruction::Call {
            function: function.into(),
            args: args.into_iter().collect(),
            out,
        })
    }

    pub fn vcall(
        &mut self,
        iface_id: InterfaceID,
        method: MethodID,
        self_arg: impl Into<Value>,
        rest_args: impl IntoIterator<Item=impl Into<Value>>,
        out: Option<Ref>,
    ) {
        self.append(Instruction::VirtualCall {
            iface_id,
            method,
            self_arg: self_arg.into(),
            rest_args: rest_args.into_iter().map(|arg| arg.into()).collect(),
            out,
        })
    }

    pub fn get_mem(&mut self, count: impl Into<Value>, out: Ref) {
        let function_ref = Ref::Global(GlobalRef::Function(self.library.instantiate_get_mem_func()));
        self.call(function_ref, [count.into()], Some(out));
    }

    pub fn free_mem(&mut self, at: impl Into<Value>) {
        let function_ref = Ref::Global(GlobalRef::Function(self.library.instantiate_free_mem_func()));
        self.call(function_ref, [at.into()], None);
    }

    pub fn addr_of(&mut self, out: impl Into<Ref>, a: impl Into<Ref>) {
        self.append(Instruction::AddrOf {
            out: out.into(),
            a: a.into(),
        })
    }

    pub fn cast(&mut self, out: impl Into<Ref>, val: impl Into<Value>, ty: Type) {
        self.append(Instruction::Cast {
            out: out.into(),
            a: val.into(),
            ty,
        })
    }

    pub fn vartag(&mut self, out: impl Into<Ref>, a: impl Into<Ref>, of_ty: Type) {
        self.append(Instruction::VariantTag {
            out: out.into(),
            a: a.into(),
            of_ty,
        })
    }

    pub fn vardata(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        of_ty: Type,
        tag: usize,
    ) {
        self.append(Instruction::VariantData {
            out: out.into(),
            a: a.into(),
            of_ty,
            tag,
        })
    }

    pub fn bind_param(&mut self, id: LocalID, ty: Type, name: impl Into<String>, by_ref: bool) {
        self.current_scope_mut().bind_param(id, name, ty, by_ref);
    }

    // binds an anonymous return local in %0 with the indicated type
    pub fn bind_return(&mut self) {
        self.current_scope_mut().bind_return();
    }

    // binds an anonymous local binding for the closure pointer of a function
    pub fn bind_closure_ptr(&mut self) -> LocalID {
        let id = self.next_local_id();
        self.current_scope_mut().bind_temp(id);

        id
    }

    pub fn next_local_id(&self) -> LocalID {
        self.scopes
            .iter()
            .flat_map(|scope| scope.locals().iter())
            .map(Local::id)
            .max_by_key(|id| id.0)
            .map(|id| LocalID(id.0 + 1))
            .unwrap_or(LocalID(0))
    }

    pub fn alloc_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label = Label(self.next_label.0 + 1);
        label
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .iter_mut()
            .rev()
            .next()
            .expect("scope must be active")
    }

    pub fn local_temp(&mut self, ty: Type) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.next_local_id();
        self.instructions
            .push(Instruction::LocalAlloc(id, ty.clone()));

        self.current_scope_mut().bind_temp(id);

        Ref::Local(id)
    }

    // creates a managed local with type `ty_def`
    pub fn local_new(&mut self, ty: Type, name: Option<String>) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.next_local_id();
        self.instructions
            .push(Instruction::LocalAlloc(id, ty.clone()));

        self.current_scope_mut().bind_new(id, name, ty);

        Ref::Local(id)
    }

    pub fn find_local(&self, name: &str) -> Option<&Local> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.local_by_name(name))
    }
    
    pub fn find_global_var(&self, name_path: &IdentPath) -> Option<VariableID> {
        self.library.find_global_var(name_path)
    }

    pub fn local_closure_capture(&mut self, ty: Type, name: String) -> Ref {
        assert_ne!(Type::Nothing, ty);
        let id = self.next_local_id();

        self.instructions
            .push(Instruction::LocalAlloc(id, ty.clone()));

        self.current_scope_mut().bind_param(id, name, ty, true);

        Ref::Local(id)
    }

    /// call `f` for every structural member of the object of type `ty_def` found at the `at`
    /// reference.
    ///
    /// returns `true` if calling `f` for any of the members (or members of members) returns `true`
    pub fn visit_deep<Visitor>(&mut self, at: Ref, ty: &Type, f: Visitor) -> bool
    where
        Visitor: Fn(&mut Builder, &Type, Ref) -> bool + Copy,
    {
        match ty {
            Type::Struct(struct_id) => {
                let struct_def = self.library.metadata().get_struct_def(*struct_id).unwrap();

                let fields: Vec<_> = struct_def
                    .fields
                    .iter()
                    .map(|(field_id, field)| (*field_id, field.ty.clone()))
                    .collect();

                let mut result = false;
                for (field, field_ty) in fields {
                    if !(field_ty.is_rc() || field_ty.is_complex()) {
                        continue;
                    }

                    // store the field pointer in a temp slot
                    let field_val = self.local_temp(field_ty.clone().ptr());
                    self.append(Instruction::Field {
                        out: field_val.clone(),
                        a: at.clone(),
                        of_ty: Type::Struct(*struct_id),
                        field,
                    });

                    result |= self.visit_deep(field_val.to_deref(), &field_ty, f);
                }

                result
            },

            Type::Variant(id) => {
                let cases = &self
                    .library
                    .metadata()
                    .get_variant_def(*id)
                    .unwrap_or_else(|| panic!("missing variant def {}", id))
                    .cases
                    .to_vec();

                let tag_ptr = self.local_temp(Type::I32.ptr());
                let is_not_case = self.local_temp(Type::Bool);

                // get the tag
                self.vartag(tag_ptr.clone(), at.clone(), Type::Variant(*id));

                // jump out of the search loop if we find the matching case
                let break_label = self.alloc_label();

                let mut result = false;

                // for each case, check if the tag matches and jump past it if not

                for (tag, case) in cases.iter().enumerate() {
                    if self.opts().debug {
                        self.comment(&format!("testing for variant case {} ({})", tag, case.name));
                    }

                    if let Some(data_ty) = &case.ty {
                        if !(data_ty.is_rc() || data_ty.is_complex()) {
                            continue;
                        }

                        let skip_case_label = self.alloc_label();

                        // is_not_case := tag_ptr^ != tag
                        let tag_val = Value::LiteralI32(tag as i32);
                        self.eq(is_not_case.clone(), tag_ptr.clone().to_deref(), tag_val);
                        self.not(is_not_case.clone(), is_not_case.clone());

                        self.jmpif(skip_case_label, is_not_case.clone());

                        // get ptr into case data and visit it

                        // only one of these allocations will occur depending on which case is
                        // active, so a scope is needed here to stop the local counter being
                        // incremented once per case
                        self.scope(|builder| {
                            let data_ptr = builder.local_temp(data_ty.clone().ptr());
                            builder.vardata(
                                data_ptr.clone(),
                                at.clone(),
                                Type::Variant(*id),
                                tag,
                            );

                            result |= builder.visit_deep(data_ptr.to_deref(), &data_ty, f);
                        });

                        // break after any case executes
                        self.jmp(break_label);

                        // jump to here if this case isn't active
                        self.label(skip_case_label);
                    }
                }

                self.label(break_label);

                result
            },

            Type::Array { element, dim } => {
                if !element.is_rc() && !element.is_complex() {
                    return false;
                }

                let element_ptr = self.local_temp((**element).clone().ptr());
                let mut result = false;

                for i in 0..*dim {
                    self.append(Instruction::Element {
                        out: element_ptr.clone(),
                        a: at.clone(),
                        element: (**element).clone(),
                        index: Value::LiteralI32(i as i32), // todo: real usize type,
                    });

                    result |= self.visit_deep(element_ptr.clone().to_deref(), element, f);
                }

                result
            },

            // field or element
            _ => f(self, ty, at),
        }
    }

    pub fn retain(&mut self, at: Ref, ty: &Type) -> bool {
        if self.opts().annotate_rc {
            self.comment(&format!("retain: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } | Type::Struct(..) | Type::Variant(..) => {
                let rc_funcs = self.library.gen_runtime_type(ty);

                if let Some(retain) = rc_funcs.retain {
                    let at_ptr = self.local_temp(ty.clone().ptr());
                    self.append(Instruction::AddrOf {
                        out: at_ptr.clone(),
                        a: at,
                    });

                    self.append(Instruction::Call {
                        function: Value::Ref(Ref::Global(GlobalRef::Function(retain))),
                        args: vec![Value::Ref(at_ptr)],
                        out: None,
                    });
                }

                true
            },

            Type::RcPointer(..) => {
                self.append(Instruction::Retain { at, weak: false });
                true
            },

            Type::RcWeakPointer(..) => {
                self.append(Instruction::Retain { at, weak: true });
                true
            },

            _ => {
                // not an RC type, nor a complex type containing RC types
                false
            },
        }
    }
    
    fn call_release(&mut self, at: Ref, ty: &Type) {
        let rc_funcs = self.library.gen_runtime_type(ty);

        if let Some(release) = rc_funcs.release {
            let at_ptr = self.local_temp(ty.clone().ptr());
            self.addr_of(at_ptr.clone(), at);
            self.call(release, [Value::from(at_ptr)], None);
        }
    }

    pub fn release(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        let at = at.into();

        if self.opts().annotate_rc {
            self.comment(&format!("release: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } => {
                self.call_release(at, ty);
                true
            }
            
            Type::Struct(_id) | Type::Variant(_id) => {
                // if let Some(dtor) = self.library.metadata().find_dtor(*id) {
                //     self.call(dtor, [Value::from(at.clone())], None);
                // }
                
                self.call_release(at, ty);
                true
            },

            Type::RcPointer(..) => {
                self.append(Instruction::Release { at, weak: false });
                true
            },

            Type::RcWeakPointer(..) => {
                self.append(Instruction::Release { at, weak: true });
                true
            },

            _ => {
                // not an RC type, nor a complex type containing RC types
                false
            },
        }
    }

    pub fn begin_loop_body_scope(&mut self, continue_label: Label, break_label: Label) {
        self.loop_stack.push(LoopScope {
            continue_label,
            break_label,
            block_level: self.scopes.len(),
        });

        self.begin_scope();
    }

    pub fn end_loop_body_scope(&mut self) {
        self.end_scope();

        self.loop_stack
            .pop()
            .expect("end_loop called without an active loop");
    }

    pub fn loop_body_scope<F>(
        &mut self,
        continue_label: Label,
        break_label: Label,
        f: F,
    ) -> &[Instruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_instruction = self.instructions.len();

        self.begin_loop_body_scope(continue_label, break_label);

        f(self);

        self.end_loop_body_scope();

        &self.instructions[start_instruction..]
    }

    pub fn current_loop(&self) -> Option<&LoopScope> {
        self.loop_stack.last()
    }

    pub fn begin_scope(&mut self) {
        self.append(Instruction::LocalBegin);
        self.scopes.push(Scope::new());

        if self.opts().debug {
            self.comment(&format!("begin scope {}", self.scopes.len()));
        }
    }

    pub fn scope<F>(&mut self, f: F) -> &[Instruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_index = self.instructions.len();

        self.begin_scope();
        f(self);
        self.end_scope();

        &self.instructions[start_index..]
    }

    /// release locals in all scopes after the position indicated by
    /// `to_scope` in the scope stack
    /// this should be used when jumping out a scope, or before popping one
    fn cleanup_scope(&mut self, to_scope: usize) {
        assert!(
            self.scopes.len() > to_scope,
            "reset_scope index out of range: {}",
            to_scope
        );

        let last_scope = self.scopes.len() - 1;

        if self.opts().debug {
            if to_scope == last_scope {
                self.comment(&format!("cleanup scope {}", to_scope + 1));
            } else {
                self.comment(&format!(
                    "cleanup scopes {}..{}",
                    to_scope + 1,
                    self.scopes.len()
                ));
            }
        }

        let cleanup_range = to_scope..=last_scope;

        if self.opts().debug {
            let debug_pops: usize = self.scopes[cleanup_range.clone()]
                .iter()
                .map(|scope| scope.debug_ctx_count())
                .sum();

            for _ in 0..debug_pops {
                // don't call the helper func to do this, we don't want to modify the scope here
                self.append(Instruction::DebugPop);
            }
        }

        // locals from all scopes up to the target scope, in order of deepest->shallowest,
        // then in reverse allocation order
        let locals: Vec<_> = self.scopes[cleanup_range]
            .iter()
            .rev()
            .flat_map(|scope| scope.locals().iter().rev().cloned())
            .collect();

        // release local bindings that will be lost when the current scope is popped.
        // of course. releasing a ref should either insert a release instruction directly
        // (for an RC pointer) or insert a call to a structural release function (for
        // complex types containing RC pointers), so should never introduce new locals
        // in the scope being popped
        for local in locals {
            if self.opts().annotate_rc {
                self.comment(&format!("expire {}", local.id()));
            }

            match local {
                Local::Param { id, ty, by_ref, .. } => {
                    if !by_ref {
                        self.release(id, &ty);
                    }
                },

                Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                },

                Local::Temp { .. } => {
                    // no cleanup required
                },

                Local::Return { .. } => {
                    if self.opts().annotate_rc {
                        self.comment("expire return slot");
                    }
                },
            }
        }
    }

    pub fn end_scope(&mut self) {
        self.cleanup_scope(self.scopes.len() - 1);

        if self.opts().debug {
            self.comment(&format!("end scope {}", self.scopes.len()));
        }

        self.scopes.pop().unwrap();
        self.append(Instruction::LocalEnd);
    }

    pub fn break_loop(&mut self) {
        let (break_label, break_scope) = {
            let current_loop = self
                .current_loop()
                .expect("break stmt must appear in a loop");

            (current_loop.break_label, current_loop.block_level)
        };

        // write cleanup code for the broken scope and its children
        self.cleanup_scope(break_scope);

        // jump to the label (presumably somewhere outside the broken scope!)
        self.append(Instruction::Jump { dest: break_label });
    }

    pub fn continue_loop(&mut self) {
        let (continue_label, continue_scope) = {
            let current_loop = self
                .current_loop()
                .expect("continue stmt must appear in a loop");

            (current_loop.continue_label, current_loop.block_level)
        };

        self.cleanup_scope(continue_scope);
        self.append(Instruction::Jump {
            dest: continue_label,
        });
    }
    
    pub fn bounds_check(&mut self,
        element_ty: &Type,
        length: impl Into<Value>,
        index: impl Into<Value>
    ) {
        let bounds_check_func = self.library.gen_bounds_check(element_ty);
        let func_ref = Value::Ref(Ref::Global(GlobalRef::Function(bounds_check_func)));

        self.call(func_ref, [
            length.into(),
            index.into(),
        ], None);
    }

    pub fn exit_function(&mut self) {
        self.cleanup_scope(0);

        self.append(Instruction::Jump { dest: EXIT_LABEL })
    }
}

pub fn jmp_exists(instructions: &[Instruction], to_label: Label) -> bool {
    instructions.iter().any(|i| match i {
        Instruction::Jump { dest } | Instruction::JumpIf { dest, .. } => *dest == to_label,
        _ => false,
    })
}
