use crate::instruction_builder::InstructionBuilder;
use crate::AsInstruction;
use crate::Instruction;
use crate::Label;
use crate::Ref;
use crate::Type;
use crate::Value;

pub fn if_then<B, Branch>(builder: &mut B, cond: impl Into<Value>, then_branch: Branch)
where
    B: InstructionBuilder + ?Sized,
    Branch: FnOnce(&mut B),
{
    let then_label = builder.next_label();
    let break_label = builder.next_label();

    builder.jmpif(then_label, cond);
    builder.jmp(break_label);

    builder.label(then_label);
    then_branch(builder);

    builder.label(break_label);
}

pub fn if_then_else<B, IfBranch, ElseBranch>(
    builder: &mut B,
    cond: impl Into<Value>,
    then_branch: IfBranch,
    else_branch: ElseBranch,
) where
    B: InstructionBuilder + ?Sized,
    IfBranch: FnOnce(&mut B),
    ElseBranch: FnOnce(&mut B),
{
    let then_label = builder.next_label();
    let else_label = builder.next_label();
    let break_label = builder.next_label();

    builder.jmpif(then_label, cond);
    builder.jmp(else_label);

    builder.label(then_label);
    then_branch(builder);
    builder.jmp(break_label);

    builder.label(else_label);
    else_branch(builder);

    builder.label(break_label);
}

pub fn counter_loop<B, F>(
    builder: &mut B,
    counter: impl Into<Ref>,
    inc_val: impl Into<Value>,
    high_val: impl Into<Value>,
    f: F,
) where
    B: InstructionBuilder + ?Sized,
    F: FnOnce(&mut B),
{
    let break_label = builder.next_label();
    let loop_label = builder.next_label();
    let done = builder.local_temp(Type::Bool);

    let counter = counter.into();

    builder.label(loop_label);
    builder.gt(done.clone(), counter.clone(), high_val);
    builder.jmpif(break_label, done);

    builder.local_begin();
    f(builder);
    builder.local_end();

    builder.add(counter.clone(), counter, inc_val);
    builder.jmp(loop_label);
    builder.label(break_label);
}

pub fn gen_default_init<B>(builder: &mut B, at: impl Into<Ref>, ty: &Type)
where
    B: InstructionBuilder + ?Sized,
{
    match ty.default_literal() {
        Some(lit) => {
            builder.mov(at, lit);
        },

        None => {
            let at = at.into();
            builder.gen_fill_byte(at, Value::SizeOf(ty.clone()), Value::LiteralU8(0));
        }
    }
}

// inline IR for FillByte-style memory set procedure
pub fn gen_fill_byte<B>(builder: &mut B, at: Ref, count: Value, byte_val: Value) 
where
    B: InstructionBuilder + ?Sized,
{
    builder.comment(&format!("fill_byte: {}, count={}, value {}", at, count, byte_val));

    let byte_ptr_ty = Type::U8.ptr();

    // dst_ptr := @at as ^UInt8
    let at_addr = builder.local_temp(Type::Nothing.ptr());
    builder.addr_of(at_addr.clone(), at);

    let dst_ptr = builder.local_temp(byte_ptr_ty.clone()).to_ref();
    builder.cast(dst_ptr.clone(), at_addr, byte_ptr_ty.clone());

    // end_ptr := dst_ptr + count 
    let end_ptr = builder.local_temp(byte_ptr_ty.clone());
    builder.add(end_ptr.clone(), dst_ptr.clone(), count);

    let continue_label = builder.next_label();
    let break_label = builder.next_label();

    builder.label(continue_label);

    // at_end := dst_ptr = end_ptr
    let at_end = builder.local_temp(Type::Bool);
    builder.eq(at_end.clone(), dst_ptr.clone(), end_ptr);

    // if at_end then break
    builder.jmpif(break_label, at_end);

    // else dst_ptr^ := byte_val
    builder.mov(dst_ptr.clone().to_deref(), byte_val.clone());

    // dst_ptr += 1;
    builder.add(dst_ptr.clone(), dst_ptr.clone(), Value::LiteralISize(1));

    // continue
    builder.jmp(continue_label);

    builder.label(break_label);
}

pub fn while_do<B, CondFn, DoFn>(builder: &mut B, cond_fn: CondFn, do_fn: DoFn)
where
    B: InstructionBuilder + ?Sized,
    CondFn: Fn(&mut B, Ref),
    DoFn: Fn(&mut B, Label, Label),
{
    let continue_label = builder.next_label();
    let break_label = builder.next_label();

    let cond_val = builder.local_temp(Type::Bool);

    builder.label(continue_label);

    builder.local_begin();
    {
        cond_fn(builder, cond_val.to_ref());
    }
    builder.local_end();

    builder.not(cond_val, cond_val);
    builder.jmpif(break_label, cond_val);

    builder.local_begin();
    {
        do_fn(builder, continue_label, break_label);
    }
    builder.local_end();

    builder.jmp(continue_label);

    builder.label(break_label);
}

/// Call `f` for every structural member of the object of type `ty_def` found at the `at`
/// reference.
///
/// Each call to the visitor should return a value indicating whether the reference
/// was used. If the visitor did nothing, we can prune the field local used to access it.
/// This assumes that false indicates that no new instructions were added and may misbehave
/// otherwise.
pub fn visit_deep<B, Visitor>(builder: &mut B, at: impl Into<Ref>, ty: &Type, f: Visitor) -> bool
where
    B: InstructionBuilder + ?Sized,
    Visitor: Fn(&mut B, &Type, Ref) -> bool + Copy,
{
    let at = at.into();

    match ty {
        Type::Struct(struct_id) => {
            let Some(struct_def) = builder.metadata().get_struct_def(*struct_id) else {
                panic!("visit_deep: missing definition for struct {struct_id}")
            };

            let fields: Vec<_> = struct_def
                .fields
                .iter()
                .map(|(field_id, field)| (*field_id, field.ty.clone()))
                .collect();

            let mut result = false;
            for (field, field_ty) in fields {
                if !(field_ty.is_object() || field_ty.is_complex()) {
                    continue;
                }

                let field_ref = at.field_ref(Type::Struct(*struct_id), field);
                result |= builder.visit_deep(field_ref.to_deref(), &field_ty, f);
            }

            result
        },

        Type::Variant(id) => {
            let variant_def = builder
                .metadata()
                .get_variant_def(*id)
                .unwrap_or_else(|| panic!("missing variant def {}", id));

            let cases = variant_def.cases.to_vec();

            let is_not_case = builder.local_temp(Type::Bool);

            // jump out of the search loop if we find the matching case
            let break_label = builder.next_label();

            let mut result = false;

            // for each case, check if the tag matches and jump past it if not

            for (tag, case) in cases.iter().enumerate() {
                builder.comment(&format!("testing for variant case {} ({})", tag, case.name));

                if let Some(data_ty) = &case.ty {
                    if !(data_ty.is_object() || data_ty.is_complex()) {
                        continue;
                    }

                    let skip_case_label = builder.next_label();

                    // get the tag
                    let tag_ref = at.clone().vartag_ref(id.to_variant_type());

                    // is_not_case := at.tag != tag
                    builder.neq(is_not_case, tag_ref.to_deref(), case.tag.clone());
                    builder.jmpif(skip_case_label, is_not_case.clone());

                    // get ptr into case data and visit it

                    // only one data_ptr local will be allocated depending on which case is
                    // active, so a scope is needed here to stop the local counter being
                    // incremented once per case
                    builder.local_begin();
                    {
                        let data_ref = at.clone().vardata_ref(id.to_variant_type(), tag);

                        result |= builder.visit_deep(data_ref.to_deref(), &data_ty, f);
                    }
                    builder.local_end();

                    // break after any case executes
                    builder.jmp(break_label);

                    // jump to here if this case isn't active
                    builder.label(skip_case_label);
                }
            }

            builder.label(break_label);

            result
        },

        Type::Array { element, dim } => {
            if !element.is_object() && !element.is_complex() {
                return false;
            }

            let mut result = false;

            for i in 0..*dim {
                let index = Value::LiteralI32(i as i32);

                let element_ref = at.clone().element_ref(ty.clone(), index);

                result |= builder.visit_deep(element_ref.to_deref(), element, f);
            }

            result
        },

        // field or element
        _ => f(builder, ty, at),
    }
}

pub fn jmp_exists(instructions: &[impl AsInstruction], to_label: Label) -> bool {
    instructions.iter().any(|i| match i.as_instruction() {
        Instruction::Jump { dest } | Instruction::JumpIf { dest, .. } => *dest == to_label,
        _ => false,
    })
}