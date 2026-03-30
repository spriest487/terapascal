use terapascal_common::CompileOpts;
use terapascal_common::version::Version;
use super::*;

fn instructions_without_comments(actual: &[DebugInstruction], count: usize) -> Vec<Instruction> {
    actual
        .iter()
        .filter_map(|i| match &i.instruction {
            Instruction::Comment(..) => None,
            instruction => Some(instruction.clone()),
        })
        .take(count)
        .collect::<Vec<_>>()
}

#[test]
fn end_loop_scope_ends_at_right_scope_level() {
    let ctx = typ::Context::root(CompileOpts::default());
    let mut library = LibraryBuilder::new("test", Version::default(), &ctx, [], CodegenOpts::default());
    let mut builder = IRBuilder::new(&mut library);

    let initial_scope = builder.local_stack.len();

    let continue_label = builder.next_label();
    let break_label = builder.next_label();
    builder.begin_loop_body_scope(continue_label, break_label);
    builder.end_loop_body_scope();

    assert_eq!(initial_scope, builder.local_stack.len());
}

#[test]
fn break_cleans_up_loop_locals() {
    let ctx = typ::Context::root(CompileOpts::default());
    let mut library = LibraryBuilder::new("test", Version::default(), &ctx, [], CodegenOpts::default());
    let mut builder = IRBuilder::new(&mut library);

    let continue_label = builder.next_label();
    let break_label = builder.next_label();

    builder.begin_loop_body_scope(continue_label, break_label);
    builder.local_var(
        Type::Object(ObjectID::Any),
        Some(Arc::new("local1".to_string())),
    );
    builder.local_var(
        Type::Object(ObjectID::Any),
        Some(Arc::new("local2".to_string())),
    );

    builder.comment("before_break");
    builder.break_loop();

    let from = builder
        .instructions
        .iter()
        .position(|i| match &i.instruction {
            Instruction::Comment(c) => c == "before_break",
            _ => false,
        })
        .unwrap();

    // Both locals should be released and cleared
    let expect = &[
        Instruction::Release {
            at: Ref::Local(LocalID(1)),
            weak: false,
            released_out: Ref::Discard,
        },
        Instruction::Move {
            out: Ref::Local(LocalID(1)),
            new_val: Value::Default(Type::Object(ObjectID::Any)),
        },
        Instruction::Release {
            at: Ref::Local(LocalID(0)),
            weak: false,
            released_out: Ref::Discard,
        },
        Instruction::Move {
            out: Ref::Local(LocalID(0)),
            new_val: Value::Default(Type::Object(ObjectID::Any)),
        },
        // and the final jmp for the break
        Instruction::Jump { dest: break_label },
    ];
    
    let actual = instructions_without_comments(
        &builder.instructions[from + 1..],
        expect.len()
    );

    assert_eq!(expect.as_slice(), actual.as_slice());
}
