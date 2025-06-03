use super::*;

fn instructions_without_comments(actual: &[Instruction], count: usize) -> Vec<Instruction> {
    actual
        .iter()
        .filter(|i| match i {
            Instruction::Comment(..) => false,
            _ => true,
        })
        .cloned()
        .take(count)
        .collect::<Vec<_>>()
}

#[test]
fn end_loop_scope_ends_at_right_scope_level() {
    let ctx = typ::Context::root(Span::zero("test"));
    let mut library = LibraryBuilder::new(ctx, Metadata::default(), CodegenOpts::default());
    let mut builder = Builder::new(&mut library);

    let initial_scope = builder.scopes.len();

    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();
    builder.begin_loop_body_scope(continue_label, break_label);
    builder.end_loop_body_scope();

    assert_eq!(initial_scope, builder.scopes.len());
}

#[test]
fn break_cleans_up_loop_locals() {
    let ctx = typ::Context::root(Span::zero("test"));
    let mut library = LibraryBuilder::new(ctx, Metadata::default(), CodegenOpts::default());
    let mut builder = Builder::new(&mut library);

    let continue_label = builder.alloc_label();
    let break_label = builder.alloc_label();

    builder.begin_loop_body_scope(continue_label, break_label);
    builder.local_new(
        Type::RcPointer(VirtualTypeID::Any),
        Some("local1".to_string()),
    );
    builder.local_new(
        Type::RcPointer(VirtualTypeID::Any),
        Some("local2".to_string()),
    );

    builder.comment("before_break");
    builder.break_loop();

    let from = builder
        .instructions
        .iter()
        .position(|i| match i {
            Instruction::Comment(c) => c == "before_break",
            _ => false,
        })
        .unwrap();

    // Both locals should be released
    let expect = &[
        Instruction::Release {
            at: Ref::Local(LocalID(1)),
            weak: false,
        },
        Instruction::Release {
            at: Ref::Local(LocalID(0)),
            weak: false,
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
