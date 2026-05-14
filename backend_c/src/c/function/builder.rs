use terapascal_common::span::Span;
use crate::c::Unit;
use crate::ir;

pub struct FuncInstanceBuilder<'a> {
    unit: &'a Unit<'a>,

    local_stack: ir::LocalStack,

    debug_stack: Vec<Span>,

    next_label: ir::Label,

    body: ir::InstructionList,
}

impl<'a> FuncInstanceBuilder<'a> {
    pub fn new(unit: &'a Unit) -> Self {
        Self {
            unit,
            local_stack: ir::LocalStack::new(),
            debug_stack: Vec::new(),
            next_label: ir::Label(ir::EXIT_LABEL.0 + 1),
            body: ir::InstructionList::new()
        }
    }

    pub fn finish(mut self) -> ir::InstructionList {
        let local_count = self.local_stack.local_slot_count();
        let mut init_instructions = Vec::with_capacity(local_count);

        for (local_id, ty) in self.local_stack.finish() {
            init_instructions.push(ir::Instruction::LocalAlloc(local_id, ty));
        }

        self.body.splice(0..0, init_instructions);
        self.body
    }
}

impl<'a> ir::InstructionBuilder for FuncInstanceBuilder<'a> {
    fn emit(&mut self, instruction: ir::Instruction) {
        let source = self.debug_stack.last().cloned();
        self.body.push(instruction, source);
    }

    fn metadata(&self) -> &impl ir::MetadataSource {
        self.unit.metadata
    }

    fn local_stack(&self) -> &ir::LocalStack {
        &self.local_stack
    }

    fn local_stack_mut(&mut self) -> &mut ir::LocalStack {
        &mut self.local_stack
    }

    fn is_debug(&self) -> bool {
        true
    }

    fn next_label(&mut self) -> ir::Label {
        let next = self.next_label;
        self.next_label.0 += 1;
        next
    }

    fn push_source(&mut self, ctx: Span) {
        self.debug_stack.push(ctx);
    }

    fn pop_source(&mut self) {
        self.debug_stack.pop();
    }
}
