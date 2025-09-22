use crate::codegen::builder::Builder;
use terapascal_ir::instruction_builder::InstructionBuilder as _;
use terapascal_ir as ir;

#[allow(unused)]
impl<'m, 'l: 'm> Builder<'m, 'l> {
    pub fn if_then<Branch>(&mut self, cond: impl Into<ir::Value>, then_branch: Branch)
    where
        Branch: FnOnce(&mut Self),
    {
        let then_label = self.next_label();
        let break_label = self.next_label();

        self.jmpif(then_label, cond);
        self.jmp(break_label);

        self.label(then_label);
        then_branch(self);

        self.label(break_label);
    }

    pub fn if_then_else<Branch>(&mut self,
        cond: impl Into<ir::Value>,
        then_branch: Branch,
        else_branch: Branch,
    )
    where
        Branch: FnOnce(&mut Self)
    {
        let then_label = self.next_label();
        let else_label = self.next_label();
        let break_label = self.next_label();

        self.jmpif(then_label, cond);
        self.jmp(else_label);

        self.label(then_label);
        then_branch(self);
        self.jmp(break_label);

        self.label(else_label);
        else_branch(self);

        self.label(break_label);
    }

    pub fn counter_loop<F>(
        &mut self,
        counter: impl Into<ir::Ref>,
        inc_val: impl Into<ir::Value>,
        high_val: impl Into<ir::Value>,
        f: F,
    )
    where F: Fn(&mut Self)
    {
        let break_label = self.next_label();
        let loop_label = self.next_label();
        let done = self.local_temp(ir::Type::Bool);

        let counter = counter.into();

        self.label(loop_label);
        self.gte(done.clone(), counter.clone(), high_val);
        self.jmpif(break_label, done);

        f(self);

        self.add(counter.clone(), counter, inc_val);
        self.jmp(loop_label);
        self.label(break_label);
    }
}
