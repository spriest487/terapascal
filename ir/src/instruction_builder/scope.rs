use crate::Label;
use crate::LocalID;
use crate::Ref;
use crate::Type;
use crate::RETURN_LOCAL;
use crate::RETURN_REF;
use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::sync::Arc;
use terapascal_common::SharedStringKey;

#[derive(Debug)]
pub struct LocalStack {
    scopes: Vec<LocalScope>,

    loops: Vec<LoopScope>,
}

impl LocalStack {
    pub fn new() -> Self {
        Self {
            scopes: vec![LocalScope::new(0)],

            loops: Vec::new(),
        }
    }
    
    pub fn len(&self) -> usize {
        self.scopes.len()
    }

    pub fn find_local(&self, name: &str) -> Option<&LocalBinding> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.local_by_name(name)
                .and_then(|id| scope.local_by_id(id)))
    }

    // locals from all scopes up to the target scope, in order of deepest->shallowest,
    // then in reverse allocation order
    pub fn all_locals(&self, range: impl Into<RangeInclusive<usize>>) -> impl Iterator<Item=&LocalBinding> {
        self.scopes[range.into()]
            .iter()
            .rev()
            .flat_map(|scope| scope.locals().iter().rev())
    }
    
    pub fn debug_ctx_count(&self, range: impl Into<RangeInclusive<usize>>) -> usize {
        self.scopes[range.into()]
            .iter()
            .map(|scope| scope.debug_ctx_count())
            .sum()
    }
    
    pub fn begin(&mut self) {
        let scope = self.current_scope().new_child();
        self.scopes.push(scope);
    }
    
    pub fn end(&mut self) {
        if self.scopes.pop().is_none() {
            panic!("mismatched begin/end scope calls, no scope to pop");
        }
    }
    
    pub fn finish(mut self) {
        while !self.scopes.is_empty() {
            self.end();
        }
    }

    pub fn current_scope(&self) -> &LocalScope {
        self.scopes
            .iter()
            .rev()
            .next()
            .expect("scope must be active")
    }

    pub fn current_scope_mut(&mut self) -> &mut LocalScope {
        self.scopes
            .iter_mut()
            .rev()
            .next()
            .expect("scope must be active")
    }
    
    pub fn push_loop(&mut self, continue_label: Label, break_label: Label) {
        self.loops.push(LoopScope {
            continue_label,
            break_label,
            block_level: self.scopes.len(),
        });
    }
    
    pub fn pop_loop(&mut self) {
        self.loops
            .pop()
            .expect("end_loop called without an active loop");
    }

    pub fn current_loop(&self) -> Option<&LoopScope> {
        self.loops.last()
    }
}

#[derive(Clone, Debug)]
pub enum LocalBinding {
    // the builder created this local allocation and must track its lifetime to drop it
    New {
        id: LocalID,
        ty: Type,
    },

    // the builder created this local allocation but we don't want to track its lifetime
    Temp {
        id: LocalID
    },

    // function parameter slots as established by the calling convention - %1.. if the function
    // has a return value, otherwise $0..

    // by-value parameter. value is copied into the local by the caller
    Param {
        id: LocalID,
        ty: Type,

        // by-ref parameter?
        // if so pointer to the value is copied into the local by the caller, and must
        // be dereferenced every time it is used
        by_ref: bool,
    },

    // return value: always occupies local %0 if present.
    // the return value is not named and is not cleaned up on scope exit (if it's a
    // rc type, the reference is owned by the caller after the function exits)
    Return,
}

impl LocalBinding {
    pub fn id(&self) -> LocalID {
        match self {
            LocalBinding::New { id, .. } | LocalBinding::Temp { id, .. } | LocalBinding::Param { id, .. } => *id,
            LocalBinding::Return { .. } => LocalID(0),
        }
    }

    /// if a local is by-ref, it's treated in pascal syntax like a value of this type but in the IR
    /// it's actually a pointer. if this returns true, it's necessary to wrap Ref::Local values
    /// that reference this local in a Ref::Deref to achieve the same effect as the pascal syntax
    pub fn by_ref(&self) -> bool {
        match self {
            LocalBinding::Param { by_ref, .. } => *by_ref,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct LocalScope {
    locals: Vec<LocalBinding>,
    named_locals: HashMap<SharedStringKey, LocalID>,

    // debug context pushes belonging to this scope - when cleaning up this scope, any un-popped
    // entries in the debug context stack that were pushed during this scope also need to be popped
    debug_ctx_depth: usize,

    next_local: usize,
}

impl LocalScope {
    pub fn new(next_local: usize) -> Self {
        Self {
            locals: Vec::new(),
            named_locals: HashMap::new(),

            debug_ctx_depth: 0,

            next_local,
        }
    }

    pub fn new_child(&self) -> Self {
        Self::new(self.next_local)
    }

    pub fn debug_ctx_count(&self) -> usize {
        self.debug_ctx_depth
    }

    pub fn inc_debug_ctx_count(&mut self) {
        self.debug_ctx_depth += 1;
    }

    pub fn dec_debug_ctx_count(&mut self) {
        self.debug_ctx_depth -= 1;
    }

    pub fn locals(&self) -> &[LocalBinding] {
        &self.locals
    }

    pub fn local_by_id(&self, id: LocalID) -> Option<&LocalBinding> {
        self.locals.iter().find(|l| l.id() == id)
    }

    pub fn local_by_name(&self, name: &str) -> Option<LocalID> {
        self.named_locals.get(name).cloned()
    }

    pub fn bind_param(
        &mut self,
        name: Option<impl Into<Arc<String>>>,
        ty: Type,
        by_ref: bool,
    ) -> LocalID {
        if by_ref {
            let is_ptr = match &ty {
                Type::Pointer(..) => true,
                _ => false,
            };
            assert!(is_ptr, "by-ref parameters must have pointer type");
        }

        let id = LocalID(self.next_local);

        assert!(
            self.local_by_id(id).is_none(),
            "scope must not already have a binding for {}: {:?}",
            Ref::Local(id),
            self
        );

        self.locals.push(LocalBinding::Param { id, ty, by_ref });

        let name = name.map(Into::into);
        if let Some(name) = name {
            if self.named_locals.insert(SharedStringKey(name.clone()), id).is_some() {
                panic!("scope must not already have a binding named {}", name);
            }
        }

        self.next_local += 1;

        id
    }

    pub fn bind_return(&mut self) -> Ref {
        assert_eq!(RETURN_LOCAL.0, self.next_local, "return local must be bound first");

        let slot_free = self.local_by_id(RETURN_LOCAL).is_none();
        assert!(slot_free, "%0 must not already be bound in bind_return");

        self.locals.push(LocalBinding::Return);
        self.next_local += 1;

        RETURN_REF
    }

    pub fn bind_temp(&mut self) -> LocalID {
        let id = LocalID(self.next_local);

        assert!(
            self.local_by_id(id).is_none(),
            "scope must not already have a binding for {}",
            Ref::Local(id),
        );

        self.locals.push(LocalBinding::Temp { id });
        self.next_local += 1;

        id
    }

    pub fn bind_new(&mut self, name: Option<Arc<String>>, ty: Type) -> LocalID {
        let id = LocalID(self.next_local);

        assert_ne!(Type::Nothing, ty);

        if let Some(name) = &name {
            if self.named_locals.insert(SharedStringKey(name.clone()), id).is_some() {
                panic!("scope must not already have a binding for {}", name);
            }
        }

        self.locals.push(LocalBinding::New {
            id,
            ty: ty.clone(),
        });

        self.next_local += 1;

        id
    }
}

#[derive(Debug)]
pub struct LoopScope {
    pub continue_label: Label,
    pub break_label: Label,

    pub block_level: usize,
}
