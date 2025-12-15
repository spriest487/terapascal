use crate::LocalID;
use crate::Ref;
use crate::Type;
use crate::RETURN_LOCAL;
use crate::Label;
use std::collections::{BTreeMap, HashMap};
use std::ops::RangeInclusive;
use std::sync::Arc;
use terapascal_common::SharedStringKey;

#[derive(Debug)]
struct LocalSlot {
    ty: Type,
    expired: bool,
}

#[derive(Debug)]
pub struct LocalStack {
    scopes: Vec<LocalScope>,

    locals: BTreeMap<LocalID, LocalSlot>,
    next_local: usize,

    loops: Vec<LoopScope>,
}

impl LocalStack {
    pub fn new() -> Self {
        Self {
            locals: BTreeMap::new(),
            next_local: 0,
            
            scopes: vec![LocalScope::new()],

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
    
    pub fn local_slot_count(&self) -> usize {
        self.locals.len()
    }

    // locals from all scopes up to the target scope, in order of deepest->shallowest,
    // then in reverse allocation order
    pub fn current_local_bindings(&self, range: impl Into<RangeInclusive<usize>>) -> impl Iterator<Item=&LocalBinding> {
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
        self.scopes.push(LocalScope::new());
    }
    
    pub fn end(&mut self) {
        let Some(end_scope) = self.scopes.pop() else {
            panic!("mismatched begin/end scope calls, no scope to pop");
        };

        for binding in end_scope.locals {
            // it's OK for a slot not to exist for every local, locals with implicit storage like
            // params are ignored
            if let Some(slot) = self.locals.get_mut(&binding.id) {
                slot.expired = true;
            }
        }
    }
    
    #[must_use]
    pub fn finish(mut self) -> BTreeMap<LocalID, Type> {
        while !self.scopes.is_empty() {
            self.end();
        }
        
        self.locals.into_iter()
            .map(|(id, slot)| (id, slot.ty))
            .collect()
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
    
    pub fn bind_return(&mut self, ty: Type) -> LocalID {
        assert!(
            self.current_scope().local_by_id(RETURN_LOCAL).is_none(), 
            "%0 must not already be bound in bind_return"
        );

        self.bind_local(ty, None, false, false, true)
    }
    
    pub fn bind_param(&mut self, ty: Type, name: Arc<String>, by_ref: bool) -> LocalID {
        if by_ref {
            assert!(ty.is_temp_ref(), "by-ref parameters must have temp reference type");
        }

        // TODO: fix param RC so they aren't auto-retained *or* released
        let autorelease = true;

        self.bind_local(ty, Some(name), autorelease, by_ref, true)
    }

    // used for implicit anonymous params like closure pointer
    pub fn bind_unnamed_param(&mut self, ty: Type) -> LocalID {
        self.bind_local(ty, None, false, false, true)
    }

    // closure capture (by-ref because the actual value is a field ref)
    pub fn bind_capture(&mut self, ty: Type, name: impl Into<Arc<String>>) -> LocalID {
        self.bind_local(ty.temp_ref(), Some(name.into()), false, true, false)
    }
    
    pub fn bind_temp(&mut self, ty: Type) -> LocalID {
        self.bind_local(ty, None, false, false, false)
    }

    pub fn bind_auto_temp(&mut self, ty: Type) -> LocalID {
        self.bind_local(ty, None, true, false, false)
    }

    pub fn bind_var(&mut self, ty: Type, name: impl Into<Arc<String>>) -> LocalID {
        self.bind_local(ty, Some(name.into()), true, false, false)
    }
    
    fn bind_local(&mut self, ty: Type, name: Option<Arc<String>>, auto_release: bool, by_ref: bool, implicit: bool) -> LocalID {
        // if there's an existing local which is expired (out of scope), it can be reused
        // instead of creating a new local
        let mut existing_id = None;
        if !implicit {
            existing_id = self.locals
                .iter()
                .find_map(|(id, slot)| {
                    (slot.expired && slot.ty == ty)
                        .then_some(*id)
                });
        }

        let id = existing_id.unwrap_or_else(|| {
            let next_id = self.next_local;
            self.next_local += 1;
            LocalID(next_id)
        });

        assert!(
            self.current_scope().local_by_id(id).is_none(),
            "current scope must not already have a binding for {}",
            Ref::Local(id),
        );

        let scope = self.current_scope_mut();
        
        scope.locals.push(LocalBinding {
            id,
            ty: ty.clone(),
            auto_release,
            by_ref,
        });

        if let Some(name) = name {
            if scope.named_locals.insert(SharedStringKey(name.clone()), id).is_some() {
                panic!("current scope must not already have a binding named {}", name);
            }
        }
        
        if !implicit {
            self.locals.insert(id, LocalSlot {
                ty,
                expired: false,
            });
        }

        id
    }
}

#[derive(Clone, Debug)]
pub struct LocalBinding {
    pub id: LocalID,
    pub ty: Type,

    // if true, insert a release of this binding automatically when the declaring scope ends 
    pub auto_release: bool,

    // TODO: represent this in the lang typesystem instead
    // if true, hints that the variable represents a local reference, and it should be dereferenced
    // on any usage e.g. in `find_local_ref`
    pub by_ref: bool,
}

#[derive(Debug)]
pub struct LocalScope {
    locals: Vec<LocalBinding>,
    named_locals: HashMap<SharedStringKey, LocalID>,

    // debug context pushes belonging to this scope - when cleaning up this scope, any un-popped
    // entries in the debug context stack that were pushed during this scope also need to be popped
    debug_ctx_depth: usize,
}

impl LocalScope {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            named_locals: HashMap::new(),

            debug_ctx_depth: 0,
        }
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
        self.locals.iter().find(|l| l.id == id)
    }

    pub fn local_by_name(&self, name: &str) -> Option<LocalID> {
        self.named_locals.get(name).cloned()
    }
}

#[derive(Debug)]
pub struct LoopScope {
    pub continue_label: Label,
    pub break_label: Label,

    pub block_level: usize,
}
