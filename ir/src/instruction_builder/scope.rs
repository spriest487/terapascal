use crate::ArgID;
use crate::Label;
use crate::LocalID;
use crate::Ref;
use crate::Type;
use crate::RESULT_REF;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::ops::RangeInclusive;
use std::sync::Arc;
use terapascal_common::SharedStringKey;

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

    pub fn find_binding(&self, name: &str) -> Option<&ScopedBinding> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.find_named_binding(name))
    }
    
    pub fn local_slot_count(&self) -> usize {
        self.locals.len()
    }

    // locals from all scopes up to the target scope, in order of deepest->shallowest,
    // then in reverse allocation order
    pub fn current_bindings(&self, range: impl Into<RangeInclusive<usize>>) -> impl Iterator<Item=&ScopedBinding> {
        self.scopes[range.into()]
            .iter()
            .rev()
            .flat_map(|scope| scope.bindings().iter().rev())
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

        for binding in end_scope.bindings {
            if let BindingStorage::Local(id) = binding.storage {
                if let Some(slot) = self.locals.get_mut(&id) {
                    slot.expired = true;
                }
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
    
    pub fn bind_return(&mut self, ty: Type) {
        assert!(
            self.current_scope().find_result_binding().is_none(), 
            "bind_return: {} must not already be bound",
            RESULT_REF,
        );

        self.bind(ty, BindingStorage::Result, None, false, false)
    }

    pub fn bind_param(&mut self, id: ArgID, ty: Type, name: Arc<String>, by_ref: bool) {
        if by_ref {
            assert!(ty.is_temp_ref(), "by-ref parameters must have temp reference type");
        }

        // TODO: fix param RC so they aren't auto-retained *or* released
        let autorelease = true;

        self.bind(ty, BindingStorage::Arg(id), Some(name), autorelease, by_ref)
    }

    // used for implicit anonymous params like closure pointer
    pub fn bind_unnamed_param(&mut self, id: ArgID, ty: Type) {
        self.bind(ty, BindingStorage::Arg(id), None, false, false);
    }

    // closure capture (by-ref because the actual value is a field ref)
    pub fn bind_capture(&mut self, ty: Type, name: impl Into<Arc<String>>) -> LocalID {
        self.bind_local_slot(ty.temp_ref(), Some(name.into()), false, true)
    }
    
    pub fn bind_temp(&mut self, ty: Type) -> LocalID {
        self.bind_local_slot(ty, None, false, false)
    }

    pub fn bind_auto_temp(&mut self, ty: Type) -> LocalID {
        self.bind_local_slot(ty, None, true, false)
    }

    pub fn bind_var(&mut self, ty: Type, name: impl Into<Arc<String>>) -> LocalID {
        self.bind_local_slot(ty, Some(name.into()), true, false)
    }
    
    fn bind_local_slot(&mut self, ty: Type, name: Option<Arc<String>>, auto_release: bool, by_ref: bool) -> LocalID {
        // if there's an existing local which is expired (out of scope), it can be reused
        // instead of creating a new local
        let existing_id = self.locals
            .iter()
            .find_map(|(id, slot)| {
                (slot.expired && slot.ty == ty)
                    .then_some(*id)
            });

        let id = existing_id.unwrap_or_else(|| {
            let next_id = self.next_local;
            self.next_local += 1;
            LocalID(next_id)
        });

        assert!(
            self.current_scope().find_local_binding(id).is_none(),
            "current scope must not already have a binding for {}",
            Ref::Local(id),
        );
        
        self.bind(ty.clone(), BindingStorage::Local(id), name, auto_release, by_ref);

        self.locals.insert(id, LocalSlot {
            ty,
            expired: false,
        });
        
        id
    }
    
    fn bind(&mut self,
        ty: Type,
        storage: BindingStorage,
        name: Option<Arc<String>>,
        auto_release: bool,
        by_ref: bool,
    ) {
        let scope = self.current_scope_mut();
        
        scope.bindings.push(ScopedBinding {
            storage,
            ty: ty.clone(),
            auto_release,
            by_ref,
        });

        if let Some(name) = name {
            if scope.named_bindings.insert(SharedStringKey(name.clone()), storage).is_some() {
                panic!("current scope must not already have a binding named {}", name);
            }
        }
    }
}

#[derive(Debug)]
struct LocalSlot {
    ty: Type,
    expired: bool,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum BindingStorage {
    Result,
    Arg(ArgID),
    Local(LocalID),
}

impl BindingStorage {
    pub fn to_ref(&self) -> Ref {
        match self {
            BindingStorage::Result => Ref::Result,
            BindingStorage::Arg(id) => Ref::Arg(*id),
            BindingStorage::Local(id) => Ref::Local(*id),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopedBinding {
    pub storage: BindingStorage,
    pub ty: Type,

    // if true, insert a release of this binding automatically when the declaring scope ends 
    pub auto_release: bool,

    // TODO: represent this in the lang typesystem instead
    // if true, hints that the variable represents a local reference, and it should be dereferenced
    // on any usage e.g. in `find_local_ref`
    pub by_ref: bool,
}

impl ScopedBinding {
    pub fn to_ref(&self) -> Ref {
        if self.by_ref {
            self.storage.to_ref().to_deref()
        } else {
            self.storage.to_ref()
        }
    }
}

#[derive(Debug)]
pub struct LocalScope {
    bindings: Vec<ScopedBinding>,
    named_bindings: HashMap<SharedStringKey, BindingStorage>,

    // debug context pushes belonging to this scope - when cleaning up this scope, any un-popped
    // entries in the debug context stack that were pushed during this scope also need to be popped
    debug_ctx_depth: usize,
}

impl LocalScope {
    pub fn new() -> Self {
        Self {
            bindings: Vec::new(),
            named_bindings: HashMap::new(),

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

    pub fn bindings(&self) -> &[ScopedBinding] {
        &self.bindings
    }

    pub fn find_binding(&self, storage: BindingStorage) -> Option<&ScopedBinding> {
        self.bindings.iter().find(|l| l.storage == storage)
    }

    pub fn find_result_binding(&self) -> Option<&ScopedBinding> {
        self.find_binding(BindingStorage::Result)
    }

    pub fn find_arg_binding(&self, id: ArgID) -> Option<&ScopedBinding> {
        self.find_binding(BindingStorage::Arg(id))
    }

    pub fn find_local_binding(&self, id: LocalID) -> Option<&ScopedBinding> {
        self.find_binding(BindingStorage::Local(id))
    }

    pub fn find_named_binding(&self, name: &str) -> Option<&ScopedBinding> {
        let storage = self.named_bindings.get(name)?;
        self.find_binding(*storage)
    }
}

#[derive(Debug)]
pub struct LoopScope {
    pub continue_label: Label,
    pub break_label: Label,

    pub block_level: usize,
}
