use crate::codegen::builder::Builder;
use std::collections::HashMap;
use std::sync::Arc;
use terapascal_common::SharedStringKey;
use terapascal_ir as ir;

#[derive(Clone, Debug)]
pub enum Local {
    // the builder created this local allocation and must track its lifetime to drop it
    New {
        id: ir::LocalID,
        ty: ir::Type,
    },

    // the builder created this local allocation but we don't want to track its lifetime
    Temp {
        id: ir::LocalID
    },

    // function parameter slots as established by the calling convention - %1.. if the function
    // has a return value, otherwise $0..

    // by-value parameter. value is copied into the local by the caller
    Param {
        id: ir::LocalID,
        ty: ir::Type,

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

impl Local {
    pub fn id(&self) -> ir::LocalID {
        match self {
            Local::New { id, .. } | Local::Temp { id, .. } | Local::Param { id, .. } => *id,
            Local::Return { .. } => ir::LocalID(0),
        }
    }

    /// if a local is by-ref, it's treated in pascal syntax like a value of this type but in the IR
    /// it's actually a pointer. if this returns true, it's necessary to wrap Ref::Local values
    /// that reference this local in a Ref::Deref to achieve the same effect as the pascal syntax
    pub fn by_ref(&self) -> bool {
        match self {
            Local::Param { by_ref, .. } => *by_ref,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub(super) struct LocalScope {
    locals: Vec<Local>,
    named_locals: HashMap<SharedStringKey, ir::LocalID>,

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

    pub fn locals(&self) -> &[Local] {
        &self.locals
    }

    pub fn local_by_id(&self, id: ir::LocalID) -> Option<&Local> {
        self.locals.iter().find(|l| l.id() == id)
    }

    pub fn local_by_name(&self, name: &str) -> Option<ir::LocalID> {
        self.named_locals.get(name).cloned()
    }

    pub fn bind_param(
        &mut self,
        name: Option<impl Into<Arc<String>>>,
        ty: ir::Type,
        by_ref: bool,
    ) -> ir::LocalID {
        if by_ref {
            let is_ptr = match &ty {
                ir::Type::Pointer(..) => true,
                _ => false,
            };
            assert!(is_ptr, "by-ref parameters must have pointer type");
        }

        let id = ir::LocalID(self.next_local);

        assert!(
            self.local_by_id(id).is_none(),
            "scope must not already have a binding for {}: {:?}",
            ir::Ref::Local(id),
            self
        );

        self.locals.push(Local::Param { id, ty, by_ref });

        let name = name.map(Into::into);
        if let Some(name) = name {
            if self.named_locals.insert(SharedStringKey(name.clone()), id).is_some() {
                panic!("scope must not already have a binding named {}", name);
            }
        }

        self.next_local += 1;

        id
    }

    pub fn bind_return(&mut self) -> ir::Ref {
        assert_eq!(ir::RETURN_LOCAL.0, self.next_local, "return local must be bound first");

        let slot_free = self.local_by_id(ir::RETURN_LOCAL).is_none();
        assert!(slot_free, "%0 must not already be bound in bind_return");

        self.locals.push(Local::Return);
        self.next_local += 1;

        ir::RETURN_REF
    }

    pub fn bind_temp(&mut self) -> ir::LocalID {
        let id = ir::LocalID(self.next_local);

        assert!(
            self.local_by_id(id).is_none(),
            "scope must not already have a binding for {}",
            ir::Ref::Local(id),
        );

        self.locals.push(Local::Temp { id });
        self.next_local += 1;

        id
    }

    pub fn bind_new(&mut self, name: Option<Arc<String>>, ty: ir::Type) -> ir::LocalID {
        let id = ir::LocalID(self.next_local);

        assert_ne!(ir::Type::Nothing, ty);

        if let Some(name) = &name {
            if self.named_locals.insert(SharedStringKey(name.clone()), id).is_some() {
                panic!("scope must not already have a binding for {}", name);
            }
        }

        self.locals.push(Local::New {
            id,
            ty: ty.clone(),
        });

        self.next_local += 1;

        id
    }
}

#[derive(Debug)]
pub struct LoopScope {
    pub continue_label: ir::Label,
    pub break_label: ir::Label,

    pub block_level: usize,
}

impl<'m, 'l: 'm> Builder<'m, 'l> {
    
}
