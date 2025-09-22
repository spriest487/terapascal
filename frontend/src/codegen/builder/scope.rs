use terapascal_ir as ir;
use crate::codegen::builder::Builder;

#[derive(Clone, Debug)]
pub enum Local {
    // the builder created this local allocation and must track its lifetime to drop it
    New {
        id: ir::LocalID,
        name: Option<String>,
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
        name: String,
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

    pub fn name(&self) -> Option<&String> {
        match self {
            Local::New { name, .. } => name.as_ref(),
            Local::Param { name, .. } => Some(&name),
            Local::Temp { .. } | Local::Return { .. } => None,
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
pub(super) struct Scope {
    locals: Vec<Local>,

    // debug context pushes belonging to this scope - when cleaning up this scope, any un-popped
    // entries in the debug context stack that were pushed during this scope also need to be popped
    debug_ctx_depth: usize,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
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

    pub fn locals(&self) -> &[Local] {
        &self.locals
    }

    pub fn local_by_id(&self, id: ir::LocalID) -> Option<&Local> {
        self.locals().iter().find(|l| l.id() == id)
    }

    pub fn local_by_name(&self, name: &str) -> Option<&Local> {
        self.locals().iter().find(|l| l.name().map(String::as_str) == Some(name))
    }

    pub fn bind_param(&mut self, id: ir::LocalID, name: impl Into<String>, ty: ir::Type, by_ref: bool) {
        let name = name.into();

        if by_ref {
            let is_ptr = match &ty {
                ir::Type::Pointer(..) => true,
                _ => false,
            };
            assert!(is_ptr, "by-ref parameters must have pointer type");
        }

        assert!(
            self.local_by_id(id).is_none(),
            "scope must not already have a binding for {}: {:?}",
            ir::Ref::Local(id),
            self
        );
        assert!(
            self.local_by_name(&name).is_none(),
            "scope must not already have a binding named {}: {:?}",
            name,
            self
        );

        self.locals.push(Local::Param { id, name, ty, by_ref });
    }

    pub fn bind_return(&mut self) {
        let slot_free = self.local_by_id(ir::LocalID(0)).is_none();
        assert!(slot_free, "%0 must not already be bound in bind_return");

        self.locals.push(Local::Return);
    }

    pub fn bind_temp(&mut self, id: ir::LocalID) {
        assert!(
            self.local_by_id(id).is_none(),
            "scope must not already have a binding for {}: {:?}",
            ir::Ref::Local(id),
            self
        );

        self.locals.push(Local::Temp { id });
    }

    pub fn bind_new(&mut self, id: ir::LocalID, name: Option<String>, ty: ir::Type) {
        assert_ne!(ir::Type::Nothing, ty);

        self.locals.push(Local::New {
            id,
            name,
            ty: ty.clone(),
        })
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
