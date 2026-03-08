mod trace;

pub use self::trace::*;
use crate::ir;
use crate::marshal::MarshalError;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::DynValue;
use crate::Pointer;
use std::borrow::Cow;
use std::convert::TryInto;
use std::fmt;
use std::mem::size_of;
use std::rc::Rc;
use terapascal_common::span::Span;
use thiserror::Error;

const SENTINEL: usize = 12345678;

#[derive(Debug)]
struct StackAlloc {
    // local vals are allocated on the fly as the vm passes LocalAlloc
    // instructions. we want to prevent IR code from alloc-ing two locals with the same
    // ID, but we might also legally run the same alloc instruction more than once if the
    // control flow takes us back over it.
    // therefore we need to remember where a local allocation was made,
    // so the duplicate check can tell whether it's two allocations with the same ID
    // function param allocs don't have an alloc location
    alloc_pc: Option<usize>,

    ty: ir::Type,

    stack_offset: usize,
}

#[derive(Debug)]
pub(super) struct StackFrame {
    name: Rc<String>,

    stack_mem: Box<[u8]>,
    stack_offset: usize,

    result: Option<StackAlloc>,
    args: Vec<StackAlloc>,

    locals: Vec<StackAlloc>,

    marshaller: Rc<Marshaller>,

    debug_ctx_stack: Vec<Span>,
}

impl StackFrame {
    pub fn new(
        name: Rc<String>,
        marshaller: Rc<Marshaller>,
        stack_size: usize,
    ) -> Self {
        let sentinel_size = size_of::<usize>();
        let mut stack_mem = vec![0; stack_size + sentinel_size];
        stack_mem[stack_size..].copy_from_slice(&SENTINEL.to_ne_bytes());

        Self {
            name: name.into(),

            result: None,
            args: Vec::new(),

            locals: Vec::new(),
            
            debug_ctx_stack: Vec::new(),

            marshaller,

            stack_mem: stack_mem.into_boxed_slice(),
            stack_offset: 0,
        }
    }

    /// Reserves stack space for the result value to be stored
    pub fn declare_result(&mut self, ty: ir::Type, value: &DynValue) -> MarshalResult<()> {
        assert!(self.result.is_none(), "result storage must not be allocated twice");
        
        let stack_offset = self.stack_alloc(&ty, value)?;

        self.result = Some(StackAlloc {
            alloc_pc: None,
            ty,
            stack_offset,
        });

        Ok(())
    }

    /// Reserves stack space for an argument value to be stored
    pub fn declare_arg(&mut self, ty: ir::Type, value: &DynValue) -> MarshalResult<ir::ArgID> {
        let stack_offset = self.stack_alloc(&ty, value)?;

        self.args.push(StackAlloc {
            alloc_pc: None,
            ty,
            stack_offset,
        });

        let id = ir::ArgID(self.args.len() - 1);
        Ok(id)
    }

    pub fn declare_local(&mut self, id: ir::LocalID, ty: ir::Type, value: &DynValue, alloc_pc: usize) -> StackResult<()> {
        // we only need to allocate new variables the first time the block is executed, so if
        // we try to allocate twice from the same instruction, just do nothing
        // todo: this could be cleaned up by allocating everything at the start of the block
        // instead of doing it as we encounter new locals
        for (existing_id, local) in self.locals.iter().enumerate() {
            if existing_id == id.0 {
                if local.alloc_pc != Some(alloc_pc) {
                    return Err(StackError::DuplicateLocalAlloc {
                        stack_frame: (*self.name).clone(),
                        id,
                        first_pc: local.alloc_pc,
                        next_pc: alloc_pc,
                    });
                }

                // we are encountering an alloc expr for a previous allocation by the same
                // instruction, it must be identical and we can skip it
                return Ok(());
            }
        }

        // we're about to make this allocation for the first time, so the expected ID should be
        // the next index in the stack
        if self.locals.len() != id.0 {
            return Err(StackError::IllegalAlloc(id));
        }

        let stack_offset = self.stack_alloc(&ty, value)?;

        self.locals.push(StackAlloc {
            alloc_pc: Some(alloc_pc),
            ty,
            stack_offset,
        });

        Ok(())
    }

    pub fn check_sentinel(&self) -> StackResult<()> {
        let sentinel_bytes = self.stack_mem[self.stack_mem.len() - size_of::<usize>()..]
            .try_into()
            .unwrap();

        let sentinel = usize::from_ne_bytes(sentinel_bytes);

        if sentinel == SENTINEL {
            Ok(())
        } else {
            Err(StackError::BadSentinel(sentinel))
        }
    }

    fn stack_alloc(&mut self, ty: &ir::Type, value: &DynValue) -> MarshalResult<usize> {
        let start_offset = self.stack_offset;
        let alloc_slice = &mut self.stack_mem[start_offset..];
        let size = self.marshaller.marshal(value, alloc_slice)?;
        
        self.stack_offset += size;

        if cfg!(debug_assertions) {
            let marshalled_size = self.stack_offset - start_offset;
            let ty_size = self.marshaller.get_ty(ty)?.size();
            assert_eq!(marshalled_size, ty_size, "stack space allocated ({}) did not match expected size {} for type {}", marshalled_size, ty_size, ty);
        }

        Ok(start_offset)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn debug_location(&self) -> Cow<'_, Span> {
        self.debug_ctx_stack.last()
            .map(|span| Cow::Borrowed(span))
            .unwrap_or_else(|| Cow::Owned(Span::zero("<unknown>")))
    }

    pub fn get_result_ptr(&self) -> StackResult<Pointer> {
        match &self.result {
            Some(alloc) => Ok(self.stack_pointer(alloc)),
            None => Err(StackError::ResultNotAllocated),
        }
    }

    pub fn get_arg_ptr(&self, id: ir::ArgID) -> StackResult<Pointer> {
        match self.args.get(id.0) {
            Some(alloc) => {
                Ok(self.stack_pointer(alloc))
            },
            None => {
                Err(StackError::ArgNotAllocated(id))
            },
        }
    }

    pub fn get_local_ptr(&self, id: ir::LocalID) -> StackResult<Pointer> {
        match self.locals.get(id.0) {
            Some(alloc) => {
                Ok(self.stack_pointer(alloc))
            },
            None => {
                Err(StackError::LocalNotAllocated(id))
            },
        }
    }
    
    fn stack_pointer(&self, alloc: &StackAlloc) -> Pointer {
        let stack_mem_addr = self.stack_mem.as_ptr() as usize;
        let addr = stack_mem_addr + alloc.stack_offset;

        Pointer::new(addr, alloc.ty.clone())
    }
    
    pub fn debug_push(&mut self, ctx: Span) {
        self.debug_ctx_stack.push(ctx);
    }
    
    pub fn debug_pop(&mut self) {
        if !self.debug_ctx_stack.pop().is_some() {
            eprintln!("vm: unbalanced debug context instructions, ignoring pop on empty stack")
        }
    }
    
    pub fn debug_depth(&self) -> usize {
        self.debug_ctx_stack.len()
    }
}

#[derive(Debug, Clone, Error)]
pub enum StackError<Ty: fmt::Display = ir::Type> {
    ResultNotAllocated,
    ArgNotAllocated(ir::ArgID),
    LocalNotAllocated(ir::LocalID),
    DuplicateLocalAlloc {
        stack_frame: String,
        id: ir::LocalID,
        first_pc: Option<usize>,
        next_pc: usize,
    },
    IllegalJmp {
        current_block: usize,
        dest_block: usize,
    },
    IllegalAlloc(ir::LocalID),
    MarshalError(MarshalError<Ty>),
    BadSentinel(usize),
}

impl<Ty: fmt::Display> From<MarshalError<Ty>> for StackError<Ty> {
    fn from(err: MarshalError<Ty>) -> Self {
        StackError::MarshalError(err)
    }
}

impl<Ty: fmt::Display> fmt::Display for StackError<Ty> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StackError::ResultNotAllocated => {
                write!(f, "storage not allocated for result value")
            }
            StackError::ArgNotAllocated(id) => {
                write!(f, "storage not allocated for arg: {}", id)
            }
            StackError::LocalNotAllocated(id) => {
                write!(f, "storage not allocated for local: {}", id)
            }
            StackError::DuplicateLocalAlloc { id, first_pc, next_pc, stack_frame } => {
                write!(f, "{}: local {} was reallocated by a separate instruction (", stack_frame, id)?;

                match first_pc {
                    Some(first_pc) => write!(f, "first alloc @ instruction {}, next alloc @ instruction {}", first_pc, next_pc)?,
                    None => write!(f, " (reallocation @ instruction {})", next_pc)?,
                }

                write!(f, ")")
            }
            StackError::IllegalJmp { current_block, dest_block } => {
                write!(f, "illegal jump from block {} to block {}", current_block, dest_block)
            }
            StackError::IllegalAlloc(id) => {
                write!(f, "allocation of local val {} is not legal here", id)
            }
            StackError::MarshalError(err) => {
                write!(f, "{}", err)
            }
            StackError::BadSentinel(sentinel) => {
                write!(f, "bad sentinel value: {}", sentinel)
            }
        }
    }
}

pub type StackResult<T, Ty = ir::Type> = Result<T, StackError<Ty>>;
