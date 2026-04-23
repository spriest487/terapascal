use crate::ir;
use crate::marshal::DynArrayHeader;
use crate::marshal::MarshalError;
use crate::marshal::Marshaller;
use crate::ptr::POINTER_FMT_WIDTH;
use crate::result::ExecResult;
use crate::DynValue;
use crate::ObjectHeader;
use crate::ObjectID;
use crate::ObjectValue;
use crate::Pointer;
use serde::Serialize;
use std::borrow::Cow;
use std::fmt;
use terapascal_ir::MetadataSource as _;
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum NativeHeapError<Ty> {
    #[error(transparent)]
    MarshallingError(MarshalError<Ty>),

    #[error("Null pointer dereference")]
    NullPointerDeref,

    #[error("Freeing value at {0} which wasn't allocated on this heap")]
    BadFree(Pointer),

    #[error("Zero-sized allocation of {count} element(s) of type {ty}")]
    ZeroSizedAllocation {
        ty: Ty,
        count: usize,
    },

    #[error("Memory leak")]
    Leak(Vec<LeakDetails<Ty>>)
}

impl<Ty> NativeHeapError<Ty> {
    pub fn map_types<F, ToTy>(self, f: F) -> NativeHeapError<ToTy>
    where
        F: Fn(Ty) -> ToTy,
    {
        match self {
            NativeHeapError::MarshallingError(err) => {
                NativeHeapError::MarshallingError(err.map_types(f))
            }
            NativeHeapError::NullPointerDeref => {
                NativeHeapError::NullPointerDeref
            }
            NativeHeapError::BadFree(ptr) => {
                NativeHeapError::BadFree(ptr)
            }
            NativeHeapError::ZeroSizedAllocation { ty, count } => {
                NativeHeapError::ZeroSizedAllocation { ty: f(ty), count }
            }
            NativeHeapError::Leak(details) => {
                NativeHeapError::Leak(details.into_iter()
                    .map(|detail| {
                        LeakDetails {
                            alloc_type: f(detail.alloc_type),
                            alloc_count: detail.alloc_count,
                            size: detail.size,
                            addr: detail.addr,
                        }
                    })
                    .collect())
            }
        }
    }
}

impl<Ty: fmt::Display> From<MarshalError<Ty>> for NativeHeapError<Ty> {
    fn from(err: MarshalError<Ty>) -> Self {
        NativeHeapError::MarshallingError(err)
    }
}

impl<Ty: fmt::Display> NativeHeapError<Ty> {
    pub fn notes(&self) -> Vec<String> {
        match self {
            NativeHeapError::Leak(details) => {
                details.iter().map(|d| d.to_string()).collect()
            }
            _ => {
                Vec::new()
            },
        }
    }
}

#[derive(Clone, Debug)]
pub struct LeakDetails<Ty> {
    pub addr: usize,
    pub size: usize,
    pub alloc_type: Ty,
    pub alloc_count: usize,
}

impl<Ty: fmt::Display> fmt::Display for LeakDetails<Ty> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:0width$x}: {}", self.addr, self.alloc_type, width = POINTER_FMT_WIDTH)?;
        if self.alloc_count > 1 {
            write!(f, "[{}]", self.alloc_count)?;
        }

        write!(f, " ({} bytes)", self.size)?;
        Ok(())
    }
}

pub type NativeHeapResult<Res, Ty = ir::Type> = Result<Res, NativeHeapError<Ty>>;

#[derive(Debug)]
struct NativeAlloc {
    addr: usize,
    alloc_type: ir::Type,
    alloc_count: usize,
    memory: Box<[u8]>,
    trace: bool,
}

#[derive(Debug)]
pub struct NativeHeap { 
    pub(super) marshaller: Marshaller,

    allocs: Vec<NativeAlloc>,

    trace_allocs: bool,

    // usage stats for trace output
    stats: HeapStats,
}

impl NativeHeap {
    pub fn new(marshaller: Marshaller, trace_allocs: bool) -> Self {
        Self {
            marshaller,

            trace_allocs,
            allocs: Vec::new(),
            
            stats: HeapStats {
                alloc_count: 0,
                free_count: 0,
                peak_alloc: 0,
            },
        }
    }

    pub fn alloc(&mut self, ty: ir::Type, count: usize, trace: bool) -> NativeHeapResult<Pointer> {
        let ty_size = self.marshaller.get_marshal_type(&ty)?.size();
        if ty_size == 0 || count == 0 {
            return Err(NativeHeapError::ZeroSizedAllocation { ty, count });
        }

        let total_len = ty_size * count;
        let addr = self.alloc_with_len(ty.clone(), count, total_len, trace);

        Ok(Pointer { addr, ty })
    }

    pub fn alloc_object(&mut self, data_size: usize, id: ObjectID, trace: bool) -> NativeHeapResult<Pointer> {
        let header_size = match id {
            ObjectID::Struct(..) | ObjectID::Box(..) => Marshaller::object_header_size(),
            ObjectID::Array(..) => Marshaller::array_header_size(),
        };

        let addr = self.alloc_with_len(id.to_type(&self.marshaller)?, 1, header_size + data_size, trace);

        Ok(Pointer {
            addr,
            ty: ir::Type::Nothing,
        })
    }

    fn alloc_with_len(&mut self,
        alloc_type: ir::Type,
        count: usize,
        total_len: usize,
        trace: bool,
    ) -> usize {
        let alloc_mem = vec![0; total_len].into_boxed_slice();
        let addr = alloc_mem.as_ptr() as usize;

        if trace && self.trace_allocs {
            if trace {
                let ty_name = if count > 1 {
                    Cow::Owned(format!("array[{count}] of {}", self.marshaller.metadata().pretty_type_name(&alloc_type)))
                } else {
                    self.marshaller.metadata().pretty_type_name(&alloc_type)
                };

                eprintln!("[heap] alloc @ 0x{:0width$x} ({ty_name}: {total_len} bytes)", addr, width = POINTER_FMT_WIDTH);
            }

            self.stats.alloc_count += 1;

            self.stats.peak_alloc = usize::max(self.stats.peak_alloc, self.allocs.iter()
                .map(|alloc| alloc.memory.len())
                .sum())
        }

        let alloc = NativeAlloc {
            addr: addr,
            alloc_type,
            alloc_count: count,
            memory: alloc_mem,
            trace,
        };
        
        match self.allocs.binary_search_by_key(&addr, |alloc| alloc.addr) {
            Ok(..) => {
                // if this happens, the previous allocation that resulted in this address must have
                // been freed without properly being removed from the alloc map
                panic!("illegal vm state: bad heap"); 
            }
            
            Err(insert_at) => {
                self.allocs.insert(insert_at, alloc);
            }
        }

        addr
    }

    pub fn free(&mut self, ptr: &Pointer) -> NativeHeapResult<()> {
        let alloc = match self.allocs.binary_search_by_key(&ptr.addr, |alloc| alloc.addr) {
            Ok(index) => self.allocs.remove(index),
            Err(..) => {
                return Err(NativeHeapError::BadFree(ptr.clone()));
            }
        };

        if self.trace_allocs && alloc.trace {
            let count = alloc.alloc_count;
            let len = alloc.memory.len();

            let ty_name = if count > 1 {
                Cow::Owned(format!("array[{count}] of {}", self.marshaller.metadata().pretty_type_name(&alloc.alloc_type)))
            } else {
                self.marshaller.metadata().pretty_type_name(&alloc.alloc_type)
            };

            eprintln!("[heap] free @ 0x{:0width$x} ({ty_name}: {len} bytes)", ptr.addr, width = POINTER_FMT_WIDTH);
            self.stats.free_count += 1;
        }

        Ok(())
    }

    fn validate_pointer(&self, pointer: &Pointer) -> NativeHeapResult<()> {
        if pointer.is_null() {
            return Err(NativeHeapError::NullPointerDeref);
        }

        Ok(())
    }

    pub fn load(&self, pointer: &Pointer) -> NativeHeapResult<DynValue> {
        self.validate_pointer(pointer)?;

        let val = self.marshaller.unmarshal_at(pointer)?;

        Ok(val)
    }

    pub fn load_object(&self, pointer: &Pointer) -> NativeHeapResult<ObjectValue> {
        self.validate_pointer(pointer)?;

        let object_val = self.marshaller.unmarshal_object_at(pointer)?;

        Ok(object_val)
    }

    pub fn load_object_header(&self, pointer: &Pointer) -> NativeHeapResult<ObjectHeader> {
        self.validate_pointer(pointer)?;

        let header = self.marshaller.unmarshal_object_header(unsafe {
            pointer.as_slice(Marshaller::object_header_size())
        })?;

        Ok(header.value)
    }

    pub fn store_object_header(&self, header: &ObjectHeader, pointer: &Pointer) -> ExecResult<()> {
        self.validate_pointer(pointer)?;

        self.marshaller.marshal_object_header(header, unsafe {
            pointer.as_slice_mut(Marshaller::object_header_size())
        })?;

        Ok(())
    }

    pub fn load_array_header(&self, pointer: &Pointer) -> NativeHeapResult<DynArrayHeader> {
        self.validate_pointer(pointer)?;

        let header = self.marshaller.unmarshal_dyn_array_header_at(pointer)?;

        Ok(header)
    }

    pub fn store(&mut self, pointer: &Pointer, val: DynValue) -> NativeHeapResult<()> {
        self.validate_pointer(pointer)?;

        self.marshaller.marshal_at(&val, pointer)?;

        Ok(())
    }
    
    pub fn print_trace(&self) {
        eprintln!("[heap] alloc count = {}", self.stats.alloc_count);
        eprintln!("[heap] free count = {}", self.stats.free_count);
        eprintln!("[heap] peak alloc size = {}", self.stats.peak_alloc);
        
        for alloc in &self.allocs {
            if !alloc.trace {
                continue;
            }

            eprintln!("[heap] remaining alloc @ 0x{:0width$x}", alloc.addr, width=POINTER_FMT_WIDTH);
        } 
    }
    
    pub fn stats(&self) -> HeapStats {
        self.stats
    }

    pub fn check_leaks(&self) -> NativeHeapResult<()> {
        if !self.allocs.iter().any(|a| a.trace) {
            return Ok(());
        }

        Err(NativeHeapError::Leak(self.allocs
            .iter()
            .filter(|alloc| alloc.trace)
            .map(|alloc| {
                LeakDetails {
                    addr: alloc.addr,
                    alloc_type: alloc.alloc_type.clone(),
                    alloc_count: alloc.alloc_count,
                    size: alloc.memory.len(),
                }
            })
            .collect()))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub struct HeapStats {
    pub alloc_count: usize,
    pub free_count: usize,
    pub peak_alloc: usize,
}
