use crate::ir;
use crate::marshal::MarshalError;
use crate::marshal::Marshaller;
use crate::ptr::POINTER_FMT_WIDTH;
use crate::DynValue;
use crate::ObjectID;
use crate::ObjectValue;
use crate::Pointer;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fmt;
use std::mem::forget;
use std::rc::Rc;
use terapascal_ir::MetadataSource;
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
    alloc_type: ir::Type,
    alloc_count: usize,
    memory: Box<[u8]>,
}

#[derive(Debug)]
pub struct NativeHeap {
    marshaller: Rc<Marshaller>,
    metadata: Rc<ir::Metadata>,

    allocs: BTreeMap<usize, NativeAlloc>,

    trace_allocs: bool,

    // usage stats for trace output
    stats: HeapStats,
}

impl NativeHeap {
    pub fn new(metadata: Rc<ir::Metadata>, marshaller: Rc<Marshaller>, trace_allocs: bool) -> Self {
        Self {
            metadata,
            marshaller,
            trace_allocs,
            allocs: BTreeMap::new(),
            
            stats: HeapStats {
                alloc_count: 0,
                free_count: 0,
                peak_alloc: 0,
            },
        }
    }

    pub fn set_metadata(&mut self, metadata: Rc<ir::Metadata>, marshaller: Rc<Marshaller>) {
        self.metadata = metadata;
        self.marshaller = marshaller;
    }

    pub fn alloc(&mut self, ty: ir::Type, count: usize) -> NativeHeapResult<Pointer> {
        let ty_size = self.marshaller.get_ty(&ty)?.size();
        if ty_size == 0 || count == 0 {
            return Err(NativeHeapError::ZeroSizedAllocation { ty, count });
        }

        let total_len = ty_size * count;
        let addr = self.alloc_with_len(ty.clone(), count, total_len);

        Ok(Pointer { addr, ty })
    }

    pub fn alloc_object(&mut self, data_size: usize, id: ObjectID) -> NativeHeapResult<Pointer> {
        let header_size = match id {
            ObjectID::Class(..) | ObjectID::Box(..) => Marshaller::object_header_size(),
            ObjectID::Array(..) => Marshaller::array_header_size(),
        };

        let addr = self.alloc_with_len(id.to_type(), 1, header_size + data_size);

        Ok(Pointer {
            addr,
            ty: ir::Type::Nothing,
        })
    }

    fn alloc_with_len(&mut self, alloc_type: ir::Type, count: usize, total_len: usize) -> usize {
        let alloc_mem = vec![0; total_len].into_boxed_slice();
        let addr = alloc_mem.as_ptr() as usize;

        if self.trace_allocs {
            let ty_name = self.metadata.pretty_ty_name(&alloc_type);
            eprintln!("[heap] alloc {} bytes @ 0x{:0width$x} ({})", total_len, addr, ty_name, width=POINTER_FMT_WIDTH);
            self.stats.alloc_count += 1;

            self.stats.peak_alloc = usize::max(self.stats.peak_alloc, self.allocs.iter()
                .map(|(_, alloc)| alloc.memory.len())
                .sum())
        }

        let alloc = NativeAlloc {
            alloc_type,
            alloc_count: count,
            memory: alloc_mem,
        };

        if self.allocs.insert(addr, alloc).is_some() {
            unreachable!();
        }

        addr
    }

    pub fn free(&mut self, ptr: &Pointer) -> NativeHeapResult<()> {
        if self.allocs.remove(&ptr.addr).is_none() {
            return Err(NativeHeapError::BadFree(ptr.clone()));
        }

        if self.trace_allocs {
            let ty_name = self.metadata.pretty_ty_name(&ptr.ty);
            eprintln!("[heap] free @ 0x{:0width$x} ({ty_name})", ptr.addr, width=POINTER_FMT_WIDTH);
            self.stats.free_count += 1;
        }

        Ok(())
    }
    
    /// remove an allocation from tracking, indicating that it will never be freed
    /// e.g. used for immortal data like TypeInfo and string literals.
    pub fn forget(&mut self, ptr: &Pointer) -> NativeHeapResult<()> {
        let Some(mem) = self.allocs.remove(&ptr.addr) else {
            return Err(NativeHeapError::BadFree(ptr.clone()));
        };

        forget(mem.memory);
        
        if self.trace_allocs {
            let ty_name = self.metadata.pretty_ty_name(&ptr.ty);
            eprintln!("[heap] forget @ 0x{:0width$x} ({ty_name})", ptr.addr, width=POINTER_FMT_WIDTH);
            self.stats.free_count += 1;
        }

        Ok(())
    }

    pub fn load(&self, pointer: &Pointer) -> NativeHeapResult<DynValue> {
        if pointer.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        let val = self.marshaller.unmarshal_at(pointer)?;

        Ok(val)
    }

    pub fn load_object(&self, pointer: &Pointer) -> NativeHeapResult<ObjectValue> {
        if pointer.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        let object_val = self.marshaller.unmarshal_object_at(pointer)?;

        Ok(object_val)
    }

    pub fn store(&mut self, pointer: &Pointer, val: DynValue) -> NativeHeapResult<()> {
        if pointer.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        self.marshaller.marshal_at(&val, pointer)?;

        Ok(())
    }
    
    pub fn print_trace(&self) {
        eprintln!("[heap] alloc count = {}", self.stats.alloc_count);
        eprintln!("[heap] free count = {}", self.stats.free_count);
        eprintln!("[heap] peak alloc size = {}", self.stats.peak_alloc);
        
        for addr in self.allocs.keys() {
            eprintln!("[heap] remaining alloc @ 0x{:0width$x}", addr, width=POINTER_FMT_WIDTH);
        } 
    }
    
    pub fn stats(&self) -> HeapStats {
        self.stats
    }

    pub fn check_leaks(&self) -> NativeHeapResult<()> {
        if self.allocs.is_empty() {
            return Ok(());
        }

        Err(NativeHeapError::Leak(self.allocs
            .iter()
            .map(|(addr, alloc)| {
                LeakDetails {
                    addr: *addr,
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
