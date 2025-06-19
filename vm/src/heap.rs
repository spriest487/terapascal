use crate::ir;
use crate::marshal::MarshalError;
use crate::marshal::Marshaller;
use crate::ptr::POINTER_FMT_WIDTH;
use crate::DynValue;
use crate::Pointer;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fmt;
use std::mem::forget;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum NativeHeapError {
    MarshallingError(MarshalError),
    NullPointerDeref,
    BadFree(Pointer),
    ZeroSizedAllocation {
        ty: ir::Type,
        count: usize,
    },
}

impl fmt::Display for NativeHeapError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NativeHeapError::MarshallingError(err) => write!(f, "{}", err),
            NativeHeapError::NullPointerDeref => write!(f, "null pointer dereference"),
            NativeHeapError::BadFree(ptr) => write!(f, "freeing value at {} which wasn't allocated on this heap", ptr),
            NativeHeapError::ZeroSizedAllocation { ty, count } => write!(f, "zero-sized allocation of {} elements of type {}", count, ty),
        }
    }
}

impl From<MarshalError> for NativeHeapError {
    fn from(err: MarshalError) -> Self {
        NativeHeapError::MarshallingError(err)
    }
}

pub type NativeHeapResult<T> = Result<T, NativeHeapError>;

#[derive(Debug)]
pub struct NativeHeap {
    marshaller: Rc<Marshaller>,
    metadata: Rc<ir::Metadata>,

    allocs: BTreeMap<usize, Box<[u8]>>,

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

        let alloc_mem = vec![0; total_len];
        let addr = alloc_mem.as_ptr() as usize;

        if self.allocs.insert(addr, alloc_mem.into_boxed_slice()).is_some() {
            unreachable!(); 
        }

        if self.trace_allocs {
            let ty_name = self.metadata.pretty_ty_name(&ty);
            eprintln!("[heap] alloc {} bytes @ 0x{:0width$x} ({})", total_len, addr, ty_name, width=POINTER_FMT_WIDTH);
            self.stats.alloc_count += 1;
            
            self.stats.peak_alloc = usize::max(self.stats.peak_alloc, self.allocs.iter()
                .map(|(_, mem)| mem.len())
                .sum())
        }
        

        Ok(Pointer { addr, ty })
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
        
        forget(mem);
        
        if self.trace_allocs {
            let ty_name = self.metadata.pretty_ty_name(&ptr.ty);
            eprintln!("[heap] forget @ 0x{:0width$x} ({ty_name})", ptr.addr, width=POINTER_FMT_WIDTH);
            self.stats.free_count += 1;
        }

        Ok(())
    }

    pub fn load(&self, addr: &Pointer) -> NativeHeapResult<DynValue> {
        if addr.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        let val = self.marshaller.unmarshal_from_ptr(addr)?;

        Ok(val)
    }

    pub fn store(&mut self, addr: &Pointer, val: DynValue) -> NativeHeapResult<()> {
        if addr.addr == 0 {
            return Err(NativeHeapError::NullPointerDeref);
        }

        self.marshaller.marshal_into(&val, addr)?;

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
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize)]
pub struct HeapStats {
    pub alloc_count: usize,
    pub free_count: usize,
    pub peak_alloc: usize,
}
