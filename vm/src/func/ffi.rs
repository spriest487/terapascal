use crate::ir;
use crate::marshal::ForeignType;
use crate::ExecResult;
use crate::Interpreter;
use ::libffi::middle::Cif;
use ::libffi::raw::ffi_call as ffi_raw_call;
use smallvec::*;
use std::ffi::c_void;
use std::ptr::null_mut;
use std::ptr::slice_from_raw_parts;

pub struct FfiInvoker {
    cif: Cif,

    symbol: *const (),

    ffi_param_tys: Vec<ForeignType>,

    return_ty: ir::Type,
    ffi_return_ty: ForeignType,
}

impl FfiInvoker {
    pub fn new(
        cif: Cif,
        symbol: *const (),
        ffi_param_tys: Vec<ForeignType>,
        return_ty: ir::Type,
        ffi_return_ty: ForeignType,
    ) -> Self {
        Self {
            cif,
            symbol,

            ffi_param_tys,

            return_ty,
            ffi_return_ty,
        }
    }

    pub fn invoke(&self, state: &mut Interpreter) -> ExecResult<()> {
        // marshal args into a byte vec - we need to pass pointers into this vec, so it can't
        // be reallocated, and we need to calculate the total size now
        let params_total_size = self.ffi_param_tys.iter().map(|p| p.size()).sum();
        let param_count = self.ffi_param_tys.len();

        let mut args: SmallVec<[u8; 64]> = smallvec![0; params_total_size];
        let mut args_ptrs: SmallVec<[*mut c_void; 4]> = smallvec![null_mut(); param_count];

        let first_param_local = match self.return_ty {
            ir::Type::Nothing => 0,
            _ => 1,
        };

        let mut arg_offset = 0;

        for i in 0..param_count {
            let local_id = ir::LocalID(first_param_local + i);

            let param_size = self.ffi_param_tys[i].size();

            let arg_val = state.load(&ir::Ref::Local(local_id))?;
            let bytes_copied = state
                .marshaller()
                .marshal(&arg_val, &mut args[arg_offset..])?;

            assert_eq!(bytes_copied, param_size);

            args_ptrs[i] =
                unsafe { (args.as_mut_ptr() as *mut c_void).offset(arg_offset as isize) };
            arg_offset += param_size;
        }

        let mut result_buf: Vec<u8> = if self.return_ty != ir::Type::Nothing {
            vec![0; self.ffi_return_ty.size()]
        } else {
            Vec::new()
        };

        unsafe {
            let fn_addr: extern "C" fn() = std::mem::transmute(self.symbol);

            ffi_raw_call(
                &self.cif as *const Cif as _,
                Some(fn_addr),
                result_buf.as_mut_ptr() as _,
                args_ptrs.as_mut_ptr(),
            );
        }

        if self.return_ty != ir::Type::Nothing {
            unsafe {
                let result_slice = slice_from_raw_parts(result_buf.as_ptr(), result_buf.len());
                let return_val = state
                    .marshaller()
                    .unmarshal(result_slice.as_ref().unwrap(), &self.return_ty)?;

                let return_local = ir::Ref::Local(ir::LocalID(0));
                state.store(&return_local, return_val.value)?;
            }
        }

        Ok(())
    }
}
