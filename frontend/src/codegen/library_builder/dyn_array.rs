use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;

pub fn gen_dyn_array_runtime_type(
    lib: &mut LibraryBuilder,
    elem_type: &ir::Type,
) {
    let array_ref_ty = elem_type.dyn_array();

    let weak_ty = ir::Type::RcWeakPointer(ir::VirtualTypeID::Class(array_class_id));
    lib.gen_runtime_type(&weak_ty);
}
