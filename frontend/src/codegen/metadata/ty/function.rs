use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::typ;
use crate::codegen::FunctionInstance;
use crate::ir;
use ir::StructLayout;
use linked_hash_map::LinkedHashMap;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;
use terapascal_common::ident::Ident;
use terapascal_ir::Visibility;

#[derive(Debug, Clone)]
pub struct ClosureInstance {
    // the function containing the closure's body
    pub func_instance: FunctionInstance,
    
    // signature of the virtual function type (no closure pointer param)
    pub sig: Rc<ir::FunctionSig>,
    
    // ID of the implementation struct type of this closure
    pub closure_id: ir::TypeDefID,
}

impl ClosureInstance {
    pub fn function_pointer_type(&self) -> ir::Type {
        self.sig.to_closure_ptr_type()
    }

    pub fn closure_class_type(&self) -> ir::Type {
        self.closure_id.to_class_ptr_type([])
    }
}

impl fmt::Display for ClosureInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "closure {} of {} ({})",
            self.closure_id, self.sig, self.func_instance.src_sig
        )
    }
}

pub fn translate_closure_struct(
    identity: ir::ClosureIdentity,
    captures: &LinkedHashMap<Ident, typ::Type>,
    lib: &mut LibraryBuilder,
) -> ir::TypeDefID {
    let id = lib.metadata_mut().new_type();

    // the function pointer needs the real type, which takes the type-erased self arg as
    // an extra 0th parameter
    let func_ptr_sig = identity.sig.to_closure_function_ptr_sig();

    let mut fields = BTreeMap::new();
    fields.insert(
        ir::CLOSURE_PTR_FIELD,
        ir::StructFieldDef::new(func_ptr_sig.into_function_type()),
    );

    let mut field_id = ir::FieldID(ir::CLOSURE_PTR_FIELD.0 + 1);

    for (capture_name, capture_ty) in captures {
        let ty = lib.translate_type(capture_ty);

        fields.insert(
            field_id,
            ir::StructFieldDef::new(ty).with_name((*capture_name.name).clone()),
        );

        field_id.0 += 1;
    }

    let struct_identity = ir::StructIdentity::ClosureObject(identity);
    let struct_def = ir::StructDef::new(struct_identity, Visibility::Internal, StructLayout::Default)
        .with_fields(fields);

    lib.metadata_mut().define_closure_ty(id, struct_def);

    id
}

pub fn translate_sig(sig: &typ::FunctionSig, lib: &mut LibraryBuilder) -> ir::FunctionSig {
    let return_ty = lib.translate_type(&sig.result_ty);
    let mut param_tys = Vec::new();
    for param in &sig.params {
        let mut ty = lib.translate_type(&param.ty);
        if param.is_by_ref() {
            ty = ty.temp_ref();
        }

        param_tys.push(ty);
    }

    ir::FunctionSig {
        result_type: return_ty,
        param_types: param_tys,
    }
}
