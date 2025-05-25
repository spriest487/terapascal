use crate::ir;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::syn;
use crate::codegen::typ;
use crate::codegen::FunctionInstance;
use linked_hash_map::LinkedHashMap;
use std::fmt;
use syn::Ident;

#[derive(Debug, Clone)]
pub struct ClosureInstance {
    // the function containing the closure's body
    pub func_instance: FunctionInstance,
    
    // ID of the function type (not the closure function but the target type)
    pub func_ty_id: ir::TypeDefID,
    
    // ID of the implementation struct type of this closure
    pub closure_id: ir::TypeDefID,
}

impl ClosureInstance {
    pub fn closure_ptr_ty(&self) -> ir::Type {
        ir::Type::RcPointer(ir::VirtualTypeID::Closure(self.func_ty_id))
    }
}

impl fmt::Display for ClosureInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "closure {} of {} ({})",
            self.closure_id, self.func_ty_id, self.func_instance.sig
        )
    }
}

pub fn translate_closure_struct(
    identity: ir::ClosureIdentity,
    captures: &LinkedHashMap<Ident, typ::Type>,
    generic_ctx: &typ::GenericContext,
    lib: &mut LibraryBuilder,
) -> ir::TypeDefID {
    let id = lib.metadata_mut().reserve_new_struct();

    let mut fields = LinkedHashMap::new();
    fields.insert(
        ir::CLOSURE_PTR_FIELD,
        ir::StructFieldDef {
            name: None,
            rc: false,
            ty: ir::Type::Function(identity.virt_func_ty),
        },
    );

    let mut field_id = ir::FieldID(ir::CLOSURE_PTR_FIELD.0 + 1);

    for (capture_name, capture_ty) in captures {
        let ty = lib.translate_type(capture_ty, generic_ctx);

        fields.insert(
            field_id,
            ir::StructFieldDef {
                name: Some((*capture_name.name).clone()),
                ty,
                rc: capture_ty.is_strong_rc_reference(),
            },
        );

        field_id.0 += 1;
    }

    lib.metadata_mut().define_closure_ty(
        id,
        ir::Struct {
            identity: ir::StructIdentity::Closure(identity),
            fields,
        },
    );

    id
}

pub fn translate_sig(
    sig: &typ::FunctionSig,
    generic_ctx: &typ::GenericContext,
    lib: &mut LibraryBuilder,
) -> ir::FunctionSig {
    assert!(
        sig.type_params.is_none(),
        "cannot create type for a generic function pointer"
    );

    let return_ty = lib.translate_type(&sig.return_ty, generic_ctx);
    let mut param_tys = Vec::new();
    for param in &sig.params {
        let mut ty = lib.translate_type(&param.ty, generic_ctx);
        if param.is_by_ref() {
            ty = ty.ptr();
        }

        param_tys.push(ty);
    }

    ir::FunctionSig {
        return_ty,
        param_tys,
    }
}
