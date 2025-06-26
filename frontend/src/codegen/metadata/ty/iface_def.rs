use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use crate::codegen::ir;

pub fn translate_iface(
    iface_def: &typ::ast::InterfaceDecl,
    generic_ctx: &typ::GenericContext,
    lib: &mut LibraryBuilder,
) -> ir::Interface {
    let name = translate_name(&iface_def.name, generic_ctx, lib);

    // it needs to be declared to reference its own ID in the Self type
    let id = lib.metadata_mut().declare_iface(&name);

    let methods: Vec<_> = iface_def
        .methods
        .iter()
        .map(|method| {
            let self_ty = ir::Type::RcPointer(ir::VirtualTypeID::Interface(id));

            ir::Method {
                name: method.ident().to_string(),
                return_ty: match method.decl.result_ty.ty() {
                    typ::Type::MethodSelf => self_ty.clone(),
                    return_ty => lib.translate_type(return_ty, generic_ctx),
                },
                params: method
                    .decl
                    .params()
                    .map(|(param, _)| match param.ty.ty() {
                        typ::Type::MethodSelf => self_ty.clone(),
                        param_ty => lib.translate_type(param_ty, generic_ctx),
                    })
                    .collect(),
            }
        })
        .collect();

    ir::Interface::new(name, methods)
}
