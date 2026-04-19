use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use crate::codegen::ir;

pub fn translate_iface(
    iface_def: &typ::ast::InterfaceDecl,
    lib: &mut LibraryBuilder,
) -> ir::InterfaceDef {
    let tags = lib.translate_tag_groups(&iface_def.tags);
    
    let name = translate_name(&iface_def.name, lib);

    // it needs to be declared to reference its own ID in the Self type
    let id = lib.metadata_mut().declare_iface(&name);
    
    let mut methods = Vec::with_capacity(iface_def.methods.len());
    
    for def_method in &iface_def.methods {
        let self_ty = ir::Type::Object(ir::ObjectID::Interface(id));

        let method = ir::Method {
            name: def_method.ident().to_string(),
            return_ty: match def_method.decl.result_ty.ty() {
                typ::Type::MethodSelf => self_ty.clone(),
                return_ty => lib.translate_type(return_ty),
            },
            params: def_method
                .decl
                .params()
                .map(|(param, _)| match param.ty.ty() {
                    typ::Type::MethodSelf => self_ty.clone(),
                    param_ty => lib.translate_type(param_ty),
                })
                .collect(),
        };
        
        methods.push(method);
    }

    ir::InterfaceDef::new(name, methods)
        .with_tags(tags)
}
