use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::metadata::translate_decl_name;
use crate::codegen::typ;

pub fn translate_iface(
    id: ir::InterfaceID,
    iface_def: &typ::ast::InterfaceDecl,
    visibility: ir::Visibility,
    lib: &mut LibraryBuilder,
) -> ir::InterfaceDef {
    let tags = lib.translate_tag_groups(&iface_def.tags);
    
    let name = translate_decl_name(&iface_def.name, lib);

    let generic_self_type = id.to_interface_type(name.generic_args());
    
    let mut methods = Vec::with_capacity(iface_def.methods.len());
    
    for def_method in &iface_def.methods {
        let method = ir::InterfaceMethod {
            name: def_method.ident().to_string(),
            result_type: match def_method.decl.result_ty.ty() {
                typ::Type::MethodSelf => generic_self_type.clone(),
                return_ty => lib.translate_type(return_ty),
            },
            params: def_method
                .decl
                .params()
                .map(|(param, item)| {
                    let param_type = match param.ty.ty() {
                        typ::Type::MethodSelf => generic_self_type.clone(),
                        param_ty => lib.translate_type(param_ty),
                    };

                    ir::FunctionParamInfo::new(param_type).with_name(item.name.clone())
                })
                .collect(),

            is_instance_method: def_method.decl.is_instance_method(),
        };

        methods.push(method);
    }

    ir::InterfaceDef::new(name, visibility, methods).with_tags(tags)
}
