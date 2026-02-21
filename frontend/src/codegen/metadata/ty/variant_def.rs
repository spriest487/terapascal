use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use crate::typ::ast::VARIANT_TAG_TYPE;

pub fn translate_variant_def(
    variant_def: &typ::ast::VariantDecl,
    generic_ctx: &typ::GenericContext,
    lib: &mut LibraryBuilder,
) -> ir::VariantDef {
    let tags = lib.translate_tag_groups(&variant_def.tags);
    
    let name_path = translate_name(&variant_def.name, generic_ctx, lib);
    
    let tag_type = lib.translate_type(&VARIANT_TAG_TYPE, generic_ctx);

    let mut cases = Vec::new();
    for case in &variant_def.cases {
        let case_ty = match case.data.as_ref() {
            Some(data) => {
                let case_ty = lib.translate_type(&data.ty, generic_ctx);
                Some(case_ty)
            },

            None => None
        };

        cases.push(ir::VariantCase {
            name: case.ident.to_string(),
            ty: case_ty,
        });
    }

    ir::VariantDef {
        name: name_path,
        tag_type,
        tags,
        cases,
    }
}
