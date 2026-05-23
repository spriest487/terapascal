use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use crate::typ::ast::VARIANT_TAG_TYPE;

pub fn translate_variant_def(
    variant_def: &typ::ast::VariantDecl,
    lib: &mut LibraryBuilder,
) -> ir::VariantDef {
    let tags = lib.translate_tag_groups(&variant_def.tags);

    let name_path = translate_name(&variant_def.name.as_ref().clone().to_generic_name(), lib);

    let tag_type = lib.translate_type(&VARIANT_TAG_TYPE);

    // in Terapascal, the tag is always an Integer for the moment, even though the IR supports
    // any numeric type
    assert_eq!(ir::Type::I32, tag_type);

    let mut cases = Vec::new();
    for (case_index, case) in variant_def.cases.iter().enumerate() {
        let case_ty = match case.data.as_ref() {
            Some(data) => {
                let case_ty = lib.translate_type(&data.ty);

                Some(case_ty)
            },

            None => None
        };

        cases.push(ir::VariantCase {
            name: case.ident.to_string(),
            tag: ir::Value::LiteralI32(case_index as i32),
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
