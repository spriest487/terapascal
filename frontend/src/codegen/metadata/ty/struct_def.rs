use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use crate::{ast, ir};
use std::collections::HashMap;
use typ::layout::StructLayoutMember;

pub fn translate_struct_def(
    struct_def: &typ::ast::StructDecl,
    generic_ctx: &typ::GenericContext,
    lib: &mut LibraryBuilder,
) -> ir::Struct {    
    let name_path = translate_name(&struct_def.name, generic_ctx, lib);

    let mut fields = HashMap::new();
    let mut pad_run = 0;
    let mut next_id = ir::FieldID(0);
    for member in lib.aligned_struct_members(struct_def) {
        match member {
            StructLayoutMember::Data { field_decl: member, decl_index, .. } => {
                if pad_run > 0 {
                    fields.insert(next_id, ir::StructFieldDef {
                        name: None,
                        rc: false,
                        ty: ir::Type::U8.array(pad_run),
                    });
                    pad_run = 0;
                    next_id.0 += 1;
                }

                let name = member.idents[decl_index].to_string();
                let ty = lib.translate_type(&member.ty, generic_ctx);
                let rc = member.ty.is_strong_rc_reference();
                fields.insert(next_id, ir::StructFieldDef { name: Some(name), ty, rc });
                next_id.0 += 1;
            }

            StructLayoutMember::PaddingByte => {
                pad_run += 1;
            }
        }
    }

    if pad_run > 0 {
        let pad_ty = ir::Type::U8.array(pad_run);
        fields.insert(next_id, ir::StructFieldDef { name: None, rc: false, ty: pad_ty });
    }

    let identity = match struct_def.kind {
        ast::StructKind::Class => ir::StructIdentity::Class(name_path),
        ast::StructKind::Record => ir::StructIdentity::Record(name_path),
    };

    ir::Struct::new(identity).with_fields(fields)
}
