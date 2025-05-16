use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::syn;
use crate::codegen::translate_name;
use crate::codegen::typ;
use ir_lang::*;
use std::collections::HashMap;
use syn::StructKind;
use typ::layout::StructLayoutMember;

pub fn translate_struct_def(
    struct_def: &typ::ast::StructDef,
    generic_ctx: &typ::GenericContext,
    lib: &mut LibraryBuilder,
) -> Struct {    
    let name_path = translate_name(&struct_def.name, generic_ctx, lib);

    let mut fields = HashMap::new();
    let mut pad_run = 0;
    let mut next_id = FieldID(0);
    for member in lib.aligned_struct_members(struct_def) {
        match member {
            StructLayoutMember::Data { member, .. } => {
                if pad_run > 0 {
                    fields.insert(next_id, StructFieldDef {
                        name: None,
                        rc: false,
                        ty: Type::U8.array(pad_run),
                    });
                    pad_run = 0;
                    next_id.0 += 1;
                }

                let name = member.ident.to_string();
                let ty = lib.translate_type(&member.ty, generic_ctx);
                let rc = member.ty.is_strong_rc_reference();
                fields.insert(next_id, StructFieldDef { name: Some(name), ty, rc });
                next_id.0 += 1;
            }

            StructLayoutMember::PaddingByte => {
                pad_run += 1;
            }
        }
    }

    if pad_run > 0 {
        let pad_ty = Type::U8.array(pad_run);
        fields.insert(next_id, StructFieldDef { name: None, rc: false, ty: pad_ty });
    }

    let identity = match struct_def.kind {
        StructKind::Class => StructIdentity::Class(name_path),
        StructKind::Record => StructIdentity::Record(name_path),
    };

    Struct::new(identity).with_fields(fields)
}
