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
) -> ir::StructDef {
    let name_path = translate_name(&struct_def.name, generic_ctx, lib);

    let mut fields = HashMap::new();
    let mut next_id = ir::FieldID(0);
    for member in lib.aligned_struct_members(struct_def) {
        match member {
            StructLayoutMember::Data { field_decl: member, decl_index, .. } => {
                let name = member.idents[decl_index].to_string();
                let ty = lib.translate_type(&member.ty, generic_ctx);
                
                fields.insert(next_id, ir::StructFieldDef { 
                    name: Some(name), 
                    ty 
                });
            }

            StructLayoutMember::PaddingByte => {
                fields.insert(next_id, ir::StructFieldDef {
                    name: None,
                    ty: ir::Type::U8,
                });
            }
        }
        next_id.0 += 1;
    }

    let identity = match struct_def.kind {
        ast::StructKind::Class => ir::StructIdentity::Class(name_path),
        ast::StructKind::Record => ir::StructIdentity::Record(name_path),
    };

    ir::StructDef::new(identity).with_fields(fields)
}
