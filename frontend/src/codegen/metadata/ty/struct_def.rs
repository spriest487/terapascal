use crate::ast::StructKind;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use crate::ir;
use std::collections::BTreeMap;
use typ::layout::StructLayoutMember;

pub fn translate_struct_def(
    struct_def: &typ::ast::StructDecl,
    lib: &mut LibraryBuilder,
) -> ir::StructDef {
    let tags = lib.translate_tag_groups(&struct_def.tags);
    
    let name_path = translate_name(&struct_def.name, lib);

    let mut fields = BTreeMap::new();
    let mut next_id = ir::FieldID(0);

    if struct_def.kind == StructKind::Class {
        assert_eq!(struct_def.packed, false, "class structs cannot have packed layout");

        for field_decl in struct_def.fields() {
            for i in 0..field_decl.idents.len() {
                let name = field_decl.idents[i].to_string();
                let ty = lib.translate_type(&field_decl.ty);

                fields.insert(next_id, ir::StructFieldDef::new(ty).with_name(name));

                next_id.0 += 1;
            }
        }
    } else {
        for member in lib.aligned_struct_members(struct_def) {
            match member {
                StructLayoutMember::Data { field_decl: member, decl_index, .. } => {
                    let name = member.idents[decl_index].to_string();
                    let ty = lib.translate_type(&member.ty);

                    fields.insert(next_id, ir::StructFieldDef::new(ty).with_name(name));
                }

                StructLayoutMember::PaddingByte => {
                    fields.insert(next_id, ir::StructFieldDef::new(ir::Type::I8));
                }
            }
            next_id.0 += 1;
        }
    }    

    let identity = match struct_def.kind {
        StructKind::Class => ir::StructIdentity::Class(name_path),
        StructKind::Record => ir::StructIdentity::Record(name_path),
    };

    ir::StructDef::new(identity)
        .with_tags(tags)
        .with_fields(fields)
}
