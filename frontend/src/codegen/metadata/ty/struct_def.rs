use crate::ast::StructKind;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::translate_name;
use crate::codegen::typ;
use crate::ir;
use std::collections::BTreeMap;
use std::rc::Rc;

pub fn class_dtor_sig(class_id: &Rc<ir::TypeRef>) -> ir::FunctionSig {
    ir::FunctionSig {
        param_types: vec![class_id.to_class_object_type()],
        result_type: ir::Type::Nothing,
    }
}

pub fn translate_struct_def(
    struct_def: &typ::ast::StructDecl,
    lib: &mut LibraryBuilder,
) -> ir::StructDef {
    let tags = lib.translate_tag_groups(&struct_def.tags);

    let generic_name = struct_def.name.as_ref().clone().to_generic_name();
    let name_path = translate_name(&generic_name, lib);

    let mut fields = BTreeMap::new();
    let mut next_id = ir::FieldID(0);

    for field_decl in struct_def.fields() {
        for i in 0..field_decl.idents.len() {
            let name = field_decl.idents[i].to_string();
            let ty = lib.translate_type(&field_decl.ty);

            fields.insert(next_id, ir::StructFieldDef::new(ty).with_name(name));

            next_id.0 += 1;
        }
    }

    let identity = match struct_def.kind {
        StructKind::Class => ir::StructIdentity::Class(name_path),
        StructKind::Record => ir::StructIdentity::Record(name_path),
    };

    let layout = if struct_def.packed {
        assert!(!identity.is_ref_type(), "translate_struct_def: only value types can be packed");
        ir::StructLayout::Packed
    } else {
        ir::StructLayout::Default
    };

    ir::StructDef::new(identity, layout)
        .with_tags(tags)
        .with_fields(fields)
}