use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::StructKind;
use crate::codegen::library_builder::LibraryBuilder;
use crate::ir;
use crate::typ;
use crate::typ::Symbol;
use crate::typ::SYSTEM_UNIT_NAME;
use std::rc::Rc;
use terapascal_common::span::Span;

pub const ALIAS_TAG_CLASS_NAME: &str = "PascalTypeAlias";
pub const ALIAS_TAG_NAME_FIELD: &str = "name";
pub const ALIAS_TAG_TARGET_FIELD: &str = "target";

pub fn create_alias_tag(lib: &mut LibraryBuilder, alias: &IdentPath, target: &typ::Type) {
    let alias_tag_path = IdentPath::new(
        Ident::new(ALIAS_TAG_CLASS_NAME, Span::zero("")),
        [Ident::new(SYSTEM_UNIT_NAME, Span::zero(""))]
    );

    let alias_tag_name = Symbol::from(alias_tag_path);

    if lib.find_struct_def(&alias_tag_name.full_path, StructKind::Class).is_none() {
        // if the tag is not defined, do nothing
        return;
    }

    let ir::Type::Object(ir::ObjectID::Class(class_ref)) = lib
        .translate_type(&typ::Type::class(alias_tag_name))
    else {
        return;
    };

    let Some(tag_class_def) = lib.metadata().get_struct_def(class_ref.def_id) else {
        return;
    };

    let Some(name_field) = tag_class_def.find_field(ALIAS_TAG_NAME_FIELD) else {
        return;
    };

    let Some(target_field) = tag_class_def.find_field(ALIAS_TAG_TARGET_FIELD) else {
        return;
    };

    let alias_string_ref = ir::Ref::from(lib.metadata_mut()
        .find_or_insert_string(&alias.to_string()));

    let target_type = lib.translate_type(target);
    let target_type_ref = ir::Ref::from(ir::GlobalRef::StaticTypeInfo(Rc::new(target_type)));

    let mut tag = ir::TagInfo::new(class_ref.def_id);
    tag.fields.insert(name_field, alias_string_ref.value());
    tag.fields.insert(target_field, target_type_ref.value());

    lib.add_tag(tag);
}