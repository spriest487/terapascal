use crate::DeclPath;
use crate::StructDef;
use crate::StructFieldDef;
use crate::StructIdentity;
use crate::StructLayout;
use crate::Type;
use crate::Visibility;
use crate::STRING_CHARS_FIELD;
use crate::STRING_LEN_FIELD;

pub fn string_def() -> StructDef {
    let name = DeclPath::new(["System".to_string()], "String".to_string());

    let mut def = StructDef::new(
        StructIdentity::Class(name.clone()),
        Visibility::Public,
        StructLayout::Default
    );

    def.fields.insert(STRING_CHARS_FIELD, StructFieldDef::new(Type::U8.ptr(), Visibility::Internal));
    def.fields.insert(STRING_LEN_FIELD, StructFieldDef::new(Type::I32, Visibility::Internal));
    
    def
}
