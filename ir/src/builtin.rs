use crate::NamePath;
use crate::StructDef;
use crate::StructFieldDef;
use crate::StructIdentity;
use crate::Type;
use crate::STRING_CHARS_FIELD;
use crate::STRING_LEN_FIELD;

pub fn string_def() -> StructDef {
    let name = NamePath::new(["System".to_string()], "String");

    let mut def = StructDef::new(StructIdentity::Class(name.clone()));

    def.fields.insert(STRING_CHARS_FIELD, StructFieldDef {
        name: None,
        ty: Type::U8.ptr(),
    });
    def.fields.insert(STRING_LEN_FIELD, StructFieldDef {
        name: None,
        ty: Type::I32,
    });
    
    def
}
