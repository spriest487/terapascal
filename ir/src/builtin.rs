use crate::NamePath;
use crate::Struct;
use crate::StructFieldDef;
use crate::StructIdentity;
use crate::Type;
use crate::STRING_CHARS_FIELD;
use crate::STRING_LEN_FIELD;

pub fn string_def() -> Struct {
    let name = NamePath::new(["System".to_string()], "String");

    Struct {
        identity: StructIdentity::Class(name.clone()),
        fields: [
            (STRING_CHARS_FIELD, StructFieldDef {
                name: None,
                ty: Type::U8.ptr(),
                rc: false,
            }),
            (STRING_LEN_FIELD, StructFieldDef {
                name: None,
                ty: Type::I32,
                rc: false,
            }),
        ].into_iter().collect()
    }
}
