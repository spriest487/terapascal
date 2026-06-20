use crate::pp::PreprocessedUnit;
use crate::token_tree::lex::lex;
use crate::token_tree::lex::STRING_ESCAPE_BACKSPACE;
use crate::token_tree::lex::STRING_ESCAPE_CR;
use crate::token_tree::lex::STRING_ESCAPE_NEWLINE;
use crate::token_tree::lex::STRING_ESCAPE_TAB;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::source_map::SourceMapBuilder;
use terapascal_common::CompileOpts;

fn make_pp_unit(src: &str) -> PreprocessedUnit {
    let name = "test";

    PreprocessedUnit {
        filename: Arc::new(PathBuf::from(name)),
        opts: CompileOpts::default(),
        source: src.to_string(),
        warnings: Vec::new(),
        source_map: SourceMapBuilder::new(PathBuf::from(name)).build(),
    }
}

#[test]
pub fn double_quote_is_escaped_in_string() {
    let pp_unit = make_pp_unit("'hello, ''world''!'");
    let result = lex(pp_unit).unwrap();

    assert_eq!(1, result.len());

    let string_token = result[0].as_literal_string().expect("result must be a string token");
    assert_eq!("hello, 'world'!", string_token)
}

#[test]
pub fn escape_chars_are_escaped() {
    for escape_char in [
        STRING_ESCAPE_BACKSPACE,
        STRING_ESCAPE_CR,
        STRING_ESCAPE_NEWLINE,
        STRING_ESCAPE_TAB
    ] {
        let escape_string = format!("{escape_char}");
        let pp_unit = make_pp_unit(&format!("'{escape_string}'"));
        let result = lex(pp_unit).unwrap();

        assert_eq!(1, result.len());

        let string_token = result[0].as_literal_string().expect("result must be a string token");
        assert_eq!(escape_string, string_token)
    }
}