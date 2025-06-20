use crate::pp::Preprocessor;
use crate::token_tree::DelimitedGroup;
use crate::IntConstant;
use crate::Keyword;
use crate::TokenTree;
use crate::DelimiterPair;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_common::span::Location;
use terapascal_common::span::Span;
use terapascal_common::CompileOpts;
use terapascal_common::fs::DefaultFilesystem;

fn tokenize(s: &str, case_sensitive: bool) -> Vec<TokenTree> {
    let mut opts = CompileOpts::default();
    opts.case_sensitive = case_sensitive;

    let test_unit = Preprocessor::new(&DefaultFilesystem, "test", opts)
        .preprocess(s)
        .unwrap();

    match TokenTree::tokenize(test_unit) {
        Ok(result) => result,
        Err(err) => {
            panic!("{} @ {:#?}", err.err, err.bt);
        },
    }
}

#[test]
fn tokenizes_literal_string() {
    let result = tokenize("  'hello world!'  ", true);

    assert_eq!(1, result.len());

    match &result[0] {
        TokenTree::String { value, .. } => assert_eq!("hello world!", value.as_str()),
        _ => panic!("got {:#?}, expected a string", result),
    }
}

#[test]
fn tokenizes_literal_char() {
    let result = tokenize(" #32 ", true);
    assert_eq!(1, result.len());

    match result[0] {
        TokenTree::IntNumber { value, .. } => assert_eq!(IntConstant::from(32), value),
        _ => panic!("got {:#?}, expected a char literal", result),
    }
}

#[test]
fn tokenizes_keywords_case_insensitive_mode() {
    let result = tokenize("  True TRUE true FALSE false", false);

    let expected_kws = [
        Keyword::True,
        Keyword::True,
        Keyword::True,
        Keyword::False,
        Keyword::False,
    ];

    assert_eq!(
        expected_kws.len(),
        result.len(),
        "expected 5 keywords, found {:#?}",
        result
    );

    for i in 0..expected_kws.len() {
        let kw = match result[i] {
            TokenTree::Keyword { kw, .. } => kw,
            _ => panic!(
                "expected result to be a {} keyword token, was {:#?}",
                expected_kws[i], result
            ),
        };

        assert_eq!(expected_kws[i], kw);
    }
}

#[test]
fn tokenizes_bracket_delim() {
    let result = tokenize("(a)", false);

    match &result[0] {
        TokenTree::Delimited(DelimitedGroup {
            delim: DelimiterPair::Bracket,
            inner,
            ..
        }) => {
            assert_eq!(1, inner.len());
            match &inner[0] {
                TokenTree::Ident(ident) => assert_eq!("a", ident.name.as_str()),
                _ => panic!("expected ident `a`, found {:#?}", inner[0]),
            }
        },

        _ => panic!("expected bracket-delimited group, got {:#?}", result),
    }
}

#[test]
fn tokenizes_mixed_delim() {
    let result = tokenize("(begin a end)", false);

    match &result[0] {
        TokenTree::Delimited(DelimitedGroup {
            delim: DelimiterPair::Bracket,
            inner,
            ..
        }) => {
            assert_eq!(1, inner.len(), "expected one inner token, got {:#?}", inner);
            match &inner[0] {
                TokenTree::Delimited(DelimitedGroup {
                    delim: DelimiterPair::BeginEnd,
                    inner,
                    ..
                }) => match &inner[0] {
                    TokenTree::Ident(ident) => assert_eq!("a", ident.name.as_str()),
                    _ => panic!("expected ident `a`, found {:#?}", inner[0]),
                },
                _ => panic!("expected begin/end delimited tree, found {:#?}", inner[0]),
            }
        },

        _ => panic!("expected bracket-delimited group, got {:#?}", result),
    }
}

fn test_span(from: (usize, usize), to: (usize, usize)) -> Span {
    Span {
        file: Arc::new(PathBuf::from("test")),
        start: Location {
            line: from.0,
            col: from.1,
        },
        end: Location {
            line: to.0,
            col: to.1,
        },
    }
}

#[test]
fn begin_end_delim_has_correct_spans() {
    let result = tokenize(
        r"begin
1 2 3
end",
        false,
    );
    match &result[0] {
        TokenTree::Delimited(DelimitedGroup {
            delim: DelimiterPair::BeginEnd,
            inner,
            open,
            close,
            span,
        }) => {
            assert_eq!(&test_span((0, 0), (0, 4)), open, "span of open token");
            assert_eq!(&test_span((2, 0), (2, 2)), close, "span of close token");
            assert_eq!(&test_span((0, 0), (2, 2)), span, "total span");
            assert_eq!(3, inner.len());
        },

        _ => panic!("expectefd begin/end delim group, got {:?}", result[0]),
    }
}

#[test]
fn bracket_delim_group_has_expected_span() {
    let result = tokenize("123 (abc 123) 123", true);
    
    assert_eq!(3, result.len());
    let group = result[1].clone().into_delimited_group().unwrap();
    
    assert_eq!(group.span.start, group.open.start);
    assert_eq!(group.span.end, group.close.end);
    
    assert_eq!(group.open.start.col, 4);
    assert_eq!(group.open.end.col, 4);

    assert_eq!(group.close.start.col,  12);
    assert_eq!(group.close.end.col, 12);
}

#[test]
fn keyword_delim_group_has_expected_span() {
    let result = tokenize("a begin abc 123 end b", true);

    assert_eq!(3, result.len());
    let group = result[1].clone().into_delimited_group().unwrap();

    assert_eq!(group.span.start, group.open.start);
    assert_eq!(group.span.end, group.close.end);

    assert_eq!(group.open.start.col, 2);
    assert_eq!(group.open.end.col, 6);

    assert_eq!(group.close.start.col,  16);
    assert_eq!(group.close.end.col, 18);
}
