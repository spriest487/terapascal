use crate::token_tree::DelimitedGroup;
use crate::DelimiterPair;
use crate::IntConstant;
use crate::Keyword;
use crate::TokenTree;
use terapascal_common::span::Location;
use terapascal_common::span::Span;
use terapascal_common::BuildOptions;
use std::path::PathBuf;
use std::rc::Rc;
use crate::pp::Preprocessor;

fn tokenize(s: &str, case_sensitive: bool) -> Vec<TokenTree> {
    let mut opts = BuildOptions::default();
    opts.case_sensitive = case_sensitive;

    let test_unit = Preprocessor::new("test", opts)
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
        file: Rc::new(PathBuf::from("test")),
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
