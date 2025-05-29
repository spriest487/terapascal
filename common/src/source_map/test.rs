use crate::source_map::*;
use crate::{Location, Span};
use std::path::PathBuf;

struct TestMapping {
    start: (usize, usize),
    end: (usize, usize),
    src_file: &'static str,
    src_start: (usize, usize),
    src_end: (usize, usize),
}

fn build_with_mappings(
    filename: &'static str,
    entries: impl IntoIterator<Item=TestMapping>
) -> SourceMap {
    let mut builder = SourceMapBuilder::new(Arc::new(filename.into()));

    for mapping in entries {
        builder.add(
            Location { line: mapping.start.0, col: mapping.start.1 },
            Location { line: mapping.end.0, col: mapping.end.1 },
            Span::new(
                mapping.src_file,
                Location { line: mapping.src_start.0, col: mapping.src_start.1 },
                Location { line: mapping.src_end.0, col: mapping.src_end.1 },
            )
        )
    }

    builder.build()
}

#[test]
fn finds_single_entry() {
    let map = build_with_mappings("test_src.tpas", [
        TestMapping { start: (2, 2), end: (2, 4), src_file: "test_src.tpas", src_start: (0, 3), src_end: (0, 5)}
    ]);

    // look up the exact span, should have the exact mapped src location
    let span = map.lookup(
        Location { line: 2, col: 2 },
        Location { line: 2, col: 4 },
    );

    assert_eq!(PathBuf::from("test_src.tpas"), *span.file);
    assert_eq!(Location::new(0, 3), span.start);
    assert_eq!(Location::new(0, 5), span.end);
}

#[test]
fn finds_nearest_entry() {
    let map = build_with_mappings("test_src.tpas", [
        TestMapping { start: (0, 0), end: (0, 10), src_file: "test_src.tpas", src_start: (0, 0), src_end: (0, 10) },
        TestMapping { start: (1, 0), end: (2, 2), src_file: "test_src.tpas", src_start: (0, 10), src_end: (0, 15) },
        TestMapping { start: (2, 1), end: (2, 4), src_file: "test_src.tpas", src_start: (0, 16), src_end: (0, 20) },
    ]);

    // looks up a 3-character span that starts in the middle of the second mapping
    let span = map.lookup(
        Location { line: 1, col: 1 },
        Location { line: 1, col: 3 },
    );

    // expect a src span of the same length offset by the same amount within the second mapping
    assert_eq!(PathBuf::from("test_src.tpas"), *span.file);
    assert_eq!(Location::new(0, 11), span.start);
    assert_eq!(Location::new(0, 13), span.end);
}
