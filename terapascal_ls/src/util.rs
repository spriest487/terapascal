use crate::project::LinksEntry;
use crate::workspace::Workspace;
use std::path::PathBuf;
use terapascal_common::span::Location;
use terapascal_common::span::Span;
use tower_lsp::lsp_types as lsp;
use tower_lsp::lsp_types::Url;

pub fn convert_text_doc_position_params(
    workspace: &Workspace,
    params: &lsp::TextDocumentPositionParams,
) -> Option<(PathBuf, Location)> {
    let uri = &params.text_document.uri;

    let Ok(file_path) = workspace.url_to_path(uri) else {
        return None;
    };

    let position = params.position;
    let location = Location {
        line: position.line as usize,
        col: position.character as usize,
    };

    Some((file_path, location))
}

pub fn convert_location(location: Location) -> lsp::Position {
    lsp::Position {
        line: location.line as u32,
        character: location.col as u32,
    }
}

pub fn convert_range(start: Location, end: Location) -> lsp::Range {
    let mut result = lsp::Range {
        start: convert_location(start),
        end: convert_location(end),
    };

    // end is exclusive not inclusive
    result.end.character += 1;

    result
}

pub fn span_range(span: &Span) -> lsp::Range {
    convert_range(span.start, span.end)
}

pub fn collect_definition_links(entries: &LinksEntry, include_key: bool) -> Vec<lsp::LocationLink> {
    let mut links: Vec<_> = entries.links.iter().filter_map(span_to_location_link).collect();

    if include_key {
        if let Some(key_link) = span_to_location_link(&entries.key) {
            links.insert(0, key_link);
        }
    }

    links
}

pub fn span_to_location_link(span: &Span) -> Option<lsp::LocationLink> {
    let target_uri = Url::from_file_path(span.file.as_ref()).ok()?;

    let range = convert_range(span.start, span.end);

    Some(lsp::LocationLink {
        target_uri,
        origin_selection_range: None,
        target_range: range,
        target_selection_range: range,
    })
}
