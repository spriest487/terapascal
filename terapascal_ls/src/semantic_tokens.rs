use terapascal_common::span::Span;
use terapascal_frontend::DelimiterPair;
use terapascal_frontend::TokenTree;
use tower_lsp::lsp_types::SemanticToken;
use tower_lsp::lsp_types::SemanticTokenType;

pub fn semantic_legend() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::KEYWORD,
        SemanticTokenType::NUMBER,
        SemanticTokenType::STRING,
        SemanticTokenType::OPERATOR,
    ]
}

const SEMANTIC_KEYWORD: u32 = 0;
const SEMANTIC_NUMBER: u32 = 1;
const SEMANTIC_STRING: u32 = 2;
const SEMANTIC_OPERATOR: u32 = 3;

fn make_semantic_token(
    span: &Span,
    token_type: u32,
    cur_line: &mut u32,
    cur_col: &mut u32,
) -> SemanticToken {
    let line = span.start.line as u32;
    let delta_line = line.saturating_sub(*cur_line);
    if line != *cur_line {
        *cur_line = line;
        *cur_col = 0;
    }

    let col = span.start.col as u32;
    let delta_col = col.saturating_sub(*cur_col);
    *cur_col = col;

    SemanticToken {
        delta_line,
        delta_start: delta_col,
        length: span.end.col.saturating_sub(span.start.col) as u32 + 1,
        token_type,
        token_modifiers_bitset: 0,
    }
}

pub fn to_semantic_tokens(
    tt: &TokenTree,
    line: &mut u32,
    col: &mut u32,
    result: &mut Vec<SemanticToken>,
) {
    if result.is_empty() {
        let span = tt.span();
        *line = span.start.line as u32;
        *col = span.start.col as u32;
    }

    match tt {
        TokenTree::IntNumber { span, .. } | TokenTree::RealNumber { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_NUMBER, line, col));
        },
        TokenTree::String { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_STRING, line, col));
        },

        TokenTree::Operator { op, span } if op.is_keyword() => {
            result.push(make_semantic_token(span, SEMANTIC_OPERATOR, line, col));
        },

        TokenTree::Separator { span, .. } | TokenTree::Operator { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_OPERATOR, line, col));
        },

        TokenTree::Keyword { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_KEYWORD, line, col));
        },

        TokenTree::Delimited(group) => {
            let delim_type = match group.delim {
                DelimiterPair::BeginEnd | DelimiterPair::CaseEnd | DelimiterPair::MatchEnd => {
                    SEMANTIC_KEYWORD
                },

                DelimiterPair::Bracket | DelimiterPair::SquareBracket => SEMANTIC_OPERATOR,
            };

            result.push(make_semantic_token(&group.open, delim_type, line, col));

            for inner in &group.inner {
                to_semantic_tokens(inner, line, col, result);
            }

            result.push(make_semantic_token(&group.close, delim_type, line, col));
        },

        _ => {},
    }
}
