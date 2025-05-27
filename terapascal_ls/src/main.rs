use dashmap::DashMap;
use std::path::PathBuf;
use terapascal_common::span::Span;
use terapascal_common::BuildOptions;
use terapascal_frontend::error::BuildError;
use terapascal_frontend::error::BuildResult;
use terapascal_frontend::parse;
use terapascal_frontend::pp::PreprocessedUnit;
use terapascal_frontend::preprocess;
use terapascal_frontend::tokenize;
use terapascal_frontend::typ;
use terapascal_frontend::TokenTree;
use terapascal_frontend::{ast, DelimiterPair};
use tower_lsp::jsonrpc::Result as RpcResult;
use tower_lsp::lsp_types::DidCloseTextDocumentParams;
use tower_lsp::lsp_types::DidOpenTextDocumentParams;
use tower_lsp::lsp_types::DidSaveTextDocumentParams;
use tower_lsp::lsp_types::DocumentFilter;
use tower_lsp::lsp_types::InitializeParams;
use tower_lsp::lsp_types::InitializeResult;
use tower_lsp::lsp_types::InitializedParams;
use tower_lsp::lsp_types::OneOf;
use tower_lsp::lsp_types::SemanticToken;
use tower_lsp::lsp_types::SemanticTokenType;
use tower_lsp::lsp_types::SemanticTokens;
use tower_lsp::lsp_types::SemanticTokensFullOptions;
use tower_lsp::lsp_types::SemanticTokensLegend;
use tower_lsp::lsp_types::SemanticTokensOptions;
use tower_lsp::lsp_types::SemanticTokensParams;
use tower_lsp::lsp_types::SemanticTokensRegistrationOptions;
use tower_lsp::lsp_types::SemanticTokensResult;
use tower_lsp::lsp_types::ServerCapabilities;
use tower_lsp::lsp_types::StaticRegistrationOptions;
use tower_lsp::lsp_types::TextDocumentRegistrationOptions;
use tower_lsp::lsp_types::TextDocumentSyncCapability;
use tower_lsp::lsp_types::TextDocumentSyncKind;
use tower_lsp::lsp_types::Url;
use tower_lsp::lsp_types::WorkDoneProgressOptions;
use tower_lsp::lsp_types::{DidChangeTextDocumentParams, DocumentHighlight, DocumentHighlightParams};
use tower_lsp::LanguageServer;
use tower_lsp::LspService;
use tower_lsp::Server;

fn make_semantic_token(span: &Span, token_type: u32, cur_line: &mut u32, cur_col: &mut u32) -> SemanticToken {
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

fn to_semantic_tokens(tt: &TokenTree, line: &mut u32, col: &mut u32, result: &mut Vec<SemanticToken>) {
    if result.is_empty() {
        let span = tt.span();
        *line = span.start.line as u32;
        *col = span.start.col as u32;
    }
    
    match tt {
        TokenTree::IntNumber { span, .. } | TokenTree::RealNumber { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_NUMBER, line, col));
        }
        TokenTree::String { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_STRING, line, col));
        }

        TokenTree::Operator { op, span } if op.is_keyword() => {
            result.push(make_semantic_token(span, SEMANTIC_OPERATOR, line, col));
        }

        TokenTree::Separator { span, .. }
        | TokenTree::Operator { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_OPERATOR, line, col));
        }

        TokenTree::Keyword { span, .. } => {
            result.push(make_semantic_token(span, SEMANTIC_KEYWORD, line, col));
        }

        TokenTree::Delimited(group) => {
            let delim_type = match group.delim {
                DelimiterPair::BeginEnd
                | DelimiterPair::CaseEnd
                | DelimiterPair::MatchEnd => SEMANTIC_KEYWORD,
                
                DelimiterPair::Bracket
                | DelimiterPair::SquareBracket => SEMANTIC_OPERATOR,
            };
            
            result.push(make_semantic_token(&group.open, delim_type, line, col));
            
            for inner in &group.inner {
                to_semantic_tokens(inner, line, col, result);
            }
            
            result.push(make_semantic_token(&group.close, delim_type, line, col));
        }
        
        _ => {}
    }
}

fn semantic_legend() -> Vec<SemanticTokenType> {
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

struct TerapascalServer {
    documents: DashMap<Url, String>,

    pp_units: DashMap<Url, PreprocessedUnit>,
    tokenized_units: DashMap<Url, Vec<TokenTree>>,
    parsed_units: DashMap<Url, ast::Unit>,
    typechecked_units: DashMap<Url, typ::ast::Unit>,
}

#[tower_lsp::async_trait]
impl LanguageServer for TerapascalServer {
    async fn initialize(&self, _params: InitializeParams) -> RpcResult<InitializeResult> {
        let semantic_token_reg_opts = SemanticTokensRegistrationOptions {
            text_document_registration_options: TextDocumentRegistrationOptions {
                document_selector: Some(vec![DocumentFilter {
                    language: Some("terapascal".to_string()),
                    scheme: Some("file".to_string()),
                    pattern: None,
                }]),
            },
            semantic_tokens_options: SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                legend: SemanticTokensLegend {
                    token_types: semantic_legend(),
                    token_modifiers: Vec::new(),
                },
                range: Some(false),
                full: Some(SemanticTokensFullOptions::Bool(true)),
            },
            static_registration_options: StaticRegistrationOptions::default(),
        };
        
        let text_document_sync = TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL);

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(text_document_sync),
                document_highlight_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(semantic_token_reg_opts.into()),
                definition_provider: Some(OneOf::Left(false)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        eprintln!("initialized")
    }

    async fn shutdown(&self) -> RpcResult<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        eprintln!("did_open: {}", params.text_document.uri);

        let uri = params.text_document.uri.clone();

        self.documents.insert(params.text_document.uri, params.text_document.text);

        if let Err(err) = self.update_document(uri) {
            eprintln!("{}", err)
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        eprintln!("did_change: {}", params.text_document.uri);

        let text = params.content_changes
            .into_iter()
            .next()
            .unwrap()
            .text;
        
        let Some(mut doc) = self.documents.get_mut(&params.text_document.uri) else {
            eprintln!("did_change: no document previously loaded at {}", params.text_document.uri);
            return;
        };

        *doc = text;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        eprintln!("did_save: {}", params.text_document.uri)
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        eprintln!("did_close: {}", params.text_document.uri);
        
        self.typechecked_units.remove(&params.text_document.uri);
        self.parsed_units.remove(&params.text_document.uri);
        self.tokenized_units.remove(&params.text_document.uri);
        self.pp_units.remove(&params.text_document.uri);
        self.documents.remove(&params.text_document.uri);
    }

    async fn document_highlight(&self, _params: DocumentHighlightParams) -> RpcResult<Option<Vec<DocumentHighlight>>> {
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> RpcResult<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        eprintln!("semantic_tokens_full: uri = {uri}");

        let mut data = Vec::new();
        let mut line = 0;
        let mut col = 0;

        if let Some(tokens) = self.tokenized_units.get(uri) {
            for tt in tokens.value() {
                to_semantic_tokens(tt, &mut line, &mut col, &mut data);
            }
        } else {
            eprintln!("document_color: {} was not opened", uri);
        };

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data,
            result_id: None,
        })))
    }
}

impl TerapascalServer {
    fn update_document(&self, uri: Url) -> BuildResult<()> {
        self.pp_units.remove(&uri);
        self.parsed_units.remove(&uri);
        self.typechecked_units.remove(&uri);

        let pp_unit = {
            let Some(document_text) = self.documents.get(&uri) else {
                return Err(BuildError::ReadSourceFileFailed {
                    path: PathBuf::from(uri.to_string()),
                    msg: "document was not found in the cache".to_string(),
                }.into());
            };

            let opts = BuildOptions::default();
            let Ok(file_path) = uri.to_file_path() else {
                return Err(BuildError::ReadSourceFileFailed {
                    path: PathBuf::from(uri.to_string()),
                    msg: "invalid file path".to_string(),
                }.into());
            };

            preprocess(file_path, document_text.as_str(), opts)?
        };

        let filename = pp_unit.filename.as_ref().clone();

        self.pp_units.insert(uri.clone(), pp_unit.clone());
        eprintln!("preprocessed {}", uri);

        let tokens = tokenize(pp_unit)?;

        self.tokenized_units.insert(uri.clone(), tokens.clone());
        eprintln!("tokenized {}", uri);

        let unit = parse(filename, tokens)?;

        self.parsed_units.insert(uri.clone(), unit);
        eprintln!("parsed {}", uri);

        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|_client| TerapascalServer {
        documents: DashMap::new(),
        pp_units: DashMap::new(),
        tokenized_units: DashMap::new(),
        parsed_units: DashMap::new(),
        typechecked_units: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
