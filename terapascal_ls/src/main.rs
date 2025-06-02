use crate::semantic_tokens::SemanticTokenBuilder;
use crate::semantic_tokens::semantic_legend;
use dashmap::DashMap;
use std::path::PathBuf;
use terapascal_common::BuildOptions;
use terapascal_frontend::{typecheck, TokenTree};
use terapascal_frontend::ast;
use terapascal_frontend::error::BuildError;
use terapascal_frontend::error::BuildResult;
use terapascal_frontend::parse;
use terapascal_frontend::pp::PreprocessedUnit;
use terapascal_frontend::preprocess;
use terapascal_frontend::tokenize;
use terapascal_frontend::typ;
use tower_lsp::LanguageServer;
use tower_lsp::LspService;
use tower_lsp::Server;
use tower_lsp::jsonrpc::Result as RpcResult;
use tower_lsp::lsp_types::DidChangeTextDocumentParams;
use tower_lsp::lsp_types::DidChangeWorkspaceFoldersParams;
use tower_lsp::lsp_types::DidCloseTextDocumentParams;
use tower_lsp::lsp_types::DidOpenTextDocumentParams;
use tower_lsp::lsp_types::DidSaveTextDocumentParams;
use tower_lsp::lsp_types::DocumentFilter;
use tower_lsp::lsp_types::DocumentHighlight;
use tower_lsp::lsp_types::DocumentHighlightParams;
use tower_lsp::lsp_types::InitializeParams;
use tower_lsp::lsp_types::InitializeResult;
use tower_lsp::lsp_types::InitializedParams;
use tower_lsp::lsp_types::OneOf;
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
use tower_lsp::lsp_types::WorkspaceFoldersServerCapabilities;
use tower_lsp::lsp_types::WorkspaceServerCapabilities;

mod semantic_tokens;

struct TerapascalServer {
    documents: DashMap<Url, String>,

    pp_units: DashMap<Url, PreprocessedUnit>,
    tokenized_units: DashMap<Url, Vec<TokenTree>>,
    parsed_units: DashMap<Url, ast::Unit>,
    typechecked_modules: DashMap<Url, typ::Module>,
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

        let workspace_folders = WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: None,
            }),
            file_operations: None,
        };

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(text_document_sync),
                document_highlight_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(semantic_token_reg_opts.into()),
                definition_provider: Some(OneOf::Left(false)),
                workspace: Some(workspace_folders),
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

        let text = params.content_changes.into_iter().next().unwrap().text;

        {
            let Some(mut doc) = self.documents.get_mut(&params.text_document.uri) else {
                eprintln!(
                    "did_change: no document previously loaded at {}",
                    params.text_document.uri
                );
                return;
            };

            *doc = text;
        }

        if let Err(build_err) = self.update_document(params.text_document.uri) {
            eprintln!("did_change: {build_err}")
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        eprintln!("did_save: {}", params.text_document.uri)
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        eprintln!("did_close: {}", params.text_document.uri);

        self.typechecked_modules.remove(&params.text_document.uri);
        self.parsed_units.remove(&params.text_document.uri);
        self.tokenized_units.remove(&params.text_document.uri);
        self.pp_units.remove(&params.text_document.uri);
        self.documents.remove(&params.text_document.uri);
    }

    async fn document_highlight(
        &self,
        _params: DocumentHighlightParams,
    ) -> RpcResult<Option<Vec<DocumentHighlight>>> {
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> RpcResult<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        eprintln!("semantic_tokens_full: uri = {uri}");

        let mut builder = SemanticTokenBuilder::new(0, 0);

        {
            if let Some(unit) = self.parsed_units.get(uri) {
                builder.add_unit(unit.value());
            } else if let Some(tokens) = self.tokenized_units.get(uri) {
                for tt in tokens.value() {
                    builder.add_token_tree(tt);
                }
            } else {
                eprintln!("document_color: {} was not opened", uri);
            };
        }
        
        let data = builder.finish();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data,
            result_id: None,
        })))
    }

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        for added in params.event.added {
            eprintln!(
                "did_change_workspace_folders: added {} ({})",
                added.name, added.uri
            )
        }

        for removed in params.event.removed {
            eprintln!(
                "did_change_workspace_folders: removed {} ({})",
                removed.name, removed.uri
            )
        }
    }
}

impl TerapascalServer {
    fn update_document(&self, uri: Url) -> BuildResult<()> {
        self.pp_units.remove(&uri);
        self.tokenized_units.remove(&uri);
        self.parsed_units.remove(&uri);
        self.typechecked_modules.remove(&uri);

        let pp_unit = {
            let Some(document_text) = self.documents.get(&uri) else {
                return Err(BuildError::ReadSourceFileFailed {
                    path: PathBuf::from(uri.to_string()),
                    msg: "document was not found in the cache".to_string(),
                }
                .into());
            };

            let opts = BuildOptions::default();
            let Ok(file_path) = uri.to_file_path() else {
                return Err(BuildError::ReadSourceFileFailed {
                    path: PathBuf::from(uri.to_string()),
                    msg: "invalid file path".to_string(),
                }
                .into());
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

        self.parsed_units.insert(uri.clone(), unit.clone());
        eprintln!("parsed {}", uri);

        let module = typecheck(&[unit], false)?;
        eprintln!("typechecked {}", uri);

        self.typechecked_modules.insert(uri, module);

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
        typechecked_modules: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
