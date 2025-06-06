mod semantic_tokens;
mod project;

use crate::semantic_tokens::semantic_legend;
use dashmap::DashMap;
use std::path::PathBuf;
use terapascal_build::error::BuildError;
use terapascal_build::error::BuildResult;
use tokio::sync::RwLock;
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
use tower_lsp::LanguageServer;
use tower_lsp::LspService;
use tower_lsp::Server;
use crate::project::ProjectCollection;

struct TerapascalServer {
    documents: DashMap<Url, String>,

    projects: RwLock<ProjectCollection>,
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

        if let Err(err) = self.update_document(uri).await {
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

        if let Err(build_err) = self.update_document(params.text_document.uri).await {
            eprintln!("did_change: {build_err}")
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        eprintln!("did_save: {}", params.text_document.uri)
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        eprintln!("did_close: {}", params.text_document.uri);

        let uri = params.text_document.uri;
        self.documents.remove(&uri);
        
        if let Ok(file_path) = url_to_path(&uri) {
            let mut projects = self.projects.write().await;
            projects.remove_project(&file_path);
        }
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

        eprintln!("semantic_tokens_full: {}", params.text_document.uri);

        let doc_path = match url_to_path(&params.text_document.uri) {
            Ok(path) => path,
            Err(err) => {
                eprintln!("semantic_tokens_full: {}", err);
                return Ok(None);
            }
        };

        let projects = self.projects.read().await;

        let Some(project) = projects.get_document_project(&doc_path) else {
            eprintln!("semantic_tokens_full: workspace project not found for path {}", doc_path.display());
            return Ok(None);
        };

        match project.semantic_tokens.get(&doc_path) {
            Some(tokens) => {
                Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                    data: tokens.clone(),
                    result_id: None,
                })))
            }
            
            None => {
                eprintln!("semantic_tokens_full: no tokens loaded for {}", doc_path.display());
                Ok(None)
            }
        }
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
    async fn update_document(&self, uri: Url) -> BuildResult<()> {
        let doc_path = url_to_path(&uri)?;

        let mut projects = self.projects.write().await;
        projects.update_document(&doc_path);

        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|_client| TerapascalServer {
        documents: DashMap::new(),
        projects: RwLock::new(ProjectCollection::new()),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn url_to_path(url: &Url) -> BuildResult<PathBuf> {
    let Ok(file_path) = url.to_file_path() else {
        return Err(BuildError::ReadSourceFileFailed {
            path: PathBuf::from(url.to_string()),
            msg: "invalid file path".to_string(),
        })
    };

    match file_path.canonicalize() {
        Ok(path) => Ok(path),

        Err(err) => {
            Err(BuildError::ReadSourceFileFailed {
                path: file_path,
                msg: err.to_string(),
            })
        }
    }
}
