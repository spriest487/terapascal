mod project;
mod semantic_tokens;
mod fs;

use crate::project::LinksEntry;
use crate::project::Workspace;
use crate::semantic_tokens::semantic_legend;
use std::path::PathBuf;
use terapascal_common::span::Location;
use terapascal_common::span::Span;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result as RpcResult;
use tower_lsp::lsp_types as lsp;
use tower_lsp::lsp_types::DidChangeTextDocumentParams;
use tower_lsp::lsp_types::DidChangeWorkspaceFoldersParams;
use tower_lsp::lsp_types::DidCloseTextDocumentParams;
use tower_lsp::lsp_types::DidOpenTextDocumentParams;
use tower_lsp::lsp_types::DidSaveTextDocumentParams;
use tower_lsp::lsp_types::DocumentFilter;
use tower_lsp::lsp_types::DocumentHighlight;
use tower_lsp::lsp_types::DocumentHighlightParams;
use tower_lsp::lsp_types::GotoDefinitionParams;
use tower_lsp::lsp_types::GotoDefinitionResponse;
use tower_lsp::lsp_types::InitializeParams;
use tower_lsp::lsp_types::InitializeResult;
use tower_lsp::lsp_types::InitializedParams;
use tower_lsp::lsp_types::OneOf;
use tower_lsp::lsp_types::ReferenceParams;
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

struct TerapascalServer {
    workspace: RwLock<Workspace>,
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
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
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
        eprintln!("[did_open] {}", params.text_document.uri);

        let mut workspace = self.workspace.write().await;
        
        let uri = params.text_document.uri.clone();
        let Ok(file_path) = workspace.url_to_path(&uri) else {
            eprintln!("[did_open] bad uri: {uri}");
            return;
        };

        workspace.filesystem.add(file_path.clone(), params.text_document.text);
        workspace.update_document(&file_path);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        eprintln!("[did_change] {}", params.text_document.uri);

        let Some(change) = params.content_changes.into_iter().next() else {
            return;
        };

        let mut workspace = self.workspace.write().await;

        let Ok(file_path) = workspace.url_to_path(&params.text_document.uri) else {
            eprintln!("[did_change] bad uri: {}", params.text_document.uri);
            return;
        };

        workspace.filesystem.add(file_path.clone(), change.text);
        workspace.update_document(&file_path);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        eprintln!("[did_save] {}", params.text_document.uri)
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        eprintln!("[did_close] {}", params.text_document.uri);

        let mut workspace = self.workspace.write().await;

        let Ok(file_path) = workspace.url_to_path(&params.text_document.uri) else {
            eprintln!("[did_change] bad uri: {}", params.text_document.uri);
            return;
        };
        
        workspace.filesystem.remove(&file_path);
        workspace.remove_project(&file_path);
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> RpcResult<Option<GotoDefinitionResponse>> {
        let text_pos = &params.text_document_position_params;
        eprintln!(
            "[goto_definition] {}:{}:{}",
            text_pos.text_document.uri,
            text_pos.position.line + 1,
            text_pos.position.character + 1
        );

        let workspace = self.workspace.read().await;

        let Some((file_path, location)) = convert_text_doc_position_params(&workspace, text_pos) else {
            return Ok(None);
        };

        let Some(project) = workspace.get_document_project(&file_path) else {
            return Ok(None);
        };

        let Some(defs) = project.find_definition(&file_path, location) else {
            return Ok(None);
        };

        let links = collect_definition_links(defs, false);

        for link in &links {
            eprintln!(
                "[goto_definition] linked to: {}:{}:{}-{}:{}",
                link.target_uri,
                link.target_range.start.line + 1,
                link.target_range.start.character + 1,
                link.target_range.end.line + 1,
                link.target_range.end.character + 1,
            );
        }

        let result = if links.is_empty() {
            None
        } else {
            Some(GotoDefinitionResponse::Link(links))
        };

        Ok(result)
    }

    async fn references(&self, params: ReferenceParams) -> RpcResult<Option<Vec<lsp::Location>>> {
        let text_pos = &params.text_document_position;
        eprintln!(
            "[references] {}:{}:{}",
            text_pos.text_document.uri,
            text_pos.position.line + 1,
            text_pos.position.character + 1
        );
        
        let workspace = self.workspace.read().await;

        let Some((file_path, location)) = convert_text_doc_position_params(&workspace, text_pos) else {
            return Ok(None);
        };

        let Some(project) = workspace.get_document_project(&file_path) else {
            return Ok(None);
        };

        let Some(references) = project.find_usages(&file_path, location) else {
            return Ok(None);
        };

        let links = collect_definition_links(references, params.context.include_declaration);
        if links.is_empty() {
            return Ok(None);
        }

        let mut locations = Vec::with_capacity(links.len());
        for link in links {
            eprintln!(
                "[references] linked to: {}:{}:{}-{}:{}",
                link.target_uri,
                link.target_range.start.line + 1,
                link.target_range.start.character + 1,
                link.target_range.end.line + 1,
                link.target_range.end.character + 1,
            );
            
            locations.push(lsp::Location {
                uri: link.target_uri,
                range: link.target_range,
            });
        }

        Ok(Some(locations))
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
        eprintln!("[semantic_tokens_full] {}", params.text_document.uri);

        let workspace = self.workspace.read().await;

        let doc_path = match workspace.url_to_path(&params.text_document.uri) {
            Ok(path) => path,
            Err(err) => {
                eprintln!("[semantic_tokens_full] {}", err);
                return Ok(None);
            },
        };

        let Some(project) = workspace.get_document_project(&doc_path) else {
            eprintln!(
                "[semantic_tokens_full] workspace project not found for path {}",
                doc_path.display()
            );
            return Ok(None);
        };

        match project.semantic_tokens.get(&doc_path) {
            Some(tokens) => Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                data: tokens.clone(),
                result_id: None,
            }))),

            None => {
                eprintln!(
                    "[semantic_tokens_full] no tokens loaded for {}",
                    doc_path.display()
                );
                Ok(None)
            },
        }
    }

    async fn did_change_workspace_folders(&self, params: DidChangeWorkspaceFoldersParams) {
        for added in params.event.added {
            eprintln!(
                "[did_change_workspace_folders] added {} ({})",
                added.name, added.uri
            )
        }

        for removed in params.event.removed {
            eprintln!(
                "[did_change_workspace_folders] removed {} ({})",
                removed.name, removed.uri
            )
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|_client| TerapascalServer {
        workspace: RwLock::new(Workspace::new()),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn convert_text_doc_position_params(
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

fn convert_location(location: Location) -> lsp::Position {
    lsp::Position {
        line: location.line as u32,
        character: location.col as u32,
    }
}

fn convert_range(start: Location, end: Location) -> lsp::Range {
    let mut result = lsp::Range {
        start: convert_location(start),
        end: convert_location(end),
    };

    // end is exclusive not inclusive
    result.end.character += 1;

    result
}

fn collect_definition_links(entries: &LinksEntry, include_key: bool) -> Vec<lsp::LocationLink> {
    let mut links: Vec<_> = entries.links.iter().filter_map(span_to_location_link).collect();

    if include_key {
        if let Some(key_link) = span_to_location_link(&entries.key) {
            links.insert(0, key_link);
        }
    }

    links
}

fn span_to_location_link(span: &Span) -> Option<lsp::LocationLink> {
    let target_uri = Url::from_file_path(span.file.as_ref()).ok()?;

    let range = convert_range(span.start, span.end);

    Some(lsp::LocationLink {
        target_uri,
        origin_selection_range: None,
        target_range: range,
        target_selection_range: range,
    })
}
