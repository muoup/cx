use dashmap::DashMap;
use std::any::Any;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::Semaphore;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod check;
mod definition;
mod diagnostics;
mod index;
mod position;
mod project;
mod semantic_tokens;
mod symbols;

use definition::Goto;
use diagnostics::DiagnosticStore;
use index::ProjectIndex;
use project::{find_project_root, load_project_settings, ProjectSettings};

struct Backend {
    client: Client,
    documents: DashMap<Url, DocumentState>,
    projects: DashMap<PathBuf, Arc<ProjectSettings>>,
    latest_checks: DashMap<Url, u64>,
    diagnostics: DiagnosticStore,
    index: ProjectIndex,
    next_generation: AtomicU64,
    check_semaphore: Semaphore,
}

#[derive(Clone)]
struct DocumentState {
    text: String,
    version: i32,
}

fn panic_message(payload: Box<dyn Any + Send>) -> String {
    if let Some(message) = payload.downcast_ref::<&str>() {
        (*message).to_string()
    } else if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else {
        "unknown compiler panic".to_string()
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "cx-lsp".to_string(),
                version: Some("0.0.1".to_string()),
            }),
            capabilities: ServerCapabilities {
                position_encoding: Some(PositionEncodingKind::UTF16),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: Some(false),
                        will_save_wait_until: Some(false),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(
                            SaveOptions::default(),
                        )),
                    },
                )),
                definition_provider: Some(OneOf::Left(true)),
                declaration_provider: Some(DeclarationCapability::Simple(true)),
                document_symbol_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: Some(false),
                            },
                            legend: SemanticTokensLegend {
                                token_types: semantic_tokens::LEGEND.into(),
                                token_modifiers: vec![],
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "CX language server initialized.")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = params.text_document;
        self.documents.insert(
            document.uri,
            DocumentState {
                text: document.text,
                version: document.version,
            },
        );
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(change) = params.content_changes.into_iter().last() else {
            return;
        };
        self.documents.insert(
            params.text_document.uri,
            DocumentState {
                text: change.text,
                version: params.text_document.version,
            },
        );
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(text) = params.text {
            if let Some(mut document) = self.documents.get_mut(&uri) {
                document.text = text;
            }
        }
        let saved_version = self.documents.get(&uri).map(|document| document.version);

        let generation = self.next_generation.fetch_add(1, Ordering::Relaxed);
        self.latest_checks.insert(uri.clone(), generation);

        let Ok(file_path) = uri.to_file_path() else {
            self.client
                .log_message(MessageType::ERROR, format!("Invalid file path: {uri}"))
                .await;
            self.client.publish_diagnostics(uri, vec![], None).await;
            return;
        };

        let root = find_project_root(&file_path);
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Typechecking {} from project root {}",
                    file_path.display(),
                    root.display()
                ),
            )
            .await;

        let Ok(permit) = self.check_semaphore.acquire().await else {
            return;
        };
        if !self.check_is_current(&uri, generation) {
            return;
        }

        let task = tokio::task::spawn_blocking(move || {
            catch_unwind(AssertUnwindSafe(|| {
                let project = load_project_settings(&root)?;
                let report = check::check(&file_path, &project);
                Ok::<_, String>((project, report))
            }))
        })
        .await;

        let (project, report) = match task {
            Ok(Ok(Ok(result))) => result,
            Ok(Ok(Err(message))) => return self.log_check_failure(&uri, &message).await,
            Ok(Err(payload)) => return self.log_check_failure(&uri, &panic_message(payload)).await,
            Err(error) => return self.log_check_failure(&uri, &error.to_string()).await,
        };
        self.projects.insert(project.root.clone(), Arc::new(project));

        if !self.check_is_current(&uri, generation) {
            return;
        }

        self.index.update(report.index);
        let publications =
            self.diagnostics
                .apply(&uri, saved_version, report.diagnostics, report.checked_files);
        for publication in publications {
            self.client
                .publish_diagnostics(publication.uri, publication.diagnostics, publication.version)
                .await;
        }

        drop(permit);
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(self.goto(params.text_document_position_params, Goto::Definition))
    }

    async fn goto_declaration(
        &self,
        params: request::GotoDeclarationParams,
    ) -> Result<Option<request::GotoDeclarationResponse>> {
        Ok(self.goto(params.text_document_position_params, Goto::Declaration))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let Some(index) = self.index.file(&params.text_document.uri) else {
            return Ok(None);
        };
        Ok(Some(DocumentSymbolResponse::Nested(
            symbols::document_symbols(&index),
        )))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let Some(text) = self.documents.get(&uri).map(|document| document.text.clone()) else {
            return Ok(None);
        };
        let Ok(path) = uri.to_file_path() else {
            return Ok(None);
        };

        let project = self.project(&find_project_root(&path));
        let index = self.index.file(&uri);
        let data = semantic_tokens::semantic_tokens(&text, &path, &project, index.as_deref());

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }
}

impl Backend {
    fn check_is_current(&self, uri: &Url, generation: u64) -> bool {
        self.latest_checks
            .get(uri)
            .is_some_and(|latest| *latest == generation)
    }

    fn project(&self, root: &std::path::Path) -> Arc<ProjectSettings> {
        if let Some(project) = self.projects.get(root) {
            return project.clone();
        }
        let project = Arc::new(
            load_project_settings(root).unwrap_or_else(|_| ProjectSettings::new(root)),
        );
        self.projects.insert(root.to_path_buf(), project.clone());
        project
    }

    fn goto(&self, position: TextDocumentPositionParams, goto: Goto) -> Option<GotoDefinitionResponse> {
        let index = self.index.file(&position.text_document.uri)?;
        definition::locate(&index, position.position, goto)
    }

    async fn log_check_failure(&self, uri: &Url, message: &str) {
        self.client
            .log_message(
                MessageType::ERROR,
                format!("Typecheck failed for {uri}: {message}"),
            )
            .await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: DashMap::new(),
        projects: DashMap::new(),
        latest_checks: DashMap::new(),
        diagnostics: DiagnosticStore::default(),
        index: ProjectIndex::default(),
        next_generation: AtomicU64::new(1),
        check_semaphore: Semaphore::new(1),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
