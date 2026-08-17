use cx_lexer::lex;
use cx_tokens::token::TokenKind;
use dashmap::DashMap;
use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use tokio::sync::Semaphore;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod position;
mod typecheck_service;

use position::byte_index_to_position;

/// Find the project root by searching for cx.toml, .internal, or .git directories
/// in parent directories of the given file path.
/// Prefers cx.toml over other markers.
fn find_project_root(file_path: &Path) -> PathBuf {
    let mut current = file_path.parent().unwrap_or(file_path);

    // First pass: look for cx.toml (highest priority)
    let mut search = current;
    loop {
        if search.join("cx.toml").is_file() {
            return search.to_path_buf();
        }
        match search.parent() {
            Some(parent) if parent != search => search = parent,
            _ => break,
        }
    }

    // Second pass: fall back to .internal or .git
    loop {
        if current.join(".internal").is_dir() || current.join(".git").is_dir() {
            return current.to_path_buf();
        }

        match current.parent() {
            Some(parent) if parent != current => current = parent,
            _ => {
                return file_path
                    .parent()
                    .unwrap_or_else(|| Path::new("/"))
                    .to_path_buf();
            }
        }
    }
}

const KEYWORD_IDX: u32 = 0;
const _OPERATOR_IDX: u32 = 1;
const STRING_IDX: u32 = 2;
const NUMBER_IDX: u32 = 3;
const _TYPE_IDX: u32 = 4;
const _VARIABLE_IDX: u32 = 5;
const _FUNCTION_IDX: u32 = 6;
const _COMMENT_IDX: u32 = 7;
const _MACRO_IDX: u32 = 8;

// Define the token types that our server supports.
const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,  // Index 0
    SemanticTokenType::OPERATOR, // Index 1
    SemanticTokenType::STRING,   // Index 2
    SemanticTokenType::NUMBER,   // Index 3
    SemanticTokenType::TYPE,     // Index 4
    SemanticTokenType::VARIABLE, // Index 5
    SemanticTokenType::FUNCTION, // Index 6
    SemanticTokenType::COMMENT,  // Index 7
    SemanticTokenType::MACRO,    // Index 8
];

struct Backend {
    client: Client,
    document_map: DashMap<Url, DocumentState>,
    latest_checks: DashMap<Url, u64>,
    diagnostic_contributions: Mutex<HashMap<Url, HashMap<Url, Vec<Diagnostic>>>>,
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
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: Some(false),
                            },
                            legend: SemanticTokensLegend {
                                token_types: LEGEND_TYPE.into(),
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
        self.latest_checks.remove(&document.uri);
        self.document_map.insert(
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

        let uri = params.text_document.uri;
        self.latest_checks.remove(&uri);
        self.document_map.insert(
            uri,
            DocumentState {
                text: change.text,
                version: params.text_document.version,
            },
        );
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.document_map.remove(&uri);
        self.latest_checks.remove(&uri);
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        if let Some(text) = params.text {
            if let Some(mut document) = self.document_map.get_mut(&uri) {
                document.text = text;
            }
        }
        let saved_version = self.document_map.get(&uri).map(|document| document.version);

        let generation = self.next_generation.fetch_add(1, Ordering::Relaxed);
        self.latest_checks.insert(uri.clone(), generation);

        let file_path = match uri.to_file_path() {
            Ok(p) => p,
            Err(_) => {
                self.client
                    .log_message(MessageType::ERROR, format!("Invalid file path: {}", uri))
                    .await;
                self.client.publish_diagnostics(uri, vec![], None).await;
                return;
            }
        };

        let detected_root = find_project_root(&file_path);
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "Typechecking {} from project root {}",
                    file_path.display(),
                    detected_root.display()
                ),
            )
            .await;

        let permit = match self.check_semaphore.acquire().await {
            Ok(permit) => permit,
            Err(_) => return,
        };

        if !self.check_is_current(&uri, generation) {
            return;
        }

        let check = tokio::task::spawn_blocking(move || {
            catch_unwind(AssertUnwindSafe(|| {
                typecheck_service::typecheck_file(&file_path, &detected_root)
            }))
        })
        .await;

        let report = match check {
            Ok(Ok(Ok(report))) => report,
            Ok(Ok(Err(message))) => {
                self.log_check_failure(&uri, &message).await;
                return;
            }
            Ok(Err(payload)) => {
                self.log_check_failure(&uri, &panic_message(payload)).await;
                return;
            }
            Err(error) => {
                self.log_check_failure(&uri, &error.to_string()).await;
                return;
            }
        };

        if !self.check_is_current(&uri, generation) {
            return;
        }

        for (file_uri, file_diagnostics, version) in
            self.apply_check_report(&uri, saved_version, report)
        {
            self.client
                .publish_diagnostics(file_uri, file_diagnostics, version)
                .await;
        }

        drop(permit);
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let Some(document) = self.document_map.get(&uri) else {
            return Ok(None);
        };
        let text = document.text.clone();
        drop(document);

        let tokens = match lex(&text) {
            Ok(tokens) => tokens,
            Err(error) => {
                eprintln!(
                    "Failed to lex document while calculating semantic tokens: {}: {}",
                    error.code(),
                    error.message()
                );
                return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                    result_id: None,
                    data: vec![],
                })));
            }
        };
        let mut semantic_tokens = Vec::new();
        let mut last_line = 0;
        let mut last_start = 0;

        for token in tokens {
            let token_type = match token.kind {
                TokenKind::Keyword(_)
                | TokenKind::Intrinsic(_)
                | TokenKind::CompilerIdentifier(_) => KEYWORD_IDX,

                TokenKind::IntLiteral(_) | TokenKind::FloatLiteral(_) => NUMBER_IDX,

                TokenKind::StringLiteral(_) => STRING_IDX,

                _ => continue,
            };

            let start = byte_index_to_position(&text, token.byte_start_index);
            let end = byte_index_to_position(&text, token.byte_end_index);
            if end.line != start.line {
                continue;
            }
            let line = start.line;
            let length = end.character.saturating_sub(start.character);

            let delta_line = line - last_line;
            let delta_start = if delta_line == 0 {
                start.character.saturating_sub(last_start)
            } else {
                start.character
            };

            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length: length.max(1),
                token_type,
                token_modifiers_bitset: 0,
            });

            last_line = line;
            last_start = start.character;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }
}

impl Backend {
    fn check_is_current(&self, uri: &Url, generation: u64) -> bool {
        self.latest_checks
            .get(uri)
            .is_some_and(|latest| *latest == generation)
    }

    async fn log_check_failure(&self, uri: &Url, message: &str) {
        self.client
            .log_message(
                MessageType::ERROR,
                format!("Typecheck failed for {uri}: {message}"),
            )
            .await;
    }

    fn apply_check_report(
        &self,
        entry_uri: &Url,
        entry_version: Option<i32>,
        report: typecheck_service::CheckReport,
    ) -> Vec<(Url, Vec<Diagnostic>, Option<i32>)> {
        let mut contributions = self
            .diagnostic_contributions
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let previous = contributions.remove(entry_uri).unwrap_or_default();
        let mut authoritative_files = report.checked_files;
        authoritative_files.extend(previous.keys().cloned());
        authoritative_files.extend(report.diagnostics.keys().cloned());

        let mut affected_files = previous.keys().cloned().collect::<HashSet<_>>();
        affected_files.extend(report.diagnostics.keys().cloned());
        affected_files.insert(entry_uri.clone());

        for diagnostics in contributions.values_mut() {
            for file_uri in &authoritative_files {
                if diagnostics.remove(file_uri).is_some() {
                    affected_files.insert(file_uri.clone());
                }
            }
        }

        contributions.insert(entry_uri.clone(), report.diagnostics);

        let mut publications = Vec::with_capacity(affected_files.len());
        for file_uri in affected_files {
            let mut merged = Vec::new();
            for diagnostics in contributions.values() {
                if let Some(file_diagnostics) = diagnostics.get(&file_uri) {
                    for diagnostic in file_diagnostics {
                        if !merged.contains(diagnostic) {
                            merged.push(diagnostic.clone());
                        }
                    }
                }
            }

            let version = (file_uri == *entry_uri).then_some(entry_version).flatten();
            publications.push((file_uri, merged, version));
        }

        publications
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: DashMap::new(),
        latest_checks: DashMap::new(),
        diagnostic_contributions: Mutex::new(HashMap::new()),
        next_generation: AtomicU64::new(1),
        check_semaphore: Semaphore::new(1),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
