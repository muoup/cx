use cx_lexer::lex;
use cx_tokens::token::TokenKind;
use dashmap::DashMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod typecheck_service;

/// Find the project root by searching for .internal or .git directories
/// in parent directories of the given file path.
fn find_project_root(file_path: &Path) -> PathBuf {
    let mut current = file_path.parent().unwrap_or(file_path);

    loop {
        // Check for .internal directory
        let internal_dir = current.join(".internal");
        if internal_dir.is_dir() {
            return current.to_path_buf();
        }

        // Check for .git directory
        let git_dir = current.join(".git");
        if git_dir.is_dir() {
            return current.to_path_buf();
        }

        // Move to parent
        match current.parent() {
            Some(parent) => {
                if parent == current {
                    // Reached root, return file's directory
                    return file_path
                        .parent()
                        .unwrap_or_else(|| Path::new("/"))
                        .to_path_buf();
                }
                current = parent;
            }
            None => {
                return file_path
                    .parent()
                    .unwrap_or_else(|| Path::new("/"))
                    .to_path_buf();
            }
        }
    }
}

const KEYWORD_IDX : u32 = 0;
const _OPERATOR_IDX : u32 = 1;
const STRING_IDX : u32 = 2;
const NUMBER_IDX : u32 = 3;
const _TYPE_IDX : u32 = 4;
const _VARIABLE_IDX : u32 = 5;
const _FUNCTION_IDX : u32 = 6;
const _COMMENT_IDX : u32 = 7;
const _MACRO_IDX : u32 = 8;

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
    document_map: DashMap<Url, String>,
    published_diagnostic_files: Mutex<HashSet<Url>>,
    project_root: Arc<Mutex<PathBuf>>,
}

fn byte_index_to_lsp_position(text: &str, index: usize) -> Position {
    let mut remaining = index.min(text.len());

    for (line_num, line) in text.lines().enumerate() {
        let line_len = line.len();
        if remaining <= line_len {
            return Position {
                line: line_num as u32,
                character: line[..remaining].chars().count() as u32,
            };
        }

        remaining = remaining.saturating_sub(line_len + 1);
    }

    Position {
        line: text.lines().count().saturating_sub(1) as u32,
        character: text
            .lines()
            .last()
            .map(|line| line.chars().count() as u32)
            .unwrap_or(0),
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Try to get project root from workspace folder
        if let Some(root_uri) = &params.root_uri {
            if let Ok(root_path) = root_uri.to_file_path() {
                *self.project_root.lock().unwrap() = root_path;
            }
        }

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "cx-lsp".to_string(),
                version: Some("0.0.1".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: Some(false),
                        will_save_wait_until: Some(false),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions::default())),
                    }
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
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.document_map
            .insert(params.text_document.uri.clone(), params.text_document.text);
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.document_map.insert(
            params.text_document.uri,
            params.content_changes.remove(0).text,
        );
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        // Convert URL to file path - must be valid to continue
        let file_path = match uri.to_file_path() {
            Ok(p) => p,
            Err(_) => {
                self.client
                    .log_message(MessageType::ERROR, format!("Invalid file path: {}", uri))
                    .await;
                self.client
                    .publish_diagnostics(uri, vec![], None)
                    .await;
                return;
            }
        };

        // Detect project root from file location
        let detected_root = find_project_root(&file_path);
        *self.project_root.lock().unwrap() = detected_root.clone();

        // Log for debugging
        self.client
            .log_message(MessageType::INFO, format!("File: {:?}, Project root: {:?}", file_path, detected_root))
            .await;

        // Perform all synchronous operations before any await
        let diagnostics_by_file = {
            let project_root_guard = self.project_root.lock().unwrap();
            self.typecheck_file_sync(&file_path, &*project_root_guard)
        };

        self.client
            .log_message(MessageType::INFO, format!("Typechecking file: {}", uri))
            .await;

        // Publish diagnostics for each file
        for (file_uri, file_diagnostics) in &diagnostics_by_file {
            self.client
                .publish_diagnostics(file_uri.clone(), file_diagnostics.clone(), None)
                .await;
        }

        let current_files = diagnostics_by_file
            .keys()
            .cloned()
            .collect::<HashSet<_>>();
        let stale_files = {
            let mut published = self
                .published_diagnostic_files
                .lock()
                .expect("published diagnostics mutex poisoned");
            let stale = published
                .difference(&current_files)
                .cloned()
                .collect::<Vec<_>>();
            *published = current_files;
            stale
        };

        for stale_uri in stale_files {
            self.client
                .publish_diagnostics(stale_uri, vec![], None)
                .await;
        }

        if !diagnostics_by_file.contains_key(&uri) {
            self.client.publish_diagnostics(uri, vec![], None).await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let Some(text) = self.document_map.get(&uri) else {
            return Ok(None);
        };
        let Some(tokens) = lex(&text) else {
            return Ok(None);
        };

        let mut semantic_tokens = Vec::new();
        let mut last_line = 0;
        let mut last_start = 0;

        for token in tokens {
            let token_type = match token.kind {
                TokenKind::Keyword(_) |
                TokenKind::Intrinsic(_) => KEYWORD_IDX,
                
                TokenKind::IntLiteral(_) |
                TokenKind::FloatLiteral(_) => NUMBER_IDX,
                
                TokenKind::StringLiteral(_) => STRING_IDX,
                
                _ => continue,
            };

            let start = byte_index_to_lsp_position(&text, token.start_index);
            let end = byte_index_to_lsp_position(&text, token.end_index);
            let line = start.line;
            let length = if end.line == start.line {
                end.character.saturating_sub(start.character)
            } else {
                text[token.start_index..token.end_index].chars().count() as u32
            };

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
    fn typecheck_file_sync(
        &self,
        file_path: &Path,
        project_root: &Path,
    ) -> HashMap<Url, Vec<Diagnostic>> {
        let unit_identifier = file_path
            .strip_prefix(project_root)
            .unwrap_or(file_path)
            .to_string_lossy()
            .to_string();

        let unit = cx_pipeline_data::CompilationUnit::from_rooted(&unit_identifier, project_root);
        let internal_directory = project_root.join(".internal").join("zed-lsp");

        // Create fresh compilation context for each typecheck
        let context = cx_pipeline_data::GlobalCompilationContext {
            config: cx_pipeline_data::CompilerConfig {
                backend: cx_pipeline_data::CompilerBackend::Cranelift,
                optimization_level: cx_pipeline_data::OptimizationLevel::O0,
                output: project_root.join("zed-lsp-output"),
                analysis: false,
                working_directory: project_root.to_path_buf(),
                internal_directory,
            },
            module_db: cx_pipeline_data::db::ModuleData::new(),
            linking_files: Mutex::new(HashSet::new()),
        };

        // Run typecheck-only pipeline
        let type_errors = cx_pipeline::typecheck_only_lsp(&context, &unit);

        // Group diagnostics by file
        typecheck_service::group_diagnostics_by_file(&type_errors)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let project_root = Arc::new(Mutex::new(std::env::current_dir().unwrap()));

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: DashMap::new(),
        published_diagnostic_files: Mutex::new(HashSet::new()),
        project_root,
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
