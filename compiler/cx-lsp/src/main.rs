use cx_lexer::lex;
use cx_lexer_data::token::TokenKind;
use dashmap::DashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

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
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
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

            let line = token.line - 1; // LSP lines are 0-based
            let start = token.start_index as u32;
            let length = (token.end_index - token.start_index) as u32;

            let delta_line = line - last_line;
            let delta_start = if delta_line == 0 {
                start - last_start
            } else {
                start
            };

            semantic_tokens.push(SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type,
                token_modifiers_bitset: 0,
            });

            last_line = line;
            last_start = start;
        }

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        document_map: DashMap::new(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
