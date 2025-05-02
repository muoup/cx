use tokio::net::TcpListener;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, DidOpenTextDocumentParams, InitializeParams, InitializeResult, InitializedParams, MessageType, Position, Range};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct LSPBackend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for LSPBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        println!("[LSP] Initializing");
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: InitializedParams) {
        println!("[LSP] Initialized");
        self.client
            .log_message(MessageType::INFO, "Server initializing")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let diagnostic = Diagnostic {
            range: Range {
                start: Position::new(0, 0),
                end: Position::new(0, 1),
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("cx-lsp".to_string()),
            message: "This is a test error".to_string(),
            related_information: None,
            tags: None,
            data: None,
        };

        self.client
            .log_message(MessageType::INFO, format!("Publishing diagnostics for file: {}", params.text_document.uri))
            .await;
        self.client
            .publish_diagnostics(params.text_document.uri, vec![diagnostic], None)
            .await;
    }
}

#[tokio::main]
async fn main() {
    println!("[LSP] Starting LSP server");

    let listener = TcpListener::bind("127.0.0.1:9257").await.unwrap();

    println!("[LSP] TCP client created");

    let (stream, _) = listener.accept().await.unwrap();
    let (read, write) = tokio::io::split(stream);

    let (service, socket) = LspService::new(|client| LSPBackend { client });
    println!("[LSP] LSP service created");

    Server::new(
        read,
        write,
        socket
    ).serve(service).await;
}