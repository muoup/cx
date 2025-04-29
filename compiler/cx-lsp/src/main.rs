use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tower_lsp::lsp_types::{InitializeParams, InitializeResult, InitializedParams, MessageType};

#[derive(Debug)]
struct LSPBackend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for LSPBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult::default())
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Server initializing")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| LSPBackend { client });
    Server::new(
        stdin,
        stdout,
        socket
    ).serve(service).await;
}