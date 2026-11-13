use zed_extension_api::{self as zed, Command, LanguageServerId};

struct CXLSP;

const LSP_BINARY: &str = "cx-lsp";

fn checkout_binary_path() -> String {
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .join("..")
        .join("..")
        .join("target")
        .join("debug")
        .join(LSP_BINARY)
        .to_string_lossy()
        .into_owned()
}

impl zed::Extension for CXLSP {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> zed::Result<Command> {
        Ok(Command {
            command: worktree
                .which(LSP_BINARY)
                .unwrap_or_else(checkout_binary_path),
            args: vec![],
            env: Default::default(),
        })
    }
}

zed::register_extension!(CXLSP);
