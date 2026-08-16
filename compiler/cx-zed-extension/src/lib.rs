use zed_extension_api::{self as zed, Command, LanguageServerId};

struct CXLSP;

fn lsp_binary_path() -> String {
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .join("..")
        .join("..")
        .join("target")
        .join("debug")
        .join("cx-lsp")
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
        _worktree: &zed::Worktree,
    ) -> zed::Result<Command> {
        Ok(Command {
            command: lsp_binary_path(),
            args: vec![],
            env: Default::default(),
        })
    }
}

zed::register_extension!(CXLSP);
