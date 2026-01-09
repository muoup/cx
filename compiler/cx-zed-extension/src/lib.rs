use zed_extension_api::{self as zed, Command, LanguageServerId};

struct CXLSP;

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
            command: "/home/user/workspace/Rust/cx/target/debug/cx-lsp".to_string(),
            args: vec![],
            env: Default::default(),
        })
    }
}

zed::register_extension!(CXLSP);