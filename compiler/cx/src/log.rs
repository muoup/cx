use cx_log::error::{context::CXErrorContextTrait, message::CXStdErrMessage, CXError};

use crate::help::{self, Topic};

struct CommandContext {
    topic: Option<Topic>,
}

impl CXErrorContextTrait for CommandContext {
    fn dump(&self, output: &mut dyn std::io::Write) -> std::io::Result<()> {
        if let Some(topic) = self.topic {
            writeln!(output, "\nUsage: {}", help::usage(topic))?;
            writeln!(
                output,
                "\nFor more information, try '{} --help'.",
                help::name(topic)
            )?;
        }
        Ok(())
    }
}

pub(crate) fn error(message: impl Into<String>, topic: Option<Topic>) -> CXError {
    CXError::new(
        CXStdErrMessage::error("", message),
        Box::new(CommandContext { topic }),
    )
}
