use chl_log::error::{context::CHLErrorContextTrait, message::CHLStdErrMessage, CHLError};

use crate::help::{self, Topic};

struct CommandContext {
    topic: Option<Topic>,
}

impl CHLErrorContextTrait for CommandContext {
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

pub(crate) fn error(message: impl Into<String>, topic: Option<Topic>) -> CHLError {
    CHLError::new(
        CHLStdErrMessage::error("", message),
        Box::new(CommandContext { topic }),
    )
}
