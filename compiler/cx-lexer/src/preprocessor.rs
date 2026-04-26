pub mod conditionals;
pub mod expr;
pub mod includes;
pub mod define;

use cx_util::CXResult;

use crate::{context::LexingContext, lexer::
    scanner::LexTransition
, preprocessor::{conditionals::{handle_elif, handle_else, handle_endif, handle_error, handle_if, handle_ifdef}, define::{handle_define, handle_undef}, includes::{handle_include, handle_pragma}}};

pub(crate) struct Preprocessor;

impl Preprocessor {
    pub(crate) fn handle_directive(&self, context: &mut LexingContext) -> CXResult<LexTransition> {
        let directive_start = context.current_frame().cursor;
        let Some(directive) = context.current_frame_mut().next_word() else {
            let frame = context.current_frame();
            
            return log_lexer_error!(
                frame.file_path.as_path(),
                &frame.source,
                directive_start,
                frame.cursor,
                "Expected preprocessor directive"
            );
        };

        let mut directive = directive;
        if directive == "#" {
            let Some(name) = context.current_frame_mut().next_word() else {
                let frame = context.current_frame();
                
                return log_lexer_error!(
                    frame.file_path.as_path(),
                    &frame.source,
                    directive_start,
                    frame.cursor,
                    "Expected preprocessor directive after '#'"
                );
            };
            directive.push_str(&name);
        }
        let directive_end = context.current_frame().cursor;

        match directive.as_str() {
            "#include" => handle_include(context, directive_start, directive_end),
            "#define" => handle_define(context, directive_start, directive_end),
            "#undef" => handle_undef(context),
            "#ifdef" | "#ifndef" => {
                handle_ifdef(context, &directive, directive_start, directive_end)
            }
            "#if" => handle_if(context, directive_start),
            "#elif" => handle_elif(context, directive_start, directive_end),
            "#else" => handle_else(context, directive_start, directive_end),
            "#endif" => handle_endif(context, directive_start, directive_end),
            "#pragma" => handle_pragma(context),
            "#line" | "#warning" => {
                context.skip_tail();
                Ok(LexTransition::Continue)
            }
            "#error" => handle_error(context, directive_start, directive_end),
            dir => {
                if !context.current_frame().is_active() {
                    context.skip_tail();
                    return Ok(LexTransition::Continue);
                }

                let frame = context.current_frame();
                
                log_lexer_error!(
                    frame.file_path.as_path(),
                    &frame.source,
                    directive_start,
                    directive_end,
                    "Preprocessor directive '{}' is not yet implemented",
                    dir
                )
            }
        }
    }
}
