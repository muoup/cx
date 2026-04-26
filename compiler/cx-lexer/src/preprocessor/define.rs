use cx_util::CXResult;

use crate::{
    context::{LexingContext, Macro},
    lexer::{comments::strip_replacement_comments, scanner::{LexTransition, tokenize_text}}, preprocessor::conditionals::{read_macro_head, rest_of_logical_directive},
};

pub(crate) fn handle_define(
    context: &mut LexingContext,
    directive_start: usize,
    directive_end: usize,
) -> CXResult<LexTransition> {
    if !context.current_frame().is_active() {
        context.skip_tail();
        return Ok(LexTransition::Continue);
    }

    context.current_frame_mut().skip_whitespace();
    let Some((name, params)) = read_macro_head(context.current_frame_mut()) else {
        let frame = context.current_frame();
        return log_lexer_error!(
            frame.file_path.as_path(),
            &frame.source,
            directive_start,
            directive_end,
            "#define requires a macro name"
        );
    };

    let rest_of_line = rest_of_logical_directive(context.current_frame_mut());
    let replacement = strip_replacement_comments(&rest_of_line);
    let file_path = context.current_frame().file_path.clone();
    let tokens = tokenize_text(&replacement, file_path.as_path())?;

    let macro_ = if let Some(params) = params {
        Macro::Function {
            params: params.into_boxed_slice(),
            body: tokens.into_boxed_slice(),
        }
    } else {
        Macro::Object(tokens.into_boxed_slice())
    };

    context.macros.insert(name, macro_);
    Ok(LexTransition::Continue)
}

pub(crate) fn handle_undef(context: &mut LexingContext) -> CXResult<LexTransition> {
    if context.current_frame().is_active()
        && let Some(name) = context.current_frame_mut().next_word()
    {
        context.macros.remove(&name);
    }
    
    context.skip_tail();
    Ok(LexTransition::Continue)
}
