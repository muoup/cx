use cx_util::CXResult;

use crate::{
    context::LexingContext,
    lexer::{scanner::LexTransition, source::SourceFrame},
    preprocessor::expr,
};

pub(crate) fn handle_ifdef(
    context: &mut LexingContext,
    directive: &str,
    directive_start: usize,
    directive_end: usize,
) -> CXResult<LexTransition> {
    context.current_frame_mut().skip_whitespace();
    let name_start = context.current_frame().cursor;
    let Some(name) = context.current_frame_mut().next_word() else {
        let frame = context.current_frame();

        return log_lexer_error!(
            frame.file_path.as_path(),
            &frame.source,
            directive_start,
            directive_end,
            "{} requires a macro name",
            directive
        );
    };

    let is_defined = context.macros.contains_key(&name);
    let condition = if directive == "#ifdef" {
        is_defined
    } else {
        !is_defined
    };

    context.push(condition);
    if context.current_frame().cursor == name_start {
        context.skip_tail();
    }

    Ok(LexTransition::Continue)
}

pub(crate) fn handle_if(
    context: &mut LexingContext,
    directive_start: usize,
) -> CXResult<LexTransition> {
    let condition = if context.current_frame().is_active() {
        let expression = rest_of_logical_directive(context.current_frame_mut());
        expr::eval(context, &expression, directive_start)?
    } else {
        context.skip_tail();
        false
    };
    context.push(condition);
    Ok(LexTransition::Continue)
}

pub(crate) fn handle_elif(
    context: &mut LexingContext,
    directive_start: usize,
    directive_end: usize,
) -> CXResult<LexTransition> {
    let parent_can_activate = match context.current_frame().conditionals.last() {
        Some(frame) if frame.else_seen => {
            let source = context.current_frame();

            return log_lexer_error!(
                source.file_path.as_path(),
                &source.source,
                directive_start,
                directive_end,
                "#elif after #else"
            );
        }
        Some(frame) => frame.parent_active && !frame.any_branch_taken,
        None => {
            let source = context.current_frame();

            return log_lexer_error!(
                source.file_path.as_path(),
                &source.source,
                directive_start,
                directive_end,
                "#elif without matching #if"
            );
        }
    };

    let condition = if parent_can_activate {
        let expression = rest_of_logical_directive(context.current_frame_mut());
        expr::eval(context, &expression, directive_start)?
    } else {
        context.skip_tail();
        false
    };

    let frame = context.current_frame_mut().conditionals.last_mut().unwrap();
    frame.branch_active = parent_can_activate && condition;
    frame.any_branch_taken |= frame.branch_active;
    Ok(LexTransition::Continue)
}

pub(crate) fn handle_else(
    context: &mut LexingContext,
    directive_start: usize,
    directive_end: usize,
) -> CXResult<LexTransition> {
    context.skip_tail();
    let Some(frame) = context.current_frame_mut().conditionals.last_mut() else {
        let source = context.current_frame();

        return log_lexer_error!(
            source.file_path.as_path(),
            &source.source,
            directive_start,
            directive_end,
            "#else without matching #if"
        );
    };

    if frame.else_seen {
        let source = context.current_frame();

        return log_lexer_error!(
            source.file_path.as_path(),
            &source.source,
            directive_start,
            directive_end,
            "Duplicate #else"
        );
    }

    frame.else_seen = true;
    frame.branch_active = frame.parent_active && !frame.any_branch_taken;
    frame.any_branch_taken |= frame.branch_active;
    Ok(LexTransition::Continue)
}

pub(crate) fn handle_endif(
    context: &mut LexingContext,
    directive_start: usize,
    directive_end: usize,
) -> CXResult<LexTransition> {
    context.skip_tail();

    if context.current_frame_mut().conditionals.pop().is_none() {
        let source = context.current_frame();

        return log_lexer_error!(
            source.file_path.as_path(),
            &source.source,
            directive_start,
            directive_end,
            "#endif without matching #if"
        );
    }
    Ok(LexTransition::Continue)
}

pub(crate) fn handle_error(
    context: &mut LexingContext,
    directive_start: usize,
    directive_end: usize,
) -> CXResult<LexTransition> {
    let message = rest_of_logical_directive(context.current_frame_mut());
    if context.current_frame().is_active() {
        let frame = context.current_frame();

        return log_lexer_error!(
            frame.file_path.as_path(),
            &frame.source,
            directive_start,
            directive_end,
            "#error{}{}",
            if message.trim().is_empty() { "" } else { ": " },
            message.trim()
        );
    }
    Ok(LexTransition::Continue)
}

pub(crate) fn rest_of_logical_directive(frame: &mut SourceFrame) -> String {
    let mut output = String::new();

    loop {
        let line = frame.rest_of_line();
        let continued = line.trim_end().ends_with('\\');

        if continued {
            let trimmed = line.trim_end();
            output.push_str(&trimmed[..trimmed.len() - 1]);
            output.push(' ');
            if frame.peek() == Some('\n') {
                frame.with_iter(|iter| {
                    iter.next();
                });
            }
        } else {
            output.push_str(&line);
            break;
        }
    }

    output
}

pub(crate) fn read_macro_head(frame: &mut SourceFrame) -> Option<(String, Option<Vec<String>>)> {
    let start = frame.cursor;
    frame.with_iter(|iter| {
        while let Some(c) = iter.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                iter.next();
            } else {
                break;
            }
        }
    });

    if start == frame.cursor {
        return None;
    }

    let name = frame.source[start..frame.cursor].to_string();

    if frame.peek() != Some('(') {
        return Some((name, None));
    }

    frame.with_iter(|iter| {
        iter.next();
    });
    let params_start = frame.cursor;
    while let Some(c) = frame.peek() {
        if c == ')' {
            let params_text = &frame.source[params_start..frame.cursor];
            let params = if params_text.trim().is_empty() {
                Vec::new()
            } else {
                params_text
                    .split(',')
                    .map(|param| param.trim().to_string())
                    .collect()
            };
            frame.with_iter(|iter| {
                iter.next();
            });
            return Some((name, Some(params)));
        }
        frame.with_iter(|iter| {
            iter.next();
        });
    }

    Some((name, Some(Vec::new())))
}
