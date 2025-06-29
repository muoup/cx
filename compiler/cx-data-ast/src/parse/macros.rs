use crate::parse::parser::TokenIter;

pub fn token_string(toks: &TokenIter, start_index: usize, end_index: usize) -> String {
    let mut tokens = String::new();
    
    for tok in &toks.slice[start_index..end_index] {
        tokens.push_str(&format!("{} ", tok));
    }

    tokens.push('\n');

    tokens
}

pub fn error_pointer(toks: &TokenIter) -> String {
    let previous_tokens = toks.index.min(3);
    let next_tokens = (toks.slice.len() - toks.index).min(3);

    let mut error_tokens = String::new();

    for tok in &toks.slice[toks.index - previous_tokens.. toks.index] {
        error_tokens.push_str(&format!("{} ", tok));
    }

    let mut error_pointer = String::new();

    for _ in 0..error_tokens.len() {
        error_pointer.push_str(" ");
    }

    error_pointer.push_str("^ ");

    for tok in &toks.slice[toks.index..toks.index + next_tokens] {
        error_tokens.push_str(&format!("{} ", tok));
    }

    error_tokens.push('\n');
    error_tokens.push_str(&error_pointer);

    error_tokens
}


#[macro_export]
macro_rules! assert_token_matches {
    ($data:ident, $pattern:pat) => {
        let $pattern = &$data.toks.next()?.kind else {
            use cx_data_ast::parse::macros::error_pointer;
            use cx_util::log_error;
            
            $data.toks.back();
            eprintln!("{}", error_pointer(&($data).toks));
            log_error!("Expected token to match pattern: {:#?}\n Found: {}", stringify!($pattern), $data.toks.peek().unwrap());
        };
    }
}

#[macro_export]
macro_rules! try_token_matches {
    ($data:ident, $pattern:pat) => {
        let Some($pattern) = $data.toks.peek() else {
            return None;
        };
        $data.toks.next();
    }
}

#[macro_export]
macro_rules! try_next {
    ($data:ident, $pattern:pat) => {
        if let Some($pattern) = $data.toks.peek().map(|k| &k.kind) {
            $data.toks.next();
            true
        } else {
            false
        }
    }
}

#[macro_export]
macro_rules! next_kind {
    ($data:ident) => {
        $data.toks.next().cloned().map(|k| k.kind);
    }
}