use cx_util::char_iter::CharIter;

pub(crate) fn handle_comment(iter: &mut CharIter) -> bool {
    assert_eq!(iter.peek(), Some('/'));
    iter.next();

    match iter.peek() {
        Some('/') => {
            iter.next();
            iter.skip_line();
            true
        }
        Some('*') => {
            iter.next();
            while iter.has_next() {
                if iter.peek() == Some('*') {
                    iter.next();
                    if iter.peek() == Some('/') {
                        iter.next();
                        break;
                    }
                } else {
                    iter.next();
                }
            }
            true
        }
        _ => {
            iter.back();
            false
        }
    }
}

pub(crate) fn skip_directive_tail(iter: &mut CharIter) {
    while let Some(c) = iter.peek() {
        if c == '\n' {
            iter.next();
            break;
        }

        if c == '/' {
            iter.next();
            match iter.peek() {
                Some('/') => {
                    iter.skip_line();
                    break;
                }
                Some('*') => {
                    iter.next();
                    while iter.has_next() {
                        if iter.peek() == Some('*') {
                            iter.next();
                            if iter.peek() == Some('/') {
                                iter.next();
                                break;
                            }
                        } else {
                            iter.next();
                        }
                    }
                }
                _ => {}
            }
        } else {
            iter.next();
        }
    }
}

pub(crate) fn strip_replacement_comments(input: &str) -> String {
    let mut output = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '/' {
            match chars.peek() {
                Some('/') => break,
                Some('*') => {
                    chars.next();
                    while let Some(c) = chars.next() {
                        if c == '*' && chars.peek() == Some(&'/') {
                            chars.next();
                            break;
                        }
                    }
                }
                _ => output.push(c),
            }
        } else {
            output.push(c);
        }
    }

    output
}
