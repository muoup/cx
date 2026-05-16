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
