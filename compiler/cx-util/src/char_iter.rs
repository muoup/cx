pub struct CharIter<'a> {
    pub source: &'a str,
    pub current_iter: usize,
    pub line: u32,
}

impl CharIter<'_> {
    pub fn new(source: &str) -> CharIter {
        CharIter {
            source,
            current_iter: 0,
            line: 1,
        }
    }

    pub fn sub_iter<'a>(parent: &CharIter, start: usize, slice: &'a str) -> CharIter<'a> {
        CharIter {
            source: slice,

            current_iter: start,
            line: parent.line,
        }
    }

    pub fn next(&mut self) -> Option<char> {
        if self.peek() == Some('\n') {
            self.line += 1;
        }
        
        if let Some(c) = self.peek() {
            self.current_iter += 1;
            Some(c)
        } else {
            None
        }
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    pub fn skip_line(&mut self) {
        while let Some(c) = self.peek() {
            self.next();
            if c == '\n' {
                break;
            }
        }
    }

    pub fn next_word(&mut self) -> Option<&str> {
        self.skip_whitespace();
        let start = self.current_iter;

        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                break;
            } else {
                self.next();
            }
        }

        if start == self.current_iter {
            None
        } else {
            Some(&self.source[start..self.current_iter])
        }
    }

    pub fn rest_of_line(&mut self) -> &str {
        let start = self.current_iter;

        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            } else {
                self.next();
            }
        }

        &self.source[start..self.current_iter]
    }
    
    pub fn peek(&self) -> Option<char> {
        self.source.as_bytes().get(self.current_iter)
            .map(|&c| c as char)
    }

    pub fn back(&mut self) {
        self.current_iter -= 1;
    }

    pub fn has_next(&self) -> bool {
        self.source.len() > self.current_iter
    }
}