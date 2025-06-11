pub struct CharIter<'a> {
    pub source: &'a str,
    pub current_iter: usize,
}

impl CharIter<'_> {
    pub fn next(&mut self) -> Option<char> {
        if let Some(c) = self.source.chars().nth(self.current_iter) {
            self.current_iter += 1;
            Some(c)
        } else {
            None
        }
    }
    pub fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current_iter)
    }

    pub fn back(&mut self) {
        self.current_iter -= 1;
    }

    pub fn has_next(&self) -> bool {
        self.source.chars().nth(self.current_iter).is_some()
    }
}