use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ScopedMap<T> {
    data: HashMap<String, T>,
    overwrites: Vec<Vec<(String, T)>>
}

impl<T> ScopedMap<T> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            overwrites: vec![]
        }
    }

    pub fn push_scope(&mut self) {
        self.overwrites.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        if self.overwrites.is_empty() {
            panic!("Scope table has uneven push/pop");
        }

        for (name, value) in self.overwrites.pop().unwrap() {
            self.data.insert(name, value);
        }
    }

    pub fn insert(&mut self, name: String, value: T)
    where T: Clone {
        if let Some((name, old_value)) = self.data.get_key_value(&name) {
            if self.overwrites.is_empty() {
                panic!("Scope table has uneven push/pop");
            }

            self.overwrites
                .last_mut().unwrap()
                .push((name.clone(), old_value.clone()));
        }

        self.data.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.data.get(name)
    }
}
