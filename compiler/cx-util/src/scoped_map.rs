use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ScopedMap<T> {
    data: HashMap<String, T>,
    overwrites: Vec<Vec<(String, Option<T>)>>
}

impl<T> Default for ScopedMap<T> {
    fn default() -> Self {
        Self::new()
    }
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

    pub fn pop_scope(&mut self) -> Box<[(String, T)]> {
        if self.overwrites.is_empty() {
            panic!("Scope table has uneven push/pop");
        }
        
        let mut acc = vec![];
        
        for (name, value) in self.overwrites.pop().unwrap().into_iter().rev() {
            let Some((name2, val)) = self.data.remove_entry(&name) else {
                panic!("Improper overwrite in scope table");
            };
            
            acc.push((name2, val));
            
            if let Some(val) = value {
                self.data.insert(name, val);
            }
        }
     
        acc.into_boxed_slice()
    }

    pub fn insert(&mut self, name: String, value: T) {
        if self.overwrites.is_empty() {
            panic!("Scope table has uneven push/pop");
        }
        
        let val = self.data.remove_entry(&name)
            .map(|(name, old_value)| (name, Some(old_value)))
            .unwrap_or((name.clone(), None));
        
        self.data.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.data.get(name)
    }
}
