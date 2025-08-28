use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct ScopedMap<T> {
    data: HashMap<String, T>,
    overwrites: Vec<Vec<(String, Option<T>)>>
}

#[derive(Debug, Clone)]
pub struct ScopedSet<T: Eq + Hash + Clone> {
    data: HashSet<T>,
    writes: Vec<Vec<T>>
}

impl<T> Default for ScopedMap<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Eq + Hash + Clone> Default for ScopedSet<T> {
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
        
        self.data.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.data.get(name)
    }
}

impl<T: Eq + Hash + Clone> ScopedSet<T> {
    pub fn new() -> Self {
        Self {
            data: HashSet::new(),
            writes: vec![]
        }
    }

    pub fn push_scope(&mut self) {
        self.writes.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        if self.writes.is_empty() {
            panic!("Scope table has uneven push/pop");
        }

        for name in self.writes.pop().unwrap() {
            self.data.remove(&name);
        }
    }

    pub fn insert(&mut self, name: T) {
        if !self.data.contains(&name) {
            self.data.insert(name.clone());
            
            self.writes
                .last_mut()
                .expect("Uneven push/pop in ScopedSet")
                .push(name.clone());
        }
    }
    
    pub fn contains(&self, name: &T) -> bool {
        self.data.contains(name)
    }
}