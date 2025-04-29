use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::sync::Mutex;

pub enum MaybeResult<Result, Consumed, Error> {
    Consumed(Result),
    Unconsumed(Consumed),
    Error(Error)
}

#[derive(Debug, Clone)]
pub(crate) struct ScopedMap<T> {
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

pub fn dump_data(data: &impl std::fmt::Display) {
    dump_write(&format!("{}\n", data));
    dump_write("\n\n\n//////////////\n\n\n\n");
}

pub fn dump_all(data: Vec<impl std::fmt::Display>) {
    let data = data
        .into_iter()
        .map(|d| format!("{}\n", d))
        .collect::<Vec<String>>()
        .join("\n");

    dump_write(&data);
}

fn dump_write(str: &str) {
    const DUMP_PATH: &str = ".internal/compiler-dump.data";
    let mut dump_file =
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(DUMP_PATH)
            .expect("Failed to open dump file");

    dump_file
        .write_all(str.as_bytes())
        .expect("Failed to write to dump file");
}