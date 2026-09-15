#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Fact<T> {
    #[default]
    Bottom,
    Known(T),
    Top,
}

impl<T: Clone + PartialEq> Fact<T> {
    pub fn merge(&mut self, incoming: &Self) -> bool {
        match (&*self, incoming) {
            (_, Self::Bottom) | (Self::Top, _) => false,
            (Self::Bottom, _) => {
                *self = incoming.clone();
                true
            }
            (Self::Known(left), Self::Known(right)) if left == right => false,
            _ => {
                *self = Self::Top;
                true
            }
        }
    }
}

pub trait State: Clone + Default + 'static {
    fn merge(&mut self, incoming: &Self) -> bool;
}

#[derive(Debug)]
pub struct Table<T> {
    entries: Vec<Fact<T>>,
}

impl<T: Clone> Clone for Table<T> {
    fn clone(&self) -> Self {
        Self {
            entries: self.entries.clone(),
        }
    }
    fn clone_from(&mut self, source: &Self) {
        self.entries.clone_from(&source.entries);
    }
}

impl<T> Default for Table<T> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<T: Clone + PartialEq + 'static> Table<T> {
    pub fn get(&self, index: usize) -> &Fact<T> {
        self.entries.get(index).unwrap_or(&Fact::Bottom)
    }

    pub fn insert(&mut self, index: usize, value: Fact<T>) {
        if index >= self.entries.len() {
            if matches!(value, Fact::Bottom) {
                return;
            }
            self.entries.resize(index + 1, Fact::Bottom);
        }
        self.entries[index] = value;
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &Fact<T>)> {
        self.entries
            .iter()
            .enumerate()
            .filter(|(_, value)| !matches!(value, Fact::Bottom))
    }

    pub fn invalidate(&mut self) {
        for value in &mut self.entries {
            if !matches!(value, Fact::Bottom) {
                *value = Fact::Top;
            }
        }
    }
}

impl<T: Clone + PartialEq + 'static> State for Table<T> {
    fn merge(&mut self, incoming: &Self) -> bool {
        self.entries
            .resize(self.entries.len().max(incoming.entries.len()), Fact::Bottom);
        let mut changed = false;
        for (target, source) in self.entries.iter_mut().zip(&incoming.entries) {
            changed |= target.merge(source);
        }
        changed
    }
}
