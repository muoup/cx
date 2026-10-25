use std::iter;

use cx_hmir::body::HMIRLocalID;

use crate::value::{Bindings, HMIRValue};

#[derive(Debug)]
struct Scope<C> {
    parent: Option<usize>,
    bindings: Bindings,
    control: C,
}

#[derive(Debug)]
pub(crate) struct Scopes<C> {
    stack: Vec<Scope<C>>,
}

impl<C: Default> Default for Scopes<C> {
    fn default() -> Self {
        let mut scopes = Self { stack: Vec::new() };
        scopes.push(C::default(), None, Bindings::new());
        scopes
    }
}

impl<C> Scopes<C> {
    pub(crate) fn push(&mut self, control: C, parent: Option<usize>, bindings: Bindings) -> usize {
        let index = self.stack.len();
        assert!(parent.is_none_or(|parent| parent < index));
        self.stack.push(Scope {
            parent,
            bindings,
            control,
        });
        index
    }

    pub(crate) fn pop(&mut self) -> C {
        assert!(self.stack.len() > 1, "the root scope is never closed");
        self.stack.pop().expect("a scope is open").control
    }

    pub(crate) fn top(&self) -> usize {
        self.stack.len() - 1
    }

    pub(crate) fn len(&self) -> usize {
        self.stack.len()
    }

    pub(crate) fn control(&self, index: usize) -> &C {
        &self.stack[index].control
    }

    pub(crate) fn control_mut(&mut self, index: usize) -> &mut C {
        &mut self.stack[index].control
    }

    pub(crate) fn controls(&self) -> impl DoubleEndedIterator<Item = (usize, &C)> {
        self.stack
            .iter()
            .enumerate()
            .map(|(index, scope)| (index, &scope.control))
    }

    pub(crate) fn local(&self, local: HMIRLocalID) -> Option<&HMIRValue> {
        self.lexical().find_map(|scope| scope.bindings.get(&local))
    }

    pub(crate) fn bind(&mut self, local: HMIRLocalID, value: HMIRValue) {
        let top = self.top();
        self.stack[top].bindings.insert(local, value);
    }

    pub(crate) fn remove(&mut self, local: HMIRLocalID) -> Option<HMIRValue> {
        let mut next = Some(self.top());
        while let Some(index) = next {
            let scope = &mut self.stack[index];
            if let Some(value) = scope.bindings.remove(&local) {
                return Some(value);
            }
            next = scope.parent;
        }
        None
    }

    pub(crate) fn visible(&self) -> Bindings {
        let mut bindings = Bindings::new();
        for scope in self.lexical() {
            for (local, value) in &scope.bindings {
                bindings.entry(*local).or_insert_with(|| value.clone());
            }
        }
        bindings
    }

    fn lexical(&self) -> impl Iterator<Item = &Scope<C>> {
        let mut next = Some(self.top());
        iter::from_fn(move || {
            let scope = &self.stack[next?];
            next = scope.parent;
            Some(scope)
        })
    }
}
