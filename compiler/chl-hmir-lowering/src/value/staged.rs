use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use chl_hmir::{HMIRUnitID, body::HMIRLocalID, expr::HMIRExprID};
use rustc_hash::FxHashMap;

use crate::value::{
    HMIRValue, HMIRValueKind,
    immediate::{HMIRImmValue, HMIRImmediate},
};

#[derive(Debug)]
pub(crate) struct Quote<V = HMIRValue> {
    source: HMIRUnitID,
    root: HMIRExprID,
    params: Vec<HMIRLocalID>,
    captures: FxHashMap<HMIRLocalID, V>,
}

impl<V> Quote<V> {
    pub(crate) fn new(
        source: HMIRUnitID,
        root: HMIRExprID,
        params: Vec<HMIRLocalID>,
        captures: FxHashMap<HMIRLocalID, V>,
    ) -> Self {
        Self {
            source,
            root,
            params,
            captures,
        }
    }

    pub(crate) fn source(&self) -> HMIRUnitID {
        self.source
    }

    pub(crate) fn root(&self) -> HMIRExprID {
        self.root
    }

    pub(crate) fn params(&self) -> &[HMIRLocalID] {
        &self.params
    }

    pub(crate) fn captures(&self) -> &FxHashMap<HMIRLocalID, V> {
        &self.captures
    }

    pub(crate) fn map_captures<U, E>(
        &self,
        mut map: impl FnMut(&V) -> Result<U, E>,
    ) -> Result<Quote<U>, E> {
        let captures = self
            .captures
            .iter()
            .map(|(&local, value)| Ok((local, map(value)?)))
            .collect::<Result<_, E>>()?;
        Ok(Quote::new(self.source, self.root, self.params.clone(), captures))
    }
}

impl Quote {
    pub(crate) fn into_kind(self) -> HMIRValueKind {
        match self.map_captures(|value| HMIRImmediate::try_from(value.clone())) {
            Ok(quote) => HMIRImmValue::Quote(QuoteRef::new(quote)).into(),
            Err(_) => HMIRValueKind::Quote(QuoteRef::new(self)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct QuoteRef<V>(Rc<Quote<V>>);

impl<V> QuoteRef<V> {
    pub(crate) fn new(quote: Quote<V>) -> Self {
        Self(Rc::new(quote))
    }

    pub(crate) fn get(&self) -> &Quote<V> {
        &self.0
    }
}

impl<V> PartialEq for QuoteRef<V> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<V> Eq for QuoteRef<V> {}

impl<V> Hash for QuoteRef<V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
