use std::{
    hash::{Hash, Hasher},
    rc::Rc,
};

use cx_hmir::{HMIRUnitID, body::HMIRLocalID, expr::HMIRExprID};
use rustc_hash::FxHashMap;

use crate::value::HMIRValue;

#[derive(Debug)]
pub(crate) struct Quote {
    source: HMIRUnitID,
    root: HMIRExprID,
    params: Vec<HMIRLocalID>,
    captures: FxHashMap<HMIRLocalID, HMIRValue>,
}

impl Quote {
    pub(crate) fn new(
        source: HMIRUnitID,
        root: HMIRExprID,
        params: Vec<HMIRLocalID>,
        captures: FxHashMap<HMIRLocalID, HMIRValue>,
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

    pub(crate) fn captures(&self) -> &FxHashMap<HMIRLocalID, HMIRValue> {
        &self.captures
    }
}

#[derive(Debug, Clone)]
pub struct QuoteRef(Rc<Quote>);

impl QuoteRef {
    pub(crate) fn new(quote: Quote) -> Self {
        Self(Rc::new(quote))
    }
    
    pub(crate) fn get(&self) -> &Quote {
        &self.0
    }
}

impl PartialEq for QuoteRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for QuoteRef {}

impl Hash for QuoteRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
