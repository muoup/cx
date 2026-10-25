use cx_hmir::HMIRDefID;
use cx_util::identifier::CXIdent;

use crate::value::HMIRPlace;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Addressable {
    Place(HMIRPlace),
    Label { function: HMIRDefID, label: CXIdent },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRAddress {
    base: Addressable,
    offset: i64,
}

impl HMIRAddress {
    pub fn new(base: Addressable, offset: i64) -> Self {
        Self { base, offset }
    }

    pub fn place(place: HMIRPlace) -> Self {
        Self::new(Addressable::Place(place), 0)
    }

    pub fn base(&self) -> &Addressable {
        &self.base
    }

    pub fn offset(&self) -> i64 {
        self.offset
    }

    pub fn checked_add(&self, bytes: i64) -> Option<Self> {
        Some(Self::new(
            self.base.clone(),
            self.offset.checked_add(bytes)?,
        ))
    }

    pub fn checked_sub(&self, bytes: i64) -> Option<Self> {
        Some(Self::new(
            self.base.clone(),
            self.offset.checked_sub(bytes)?,
        ))
    }

    pub fn whole_place(&self) -> Option<HMIRPlace> {
        match self.base {
            Addressable::Place(place) if self.offset == 0 => Some(place),
            _ => None,
        }
    }
}
