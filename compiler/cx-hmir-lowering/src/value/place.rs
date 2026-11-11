use std::fmt;

use cx_mir::MIRFunctionID;
use cx_util::{branded_id, dense_id};

dense_id!(HMIRPageID, "pg");
branded_id!(HMIRPlaceID, HMIRPlaceBrand, "p");

// The storage a place belongs to. With the top bit set it is a runtime function, which numbers its places as MIR does;
// otherwise it is a comptime page, whose ids are never reused so that a place outliving its page cannot reach another
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRPlaceBrand(u32);

impl HMIRPlaceBrand {
    const RUNTIME: u32 = 1 << 31;

    // The function of a context only evaluated for its checks, whose places never outlive it
    pub const DETACHED: Self = Self(u32::MAX);

    pub fn runtime(function: MIRFunctionID) -> Self {
        Self(Self::RUNTIME | Self::bounded(function.index(), Self::DETACHED.0 & !Self::RUNTIME))
    }

    pub fn comptime(page: HMIRPageID) -> Self {
        Self(Self::bounded(page.index(), Self::RUNTIME))
    }

    pub fn page(self) -> Option<HMIRPageID> {
        (self.0 & Self::RUNTIME == 0).then(|| HMIRPageID::new(self.0 as usize))
    }

    fn bounded(index: usize, limit: u32) -> u32 {
        u32::try_from(index)
            .ok()
            .filter(|index| *index < limit)
            .unwrap_or_else(|| panic!("place brand index {index} exceeds {limit}"))
    }
}

impl fmt::Display for HMIRPlaceBrand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.page() {
            Some(page) => write!(f, "{page}"),
            None if *self == Self::DETACHED => write!(f, "detached"),
            None => write!(f, "@f{}", self.0 & !Self::RUNTIME),
        }
    }
}
