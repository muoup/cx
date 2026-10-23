use std::{
    fmt::{self, Display, Formatter},
    sync::atomic::{AtomicU32, Ordering},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRUnitID(u32);

#[derive(Debug, Default)]
pub struct HMIRUnitAllocator {
    next: AtomicU32,
}

impl HMIRUnitAllocator {
    pub fn allocate(&self) -> HMIRUnitID {
        let id = self
            .next
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |next| {
                next.checked_add(1)
            })
            .expect("HMIR unit IDs exhausted");
        HMIRUnitID(id)
    }
}

impl Display for HMIRUnitID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "unit{}", self.0)
    }
}
