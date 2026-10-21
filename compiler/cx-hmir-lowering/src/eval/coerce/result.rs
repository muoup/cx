use cx_log::CXResult;

use crate::value::HMIRValue;

pub enum Coercion {
    Applied(HMIRValue),
    Unapplied(HMIRValue),
}

impl Coercion {
    pub fn map(self, f: impl FnOnce(HMIRValue) -> HMIRValue) -> Self {
        match self {
            Coercion::Applied(value) => Coercion::Applied(f(value)),
            unapplied => unapplied,
        }
    }

    pub fn or_else(self, f: impl FnOnce(HMIRValue) -> CXResult<Self>) -> CXResult<Self> {
        match self {
            Coercion::Unapplied(value) => f(value),
            applied => Ok(applied),
        }
    }
}
