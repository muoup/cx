use speedy::{Readable, Writable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub struct FloatWrapper {
    bits: u64
}

impl From<f64> for FloatWrapper {
    fn from(value: f64) -> Self {
        FloatWrapper { bits: value.to_bits() }
    }
}

impl Into<f64> for FloatWrapper {
    fn into(self) -> f64 {
        f64::from_bits(self.bits)
    }
}

impl std::fmt::Display for FloatWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value: f64 = (*self).into();
        write!(f, "{}", value)
    }
}