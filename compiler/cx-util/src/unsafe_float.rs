use speedy::{Readable, Writable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, Hash, Readable, Writable)]
pub struct FloatWrapper {
    bits: u64
}

impl From<f64> for FloatWrapper {
    fn from(value: f64) -> Self {
        FloatWrapper { bits: value.to_bits() }
    }
}

impl Into<f64> for &FloatWrapper {
    fn into(self) -> f64 {
        f64::from_bits(self.bits)
    }
}

impl Into<f32> for &FloatWrapper {
    fn into(self) -> f32 {
        let value: f64 = self.into();
        value as f32
    }
}

impl std::fmt::Display for FloatWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value: f64 = self.into();
        write!(f, "{}", value)
    }
}

impl PartialOrd for FloatWrapper {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let left: f64 = self.into();
        let right: f64 = other.into();
        left.partial_cmp(&right)
    }
}