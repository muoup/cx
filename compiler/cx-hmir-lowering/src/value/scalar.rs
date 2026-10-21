pub mod float;
pub mod int;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarFault {
    ZeroDivisor,
    Undefined,
}
